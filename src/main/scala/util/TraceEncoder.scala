// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._
import scala.math.min

class TraceEncoderParams(
  val coreParams: TraceCoreParams,
  val bufferDepth: Int
)

class TraceEncoderControlInterface() extends Bundle {
  val enable = Bool()
}

object FullHeaderType extends ChiselEnum {
  val FTakenBranch    = Value(0x0.U) // 000
  val FNotTakenBranch = Value(0x1.U) // 001
  val FUninfJump      = Value(0x2.U) // 010
  val FinfJump        = Value(0x3.U) // 011
  val FTrap           = Value(0x4.U) // 100
  val FSync           = Value(0x5.U) // 101
  val FValue          = Value(0x6.U) // 110
  val FReserved       = Value(0x7.U) // 111

  def encoderFullHeader(header_type: FullHeaderType.Type): UInt = {
    Cat(
      0.U(3.W),                       // 0b000
      header_type.asUInt,    // 0bxxx
      CompressedHeaderType.CNA.asUInt // 0b10 
    )
  }
}

object CompressedHeaderType extends ChiselEnum {
  val CTB = Value(0x0.U) // 00, taken branch
  val CNT = Value(0x1.U) // 01, not taken branch
  val CNA = Value(0x2.U) // 10, not a compressed packet
  val CIJ = Value(0x3.U) // 11, is a jump
}

// Variable-length encoding helper module
class VarLenEncoder(val maxWidth: Int) extends Module {
  val maxNumBytes = maxWidth/(8-1) + 1
  // println(s"maxNumBytes: $maxNumBytes")
  val io = IO(new Bundle {
    val input_value = Input(UInt(maxWidth.W))
    val output_num_bytes = Output(UInt(log2Ceil(maxNumBytes).W))
    val output_bytes = Output(Vec(maxNumBytes, UInt(8.W)))
  })

  // 0-indexed MSB index 
  val msb_index = (maxWidth - 1).U - PriorityEncoder(Reverse(io.input_value))
  io.output_num_bytes := (msb_index / 7.U) + 1.U 
  
  for (i <- 0 until maxNumBytes) {
    val is_last_byte = (i.U === (io.output_num_bytes - 1.U))
    io.output_bytes(i) := Mux(i.U < io.output_num_bytes,
      io.input_value(min(i*7+6, maxWidth-1), i*7) | Mux(is_last_byte, 0x80.U, 0.U),
      0.U
    )
  }
}

// slice packets into bytes TODO: is this efficient?
class TracePacketizer(val params: TraceEncoderParams) extends Module {
  def getMaxNumBytes(width: Int): Int = { width/(8-1) + 1 }
  val addrMaxNumBytes = getMaxNumBytes(params.coreParams.iaddrWidth)
  val timeMaxNumBytes = getMaxNumBytes(params.coreParams.xlen)
  // println(s"addrMaxNumBytes: $addrMaxNumBytes, timeMaxNumBytes: $timeMaxNumBytes")
  val metaDataWidth = log2Ceil(addrMaxNumBytes) + log2Ceil(timeMaxNumBytes) + 1
  val io = IO(new Bundle {
    val addr = Flipped(Decoupled(Vec(addrMaxNumBytes, UInt(8.W))))
    val time = Flipped(Decoupled(Vec(timeMaxNumBytes, UInt(8.W))))
    val byte = Flipped(Decoupled(UInt(8.W)))
    val metadata = Flipped(Decoupled(UInt(metaDataWidth.W)))
    val out = Decoupled(UInt(8.W))
  })

  val pIdle :: pComp :: pFull :: Nil = Enum(3)
  val state = RegInit(pIdle)

  val is_compressed = io.metadata.bits(0)

  val addr_num_bytes = Reg(UInt(log2Ceil(addrMaxNumBytes).W))
  val addr_index = Reg(UInt(log2Ceil(addrMaxNumBytes).W))
  val time_num_bytes = Reg(UInt(log2Ceil(timeMaxNumBytes).W))
  val time_index = Reg(UInt(log2Ceil(timeMaxNumBytes).W))
  val header_num_bytes = Reg(UInt(1.W))
  val header_index = Reg(UInt(1.W))
  
  // default values
  io.out.valid := false.B
  io.metadata.ready := false.B
  io.addr.ready := false.B
  io.time.ready := false.B
  io.byte.ready := false.B
  io.out.bits := 0.U
  
  switch (state) {
    is (pIdle) {
      io.metadata.ready := true.B
      when (io.metadata.fire) {
        addr_num_bytes := io.metadata.bits(metaDataWidth-1, log2Ceil(timeMaxNumBytes)+1)
        addr_index := 0.U
        time_num_bytes := io.metadata.bits(log2Ceil(timeMaxNumBytes), 1)
        time_index := 0.U
        header_num_bytes := is_compressed
        header_index := 0.U
        state := Mux(is_compressed, pComp, pFull)
      }
    }
    is (pComp) {
      // transmit a byte from byte buffer
      io.byte.ready := io.out.ready
      io.out.valid := io.byte.valid
      io.out.bits := io.byte.bits
      when (io.byte.fire) {
        // metadata runs ahead by 1 cycle for performance optimization
        io.metadata.ready := true.B
        state := Mux(io.metadata.fire, 
          Mux(is_compressed, pComp, pFull),
          pIdle
        )
      }
    }
    is (pFull) {
      // header, addr, time
      io.out.valid := true.B
      when (header_num_bytes > 0.U && header_index < header_num_bytes) {
        io.out.bits := io.byte.bits
        header_index := header_index + io.out.fire
      } .elsewhen (addr_num_bytes > 0.U && addr_index < addr_num_bytes) {
        io.out.bits := io.addr.bits(addr_index)
        addr_index := addr_index + io.out.fire
      } .elsewhen (time_num_bytes > 0.U && time_index < time_num_bytes) {
        io.out.bits := io.time.bits(time_index)
        time_index := time_index + io.out.fire
      } .otherwise {
        io.out.valid := false.B
        io.byte.ready := true.B
        io.addr.ready := true.B
        io.time.ready := true.B
        io.metadata.ready := true.B
        state := Mux(io.metadata.fire, 
          Mux(is_compressed, pComp, pFull),
          pIdle
        )
      }
    }
  }
}

class TraceEncoder(val params: TraceEncoderParams) extends Module {
  val io = IO(new Bundle {
    val control = new TraceEncoderControlInterface()
    val in = Input(new TraceCoreInterface(params.coreParams))
    val stall = Output(Bool())
    val out = Decoupled(UInt(8.W))
  })

  val MAX_DELTA_TIME_COMP = 0xCF // 63, 6 bits

  // states
  val sIdle :: sSync :: sData :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val enabled = RegInit(false.B)
  val stall = Wire(Bool())
  val prev_time = Reg(UInt(params.coreParams.xlen.W))

  // pipeline of ingress data
  val ingress_0 = RegInit(Wire(new TraceCoreInterface(params.coreParams)))
  val ingress_1 = RegInit(Wire(new TraceCoreInterface(params.coreParams)))

  // shift every cycle, if not stalled
  when (!stall) {
    ingress_0 := io.in
    ingress_1 := ingress_0
  }

  // encoders
  val addr_encoder = Module(new VarLenEncoder(params.coreParams.iaddrWidth))
  val time_encoder = Module(new VarLenEncoder(params.coreParams.xlen))
  val metadataWidth = log2Ceil(addr_encoder.maxNumBytes) + log2Ceil(time_encoder.maxNumBytes) + 1

  // queue buffers
  val addr_buffer = Module(new Queue(Vec(addr_encoder.maxNumBytes, UInt(8.W)), params.bufferDepth))
  val time_buffer = Module(new Queue(Vec(time_encoder.maxNumBytes, UInt(8.W)), params.bufferDepth))
  val byte_buffer = Module(new Queue(UInt(8.W), params.bufferDepth)) // buffer compressed packet or full header
  val metadata_buffer = Module(new Queue(UInt(metadataWidth.W), params.bufferDepth))
  // intermediate varlen encoder signals
  val full_addr      = Wire(Vec(addr_encoder.maxNumBytes, UInt(8.W)))
  val addr_num_bytes = Wire(UInt(log2Ceil(addr_encoder.maxNumBytes).W))
  val full_time      = Wire(Vec(time_encoder.maxNumBytes, UInt(8.W)))
  val time_num_bytes = Wire(UInt(log2Ceil(time_encoder.maxNumBytes).W))
  full_addr := addr_encoder.io.output_bytes
  addr_num_bytes := addr_encoder.io.output_num_bytes
  full_time := time_encoder.io.output_bytes
  time_num_bytes := time_encoder.io.output_num_bytes

  // intermediate packet signals
  val is_compressed = Wire(Bool())
  val delta_time = ingress_1.time - prev_time
  val packet_valid = Wire(Bool())
  val full_header   = Wire(UInt(8.W)) // full header
  val comp_packet   = Wire(UInt(8.W)) // compressed packet

  // packetization of buffered message
  val trace_packetizer = Module(new TracePacketizer(params))
  trace_packetizer.io.addr <> addr_buffer.io.deq
  trace_packetizer.io.time <> time_buffer.io.deq
  trace_packetizer.io.byte <> byte_buffer.io.deq
  trace_packetizer.io.metadata <> metadata_buffer.io.deq
  trace_packetizer.io.out <> io.out

  // metadata buffering
  val metadata = Cat(addr_num_bytes, time_num_bytes, is_compressed)
  metadata_buffer.io.enq.bits := metadata
  metadata_buffer.io.enq.valid := packet_valid
  // buffering compressed packet or full header depending on is_compressed
  byte_buffer.io.enq.bits := Mux(is_compressed, comp_packet, full_header)
  byte_buffer.io.enq.valid := packet_valid
  // address buffering
  addr_buffer.io.enq.bits := full_addr
  addr_buffer.io.enq.valid := !is_compressed && packet_valid
  // time buffering
  time_buffer.io.enq.bits := full_time
  time_buffer.io.enq.valid := !is_compressed && packet_valid

  // stall if any buffer is full
  stall := !addr_buffer.io.enq.ready || !time_buffer.io.enq.ready || !byte_buffer.io.enq.ready

  // state machine
  switch (state) {
    is (sIdle) {
      when (io.control.enable) { state := sSync }
    }
    is (sSync) {
      full_header := FullHeaderType.encoderFullHeader(FullHeaderType.FTakenBranch)
      time_encoder.io.input_value := ingress_1.time
      prev_time := ingress_1.time
      addr_encoder.io.input_value := ingress_1.group(0).iaddr >> 1.U // last bit is always 0
      is_compressed := false.B
      packet_valid := true.B
      // state transition: wait for message to go in
      state := Mux(!stall, sData, sSync)
    }
    is (sData) {
      switch (ingress_1.group(0).itype) {
        is (TraceItype.ITNothing) {
          packet_valid := false.B
        }
        is (TraceItype.ITBrTaken) {
          full_header := FullHeaderType.encoderFullHeader(FullHeaderType.FNotTakenBranch)
          comp_packet := Cat(delta_time(5, 0), CompressedHeaderType.CTB.asUInt)
          time_encoder.io.input_value := delta_time
          prev_time := ingress_1.time
          is_compressed := delta_time <= MAX_DELTA_TIME_COMP.U
          packet_valid := true.B
        }
        is (TraceItype.ITBrNTaken) {
          full_header := FullHeaderType.encoderFullHeader(FullHeaderType.FNotTakenBranch)
          comp_packet := Cat(delta_time(5, 0), CompressedHeaderType.CNT.asUInt)
          time_encoder.io.input_value := delta_time
          prev_time := ingress_1.time
          is_compressed := delta_time <= MAX_DELTA_TIME_COMP.U
          packet_valid := true.B
        }
        is (TraceItype.ITInJump) {
          full_header := FullHeaderType.encoderFullHeader(FullHeaderType.FUninfJump)
          comp_packet := Cat(delta_time(5, 0), CompressedHeaderType.CIJ.asUInt)
          time_encoder.io.input_value := delta_time
          prev_time := ingress_1.time
          is_compressed := delta_time <= MAX_DELTA_TIME_COMP.U
          packet_valid := true.B
        }
        is (TraceItype.ITUnJump) {
          full_header := FullHeaderType.encoderFullHeader(FullHeaderType.FUninfJump)
          time_encoder.io.input_value := delta_time 
          prev_time := ingress_1.time
          addr_encoder.io.input_value := (ingress_1.group(0).iaddr ^ ingress_0.group(0).iaddr) >> 1.U
          is_compressed := false.B
          packet_valid := true.B
        }
      }
    }
  }
}


package freechips.rocketchip.util

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.prci._
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}
trait HasTraceSinkIO {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
  })
}

class TraceSinkPrint()(implicit p: Parameters) extends LazyModule {
  override lazy val module = new TraceSinkPrintImpl(this)
  class TraceSinkPrintImpl(outer: TraceSinkPrint) extends LazyModuleImp(outer) with HasTraceSinkIO {
    withClockAndReset(clock, reset) {
      io.in.ready := true.B
      val byte_printer = Module(new BytePrinter())
      byte_printer.io.clk := clock
      byte_printer.io.reset := reset
      byte_printer.io.in_valid := io.in.valid
      byte_printer.io.in_byte := io.in.bits
    }
  }
}

class BytePrinter() extends BlackBox  with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset = Input(Reset())
    val in_valid = Input(Bool())
    val in_byte = Input(UInt(8.W))
  })
  addResource("/vsrc/BytePrinter.v")
}

class TraceSinkDMA(addr: BigInt, beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLClientParameters(
    name = "trace-sink-dma", sourceId = IdRange(0, 1))))))

  val device = new SimpleDevice("trace-sink-dma", Seq("ucbbar,trace0"))
  val regnode = TLRegisterNode(
    address = Seq(AddressSet(addr, 0xFF)),
    device = device,
    beatBytes = beatBytes
  )

  lazy val module = new TraceSinkDMAImpl(this)
  class TraceSinkDMAImpl(outer: TraceSinkDMA) extends LazyModuleImp(outer) with HasTraceSinkIO {
    val fifo = Module(new Queue(UInt(8.W), 32))
    fifo.io.enq <> io.in
    val (mem, edge) = outer.node.out(0)
    val addrBits = edge.bundle.addressBits
    val busWidth = edge.bundle.dataBits
    val blockBytes = p(CacheBlockBytes)
    
    val mIdle :: mCollect :: mWrite :: mResp :: Nil = Enum(4)
    val mstate = RegInit(mIdle)
    
    // tracks how much trace data have we written in total
    val addr_counter = RegInit(0.U(64.W))
    // tracks how much trace data have we collected in current transaction
    val collect_counter = RegInit(0.U(4.W))
    val msg_buffer = RegInit(VecInit(Seq.fill(busWidth / 8)(0.U(8.W))))

    val dma_start_addr = RegInit(0.U(64.W))
    val dma_addr_write_valid = Wire(Bool())

    val flush_reg = RegInit(false.B)
    val done_reg = RegInit(false.B)
    val collect_full = collect_counter === (busWidth / 8).U
    val collect_advance = Mux(flush_reg, collect_full || fifo.io.deq.valid === false.B, collect_full)
    val flush_done = (flush_reg) && (fifo.io.deq.valid === false.B)
    done_reg := done_reg || flush_done
    
    mem.a.valid := mstate === mWrite
    mem.d.ready := mstate === mResp
    fifo.io.deq.ready := false.B // default case 
    dontTouch(mem.d.valid)

    // mask according to collect_counter
    val mask = (1.U << collect_counter) - 1.U
    // putting the buffer data on the TL mem lane

    val put_req = edge.Put(
      fromSource = 0.U,
      toAddress = addr_counter + dma_start_addr,
      lgSize = log2Ceil(busWidth / 8).U,
      data = Cat(msg_buffer.reverse),
      mask = mask)._2

    mem.a.bits := put_req

    switch(mstate) {
      is (mIdle) {
        fifo.io.deq.ready := false.B
        mstate := Mux(fifo.io.deq.valid, mCollect, mIdle)
        collect_counter := 0.U
      }
      is (mCollect) {
        // either we have collected enough data or that's all the messages for now
        mstate := Mux(collect_advance, mWrite, mCollect)
        collect_counter := Mux(fifo.io.deq.fire, collect_counter + 1.U, collect_counter)
        msg_buffer(collect_counter) := Mux(fifo.io.deq.fire, fifo.io.deq.bits, msg_buffer(collect_counter))
        fifo.io.deq.ready := collect_counter < (busWidth / 8).U
      }
      // potentially, optimize this by pipelining collect and write
      is (mWrite) {
        // we need to write the collected data to the memory
        fifo.io.deq.ready := false.B
        mstate := Mux(mem.a.fire, mResp, mWrite)
      }
      is (mResp) {
        fifo.io.deq.ready := false.B
        mstate := Mux(mem.d.fire, mIdle, mResp)
        addr_counter := Mux(mem.d.fire, addr_counter + collect_counter, addr_counter)
      }
    }

    // regmap handler functions
    def traceSinkDMARegWrite(valid: Bool, bits: UInt): Bool = {
      dma_addr_write_valid := valid && mstate === mIdle
      when (dma_addr_write_valid) {
        dma_start_addr := bits
        printf("Writing to trace sink DMA reg from %x to %x\n",
         dma_start_addr, bits)
      }
      true.B
    }

    def traceSinkDMARegRead(ready: Bool): (Bool, UInt) = {
      (true.B, dma_start_addr)
    }

    val regmap = regnode.regmap(
      Seq(
        0x00 -> Seq(
          RegField(1, flush_reg,
            RegFieldDesc("flush_reg", "Flush register"))
        ),
        0x04 -> Seq(
          RegField.r(1, done_reg,
            RegFieldDesc("done_reg", "Done register"))
        ),
        0x08 -> Seq(
          RegField(64, traceSinkDMARegRead(_), traceSinkDMARegWrite(_, _),
            RegFieldDesc("dma_start_addr", "DMA start address"))
        ),
        0x10 -> Seq(RegField(64, addr_counter,
            RegFieldDesc("addr_counter", "Address counter"))
        )
      ):_*
    )
  }
}

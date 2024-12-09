// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.diplomacy.{AddressSet}
import freechips.rocketchip.resources.{SimpleDevice}
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}

class TraceEncoderControlInterface() extends Bundle {
  val enable = Bool()
  val target = UInt(TraceSinkTarget.getWidth.W)
}
class TraceEncoderController(addr: BigInt, beatBytes: Int)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("trace-encoder-controller", Seq("ucbbar,trace0"))
  val node = TLRegisterNode(
    address = Seq(AddressSet(addr, 0xFF)),
    device = device,
    beatBytes = beatBytes
  )
  
  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val control = Output(new TraceEncoderControlInterface())
    })

    val control_reg_write_valid = Wire(Bool())
    val control_reg_bits = RegInit(1.U(2.W))
    val enable = control_reg_bits(1)
    val active = control_reg_bits(0)
    io.control.enable := enable

    val trace_sink_target = RegInit(TraceSinkTarget.STPrint.asUInt)
    io.control.target := trace_sink_target.asUInt

    def traceEncoderControlRegWrite(valid: Bool, bits: UInt): Bool = {
      control_reg_write_valid := valid
      when (control_reg_write_valid) {
        control_reg_bits := bits
        printf("Writing to trace encoder control reg from %x to %x\n",
         control_reg_bits, bits)
      }
      true.B
    }

    def traceEncoderControlRegRead(ready: Bool): (Bool, UInt) = {
      (true.B, control_reg_bits)
    }

    val regmap = node.regmap(
      Seq(
        0x00 -> Seq(
          RegField(2, traceEncoderControlRegRead(_), traceEncoderControlRegWrite(_, _),
            RegFieldDesc("control", "Control trace encoder"))
        ),
        0x04 -> Seq(
          RegField(1, trace_sink_target,
            RegFieldDesc("target", "Trace sink target"))
        )
      ):_*
    )
  }
}

class TraceSinkArbiter(n: Int) extends Module {
  val io = IO(new Bundle {
    val target = Input(UInt(TraceSinkTarget.getWidth.W))
    val in = Flipped(Decoupled(UInt(8.W)))
    val out = Vec(n, Decoupled(UInt(8.W)))
  })
  io.in.ready := io.out(io.target).ready
  io.out.zipWithIndex.foreach { case (o, i) => 
    o.valid := io.in.valid && (io.target === i.U)
    o.bits := io.in.bits
  }
}

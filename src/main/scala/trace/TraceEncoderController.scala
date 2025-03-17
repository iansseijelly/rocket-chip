// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.diplomacy.{AddressSet}
import freechips.rocketchip.resources.{SimpleDevice}
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}

object TraceSinkTarget {
  def width = 8
}

class TraceEncoderControlInterface() extends Bundle {
  val enable = Bool()
  val target = UInt(TraceSinkTarget.width.W)
  val bp_mode = UInt(32.W)
  val hpm_target = UInt(TraceSinkTarget.width.W)
  val hpmcounter_enable = UInt(32.W)
  val hpmcounter_report_interval = UInt(32.W)
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

    val trace_encoder_impl = RegInit(0.U(32.W))

    val trace_sink_target = RegInit(0.U(TraceSinkTarget.width.W))
    io.control.target := trace_sink_target.asUInt

    val trace_bp_mode = RegInit(0.U(32.W))
    io.control.bp_mode := trace_bp_mode

    val trace_hpm_target = RegInit(0.U(TraceSinkTarget.width.W))
    io.control.hpm_target := trace_hpm_target.asUInt

    val trace_hpmcounter_enable = RegInit(0.U(32.W))
    io.control.hpmcounter_enable := trace_hpmcounter_enable

    val trace_hpmcounter_report_interval = RegInit(0.U(32.W))
    io.control.hpmcounter_report_interval := trace_hpmcounter_report_interval

    def traceEncoderControlRegWrite(valid: Bool, bits: UInt): Bool = {
      control_reg_write_valid := valid
      when (control_reg_write_valid) {
        control_reg_bits := bits
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
          RegField.r(32, trace_encoder_impl,
            RegFieldDesc("impl", "Trace encoder implementation"))
        ),
        0x20 -> Seq(
          RegField(1, trace_sink_target,
            RegFieldDesc("target", "Trace sink target"))
        ),
        0x24 -> Seq(
          RegField(32, trace_bp_mode,
            RegFieldDesc("bp_mode", "Trace branch predictor mode"))
        ),
        0x30 -> Seq(
          RegField(32, trace_hpm_target,
            RegFieldDesc("hpm_target", "Trace hpm target"))
        ),
        0x34 -> Seq(
          RegField(32, trace_hpmcounter_enable,
            RegFieldDesc("hpmcounter_enable", "Trace hpm counter enable"))
        ),
        0x38 -> Seq(
          RegField(32, trace_hpmcounter_report_interval,
            RegFieldDesc("hpmcounter_report_interval", "Trace hpm counter report interval"))
        )
      ):_*
    )
  }
}
// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import freechips.rocketchip.diplomacy.{AddressSet}
import freechips.rocketchip.resources.{Description, ResourceBindings, SimpleDevice}
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}

object TraceSinkTarget {
  def width = 8
}

class TraceEncoderControlInterface() extends Bundle {
  val enable = Bool()
  val target = UInt(TraceSinkTarget.width.W)
  val bp_mode = UInt(32.W)
  // Lossy mode: on queue pressure the encoder emits Sync Pause, drops whole
  // retire groups, and emits Sync Resume once the queues drain, instead of
  // asserting stall to the core. Its own register so it is never written
  // together with enable.
  val lossy = Bool()
}

class TraceEncoderPerformanceInterface() extends Bundle {
  // packet queues are at the high watermark this cycle. Lossless mode: the
  // core is being stalled. Lossy mode: the core would have been stalled.
  val full = Bool()
  // inside a Pause..Resume gap this cycle
  val paused = Bool()
  // a Pause packet was enqueued this cycle
  val pause_fire = Bool()
  // packets discarded this cycle (whole retire groups; up to nGroups)
  val dropped_inc = UInt(8.W)
}

class TraceEncoderController(addr: BigInt, beatBytes: Int, hartId: Int)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice(s"trace-encoder-controller$hartId", Seq("ucbbar,trace")) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      val traceDmaProp = Map(
        "ucbbar,trace-dma" -> resources("ucbbar,trace-dma").map(_.value)
      ).collect { case (k, v) if v.nonEmpty => (k, v) }
      Description(name, mapping ++ traceDmaProp)
    }
  }
  val node = TLRegisterNode(
    address = Seq(AddressSet(addr, 0xFF)),
    device = device,
    beatBytes = beatBytes
  )
  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val control = Output(new TraceEncoderControlInterface())
      val perf = Input(new TraceEncoderPerformanceInterface())
    })

    val control_reg_write_valid = Wire(Bool())
    val control_reg_bits = RegInit(1.U(2.W))
    val enable = control_reg_bits(1) // 2 means enable, 0 means disable
    val active = control_reg_bits(0) // 1 means active, 0 means inactive
    io.control.enable := enable

    val trace_encoder_impl = RegInit(0.U(32.W))

    val trace_sink_target = RegInit(0.U(TraceSinkTarget.width.W))
    io.control.target := trace_sink_target.asUInt

    val trace_bp_mode = RegInit(0.U(32.W))
    io.control.bp_mode := trace_bp_mode

    val trace_lossy = RegInit(0.U(32.W))
    io.control.lossy := trace_lossy(0)

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

    val full = RegInit(0.U(64.W))
    when (io.perf.full) {
      full := full + 1.U
    }
    val gap_cycles = RegInit(0.U(64.W))
    when (io.perf.paused) {
      gap_cycles := gap_cycles + 1.U
    }
    val dropped_packets = RegInit(0.U(64.W))
    dropped_packets := dropped_packets + io.perf.dropped_inc
    val pause_count = RegInit(0.U(64.W))
    when (io.perf.pause_fire) {
      pause_count := pause_count + 1.U
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
          RegField(8, trace_sink_target,
            RegFieldDesc("target", "Trace sink target"))
        ),
        0x24 -> Seq(
          RegField(32, trace_bp_mode,
            RegFieldDesc("bp_mode", "Trace branch predictor mode"))
        ),
        0x28 -> Seq(
          RegField.r(64, full,
            RegFieldDesc("full", "Cycles the packet queues were at the high watermark (lossless: core stalled; lossy: core would have stalled)"))
        ),
        0x30 -> Seq(
          RegField(32, trace_lossy,
            RegFieldDesc("lossy", "Lossy mode (bit 0): pause/resume instead of stalling the core"))
        ),
        0x38 -> Seq(
          RegField.r(64, gap_cycles,
            RegFieldDesc("gapCycles", "Cycles spent inside Pause..Resume gaps"))
        ),
        0x40 -> Seq(
          RegField.r(64, dropped_packets,
            RegFieldDesc("droppedPackets", "Packets discarded in lossy mode"))
        ),
        0x48 -> Seq(
          RegField.r(64, pause_count,
            RegFieldDesc("pauseCount", "Number of Pause packets emitted"))
        )
      ):_*
    )
  }
}

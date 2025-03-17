// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import scala.math.min

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.CSR

case class TraceEncoderParams(
  encoderBaseAddr: BigInt,
  buildEncoder: Parameters => LazyTraceEncoder,
  useArbiterMonitor: Boolean,
  // a seq of functions that takes a parameter and returns a lazymodule and a target id
  buildSinks: Seq[Parameters => (LazyTraceSink, Int)] = Seq.empty[Parameters => (LazyTraceSink, Int)],
  buildHPMEncoder: Parameters => LazyHPMEncoder,
  buildHPMSinks: Seq[Parameters => (LazyTraceSink, Int)] = Seq.empty[Parameters => (LazyTraceSink, Int)]
)

class LazyTraceEncoder(val coreParams: TraceCoreParams)(implicit p: Parameters) extends LazyModule {
  override lazy val module = new LazyTraceEncoderModule(this)
  override def shouldBeInlined = false
}

class LazyTraceEncoderModule(outer: LazyTraceEncoder) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val control = Input(new TraceEncoderControlInterface())
    val in = Input(new TraceCoreInterface(outer.coreParams))
    val stall = Output(Bool())
    val out = Decoupled(UInt(8.W))
  })
}

class LazyHPMEncoder(val xlen: Int)(implicit p: Parameters) extends LazyModule {
  override lazy val module = new LazyHPMEncoderModule(this)
  override def shouldBeInlined = false
}

class LazyHPMEncoderModule(outer: LazyHPMEncoder) extends LazyModuleImp(outer) {
  val io = IO(new Bundle {
    val control = Input(new TraceEncoderControlInterface())
    val time = Input(UInt(outer.xlen.W))
    val hpmcounters = Input(Vec(CSR.nHPM, UInt(CSR.hpmWidth.W)))
    val out = Decoupled(UInt(8.W))
  })
}
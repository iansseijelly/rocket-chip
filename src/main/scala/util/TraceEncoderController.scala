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
    val enable = RegInit(false.B)
    io.control.enable := enable
    val regmap = node.regmap(
      Seq(
        0x00 -> Seq(
          RegField.r(1, enable, RegFieldDesc("enable", "Enable trace encoder"))
        )
      ):_*
    )
  }
}


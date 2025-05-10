// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._

case object TrackingParams {
  val nEvents = 8
  val eventIFL1 = 0 // L1 I$ miss
  val eventIFTLB = 1 // TLB miss
  val eventSQ = 2 // SQ full
  val eventFLMB = 3 // flush due to misprediction
  val eventFLEX = 4 // flush due to exception
  val eventLSL1D = 5 // L1 D$ miss
  val eventLSTLB = 6 // L1 I$ miss
  val eventLSLLC = 7 // LLC miss
}

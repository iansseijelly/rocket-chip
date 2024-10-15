// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._

class TraceEncoderParams(
    val coreParams: TraceCoreParams,
    val bufferDepth: Int
) 

class TraceEncoder(val params: TraceEncoderParams) extends Module {
    val io = IO(new Bundle {
        val in = Input(new TraceCoreInterface(params.coreParams))
        val out = Output(UInt(8.W))
    })

}


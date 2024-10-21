package freechips.rocketchip.util

import chisel3._
import chisel3.util._

class TraceSinkPrint() extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
  })

  io.in.ready := true.B
  when (io.in.fire) {
    // TODO: make this a direct binary dump
    printf ("%x", io.in.bits)
  }
}

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

class TraceSinkPrint() extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(UInt(8.W)))
  })

  io.in.ready := true.B
  val byte_printer = Module(new BytePrinter())
  byte_printer.io.clk := clock
  byte_printer.io.reset := reset
  byte_printer.io.in_valid := io.in.valid
  byte_printer.io.in_byte := io.in.bits
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

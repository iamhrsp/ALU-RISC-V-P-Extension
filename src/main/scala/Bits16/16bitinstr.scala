package Bits16

import chisel3._
import chisel3.util._

class PADDH extends Module {
  val io = IO(new Bundle {
    val Rs1 = Input(UInt(32.W))
    val Rs2 = Input(UInt(32.W))
    val Rd  = Output(UInt(32.W))
    val vxsat = Output(UInt(1.W))
  })

  // function of class PADDH
  val sWire   = Wire(Vec(2,UInt(17.W)))
  val vxsatOV = RegInit(0.U(1.W))

  for (x <- 0 until 2) {
    val a = io.Rs1((x*16+15) , (x*16+0))
    val b = io.Rs2((x*16+15) , (x*16+0))
    val t = Wire(UInt(17.W)) 

    t        := (a +& b)
    sWire(x) := t
  }
  io.Rd    := Cat(sWire(1)(15,0) , sWire(0)(15,0))
  io.vxsat := vxsatOV
}
object PADDHMain extends App {
  // Instantiate PADDH and generate Verilog
  (new chisel3.stage.ChiselStage).emitVerilog(new PADDH(), Array("--target-dir", "generated"))
}


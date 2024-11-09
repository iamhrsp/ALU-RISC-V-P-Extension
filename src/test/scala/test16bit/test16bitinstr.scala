package test16bit

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import Bits16._
///============================TESTER====PADD.H -- SIMD 16bit Addition=========================///

class PADDHTester extends AnyFlatSpec with ChiselScalatestTester {
  "PADDH" should "correctly compute 16bit addition" in {
    test(new PADDH) { dut =>

        dut.io.Rs1.poke(BigInt("7FFFFFFF", 16).U) 
        dut.io.Rs2.poke(BigInt("7FFF0000", 16).U)
        dut.clock.step()
        dut.io.Rd.expect(BigInt("FFFEFFFF", 16).U) //0x7FFF + 0x7FFF = 32767 + 32767 = 65534 → 0xFFFE || 0xFFFF + 0x0001 = 65535 + 1 = 65536 → 0x10000, Carry discarded 
            
        dut.io.Rs1.poke(BigInt("80008000", 16).U) 
        dut.io.Rs2.poke(BigInt("7FFF7FFF", 16).U)
        dut.clock.step()
        dut.io.Rd.expect(BigInt("FFFFFFFF", 16).U) 
        //0x8000 + 0x7FFF =  32768 + 32767 = 65535 → 0xFFFF  ---Unsigned Addition interpretation
        //0x8000 + 0x7FFF = -32768 + 32767 = -1    → 0xFFFF  ---Signed Addition interpretation  
    }
  }
}

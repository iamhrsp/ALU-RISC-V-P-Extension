import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import Bits8._

///============================TESTER====PAADD.B -- SIMD 8bit Addition=========================///

class PADDBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PADDB" should "correctly compute 8bit addition" in {
    test(new PADDB) { dut =>
      // Test Case 1
      val Rs1 = 0x00204060.U // First 32-bit word (Byte 4: 0x00, Byte 3: 0x20, Byte 2: 0x40, Byte 1: 0x60)
      val Rs2 = 0x01030507.U // Second 32-bit word (Byte 4: 0x01, Byte 3: 0x03, Byte 2: 0x05, Byte 1: 0x07)     
      val expectedSum = 0x01234567.U // Expected result 
 
      dut.io.Rs1.poke(Rs1)
      dut.io.Rs2.poke(Rs2)
      dut.clock.step()
      dut.io.Rd.expect(expectedSum)

      // Test Case 2 - with  carry between bytes
      val rs1_carry = "hEF_10_F0_F0".U // First 32-bit word
      val rs2_carry = "h01_FF_10_11".U // Second 32-bit word
      val expectedSum_carry = "hF0_0F_00_01".U // Expected result with carry handling
      
      dut.io.Rs1.poke(rs1_carry)
      dut.io.Rs2.poke(rs2_carry)
      dut.clock.step()
      dut.io.Rd.expect(expectedSum_carry)

      // Test Case 3 - signed values  
      dut.io.Rs1.poke("h00_00_00_F6".U)
      dut.io.Rs2.poke("h00_00_00_F3".U)
      dut.clock.step()
      dut.io.Rd.expect("h00_00_00_E9".U)
    }
  }
}

///============================TESTER====PAADD.B -- SIMD 8-bit Signed Averaging Addition=========================///

class PAADDBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PAADDB" should "correctly compute Signed Averaging Addition" in {
    test(new PAADDB) { dut =>
      //Test Case : Rs1 = 0x7F_00_80_00, Rs2 = 0x7F_00_80_00
      dut.io.Rs1.poke(BigInt("7F008000" , 16).U) // Rs1 = 127, 0, -128, 0
      dut.io.Rs2.poke(BigInt("7F008000" , 16).U) // Rs2 = 127, 0, -128, 0
      dut.clock.step()
      // Expected Rd = (127+127)>>1, (0+0)>>1, (-128-128)>>1, (0+0)>>1 = 127, 0, -128, 0 => Rd = 0x7F008000
      dut.io.Rd.expect(BigInt("7F008000" , 16).U)

      // Test Case: typical
      dut.io.Rs1.poke(BigInt("80F280E4" , 16).U)     // Rs1 = 80 --> -128,F2 --> -14, 80 --> -128, E4 --> -28
      dut.io.Rs2.poke(BigInt("F8F6080E" , 16).U)     // Rs2 = F8 -->   -8,F6 --> -10, 08 -->   +8, 0E --> +14 
      dut.clock.step()
                                  // Expected Rd  =         -68 is BC , -12 is F4 , -60 is C4 ,  -7 is F9 in 2's Complement form.
      dut.io.Rd.expect(BigInt("BCF4C4F9" , 16).U)

      // Test Case : Rs1 = 0xFF_01_02_80, Rs2 = 0x00_FF_7E_7F
      dut.io.Rs1.poke(BigInt("FF010280" , 16).U) // Rs1 = -1, 1, 2, -128
      dut.io.Rs2.poke(BigInt("00FF7E7F" , 16).U) // Rs2 = 0, -1, 126, 127
      dut.clock.step()
      // Expected Rd = (-1+0)>>1, (1-1)>>1, (2+126)>>1, (-128+127)>>1 => -1, 0, 64, -1 => Rd = 0xFF0040FF
      dut.io.Rd.expect(BigInt("FF0040FF" , 16).U)

      // Test Case: Normal Addition and Averaging
      dut.io.Rs1.poke(BigInt("12345678" , 16).U) 
      dut.io.Rs2.poke(BigInt("10203040" , 16).U)  
      dut.clock.step()
      // Expected Rd = 0x22 >> 1 = 0x11, 0x54 >> 1 = 0x2A, 0x86 >> 1 = 0x43, 0xB8 >> 1 = 0x5C
      dut.io.Rd.expect(BigInt("112A435C" , 16).U)

      // Test Case: signed decimal
      // dut.io.Rs1.poke(-128.S) 
      // dut.io.Rs2.poke(8.S) 
      // dut.clock.step()
      // //  -128 + 8 = -120 ==> Averaged result is -60
      // dut.io.Rd.expect(-60.S)
    }
  }
}


///============================TESTER====PAADDU.B -- SIMD 8-bit Unsigned Averaging Addition=========================///
class PAADDUBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PAADDUB" should "correctly compute Unsigned Averaging Addition" in {
    test(new PAADDUB) { dut =>
      
      // Test Case:  Rs1 = 0x7F_00_80_00, Rs2 = 0x7F_00_80_00
      dut.io.Rs1.poke("h00_7F_80_40".U) // Rs1 = 127, 0, -128, 0
      dut.io.Rs2.poke("h00_7F_80_80".U) // Rs2 = 127, 0, -128, 0
      dut.clock.step()
      // Expected Rd = (127+127)>>1, (0+0)>>1, (-128-128)>>1, (0+0)>>1 = 127, 0, -128, 0 => Rd = 0x7F008000
      dut.io.Rd.expect("h00_7F_80_60".U)

    }
  }
}
//=============================TESTER=====PSADDU.B -- SIMD 8Bit Unsigned Saturating Addition========================///
class PSADDUBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PSADDUB" should "correctly compute Unsigned Saturating Addition" in {
    test(new PSADDUB) { dut =>
      
      // Test Case:  
      dut.io.Rs1.poke("hFF_FF_80_40".U) // 
      dut.io.Rs2.poke("h02_01_80_80".U) // 
      dut.clock.step()
      //for addition result exceeding the 8bit register limit, output saturates to FF. Eg 127 + 127 = 255 i.e., h80+h80 != h100 but hFF
      dut.io.Rd.expect("hFF_FF_FF_C0".U)

    }
  }
}

///============================TESTER====PADD.W -- SIMD 32-bit Addition=========================///
class PADDWTester extends AnyFlatSpec with ChiselScalatestTester {
  "PADDW" should "correctly compute 32bit addition" in {
    test(new PADDW) { dut =>
      // Test Case 1
      val Rs1 = 0x00204060.U // First 32-bit word (Byte 4: 0x00, Byte 3: 0x20, Byte 2: 0x40, Byte 1: 0x60)
      val Rs2 = 0x01030507.U // Second 32-bit word (Byte 4: 0x01, Byte 3: 0x03, Byte 2: 0x05, Byte 1: 0x07)      
      val expectedSum = 0x01234567.U // Expected result 
 
      dut.io.Rs1.poke(Rs1)
      dut.io.Rs2.poke(Rs2)    
      dut.clock.step()
      dut.io.Rd.expect(expectedSum)

      // Test Case 2 - with  carry between bytes
      val rs1_carry = "hFF_00_F0_F0".U // First 32-bit word
      val rs2_carry = "h01_FF_10_11".U // Second 32-bit word
      val expectedSum_carry = "h01_00_01_01".U // Expected result with carry handling
      
      dut.io.Rs1.poke(rs1_carry)
      dut.io.Rs2.poke(rs2_carry)
      dut.clock.step()
      dut.io.Rd.expect(expectedSum_carry)
    }
  }
}
//===============================================================================================================================================//
//-----------------------------------------------------------------SUBTRACTION-------------------------------------------------------------------
//===============================================================================================================================================//

///============================TESTER====PSUB.B -- SIMD 8-bit Subtraction=========================///
class PSUBBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PSUBB" should "correctly compute 8bit subtraction" in {
    test(new PSUBB) { dut =>

      dut.io.Rs1.poke("h0F_F1_EF_DF".U) // 15 , -15 , -17 , -33  8 bit input values are given in 2's complement form
      dut.io.Rs2.poke("h0D_0D_F3_02".U) // 13 , +13 , -13 ,  +2  8 bit input values are given in 2's complement form
      dut.clock.step()                  // Subtraction
      dut.io.Rd.expect("h02_E4_FC_DD".U)//  2 , -28 ,  -4 , -35  8 bit output result is in 2's complement form.
      
    }
  }
}
///============================TESTER====PASUB.B -- SIMD 8-bit Signed Averaging Subtraction=========================///
class PASUBBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PASUBB" should "correctly compute 8bit Signed Averaging Subtraction" in {
    test(new PASUBB) { dut =>

      dut.io.Rs1.poke(BigInt("7F409C19" , 16).U) // Rs1: +127 (0x7F), +64 (0x40), -100 (0x9C), +25 (0x19)
      dut.io.Rs2.poke(BigInt("83C032DD" , 16).U) // Rs2: -125 (0x3C), -64 (0xC0),  +50 (0x32), -35 (0xDD)
      dut.clock.step()                  // Subtraction      
      dut.io.Rd.expect(BigInt("7E40B51E" , 16).U)//  (+127-(-125) / 2 = +126 (0x4D), (+64 - (-64)) / 2 = +64 (0x40), (-100 - (+50)) / 2 = -75 (0xCB), (+25 - (-35)) / 2 = +30 (0x1E)

      //Test case
      // dut.io.Rs1.poke(-127.S)  
      // dut.io.Rs2.poke(1.S) 
      // dut.clock.step()                 
      
      // dut.io.Rd.expect(-64.S) 
    }
  }
}
///============================TESTER====PASUBU.B -- SIMD 8-bit Unsigned Averaging Subtraction=========================///
class PASUBUBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PASUBUB" should "correctly compute 8bit Unsigned Averaging Subtraction" in {
    test(new PASUBUB) { dut =>

      dut.io.Rs1.poke("hFF_96_C8_0A".U) // Rs1: 255 (0xFF), 150 (0x96), 200 (0xC8), 10 (0x0A)
      dut.io.Rs2.poke("h64_3C_96_05".U) // Rs2: 100 (0x64), 60 (0x3C), 150 (0x96), 5 (0x05)
      dut.clock.step()                  // Subtraction     
      dut.io.Rd.expect("h4D_2D_19_02".U)//  (255 - 100) / 2 = 77 (0x4D), (150 - 60) / 2 = 45 (0x2D), (200 - 150) / 2 = 25 (0x19), (10 - 5) / 2 = 2 (0x02)
      
    }
  }
}
///============================TESTER====PSSUB.B -- SIMD 8-bit Signed Saturating Subtraction=========================///
class PSSUBBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PSSUBB" should "correctly compute 8bit Signed Saturating Subtraction" in {
    test(new PSSUBB) { dut =>

      // dut.io.Rs1.poke(-127.S) 
      // dut.io.Rs2.poke(50.S) 
      // dut.clock.step()                  
      // dut.io.Rd.expect(-128.S)

      dut.io.Rs1.poke(BigInt("7F8020FF" , 16).U) // Rs1: +127 (0x7F), -128 (0x80), +32 (0x20), -1 (0xFF)
      dut.io.Rs2.poke(BigInt("81803001" , 16).U) // Rs2: -127 (0x81), -128 (0x80), +48 (0x30), +1 (0x01)
      dut.clock.step()                  // Subtraction
      dut.io.Rd.expect(BigInt("7F00F0FE" , 16).U)// (+127 - -127 = +127 (saturated, 0x7F)), 
      dut.io.vxsat.expect(1.U)           // (-128 - (-128) = 0x00), 
                                         // (+32 - +48 = -16 (0xF0)), 
                                         // (-1 - +1 = -2 (0xFE))

      dut.io.Rs1.poke(BigInt("407F8180" , 16).U) // Rs1: +64 (0x40), +127 (0x7F), -127 (0x81), -128 (0x80)
      dut.io.Rs2.poke(BigInt("C07F7F00" , 16).U) // Rs2: -64 (0xC0), +127 (0x7F), +127 (0x7F), 0 (0x00)
      dut.clock.step()                  // Subtraction
      dut.io.Rd.expect(BigInt("7F008080" , 16).U)// (+64 - (-64) = +127 (0x7F), 
      dut.io.vxsat.expect(1.U)           // (+127 - +127 = 0x00), 
                                         // (-127 - +127 = -128 (saturated, 0x80)), 
                                         // (-128 - 0 = -128 (0x80))    
    }
  }
}
///============================TESTER====PASUBU.B -- SIMD 8-bit Unsigned Averaging Subtraction=========================///
class PSSUBUBTester extends AnyFlatSpec with ChiselScalatestTester {
  "PSSUBUB" should "correctly compute 8bit Unsigned Saturating Subtraction" in {
    test(new PSSUBUB) { dut =>

      // Test case 1:
      dut.io.Rs1.poke("h50_20_FF_10".U)  // Rs1: 80 (0x50), 32 (0x20), 255 (0xFF), 16 (0x10)
      dut.io.Rs2.poke("h30_40_80_10".U)  // Rs2: 48 (0x30), 64 (0x40), 128 (0x80), 16 (0x10)
      dut.clock.step() 
      dut.io.Rd.expect("h20_00_7F_00".U) // Rd: (80 - 48) = 32 (0x20), (32 - 64) = 0 (saturated to 0x00), (255 - 128) = 127 (0x7F), (16 - 16) = 0 (0x00)
      dut.io.vxsat.expect(1.U)           // vxsat set due to saturation in the second and fourth elements
      
      // Test case 2:
      dut.io.Rs1.poke("h05_64_A0_F0".U)  // Rs1: 5 (0x05), 100 (0x64), 160 (0xA0), 240 (0xF0)
      dut.io.Rs2.poke("h10_32_B0_FF".U)  // Rs2: 16 (0x10), 50 (0x32), 176 (0xB0), 255 (0xFF)
      dut.clock.step() 
      dut.io.Rd.expect("h00_32_00_00".U) // Rd: (5 - 16) = 0 (saturated to 0x00), (100 - 50) = 50 (0x32), (160 - 176) = 0 (saturated to 0x00), (240 - 255) = 0 (saturated to 0x00)
      dut.io.vxsat.expect(1.U)           // vxsat set due to saturation in the first, third, and fourth elements
      
    }
  }
 }

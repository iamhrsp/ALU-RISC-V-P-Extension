import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class PextALUTest extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "PextALU"

  it should "perform each operation with predefined inputs and check expected results" in {
    test(new PextALU) { dut =>

      // Define test cases with specific Rs1, Rs2 values and expected Rd, vxsat for each operation
      val testCases = Seq(
        
        //============================
        //PADD.B -- SIMD 8bit Addition
        //============================
        // Rs1 = 0x00204060 -> [0x00, 0x20, 0x40, 0x60] (Decimal: [0, 32, 64, 96])
        // Rs2 = 0x01030507 -> [0x01, 0x03, 0x05, 0x07] (Decimal: [1, 3, 5, 7])
        // Expected Rd = 0x01234567 -> [0x01, 0x23, 0x45, 0x67] (Decimal: [1, 35, 69, 103])
        (ALUops.PADDB, 0x00204060.U, 0x01030507.U, 0x01234567.U, 0.U),
        // Rs1 = 0xEF10F0F0 -> [0xEF, 0x10, 0xF0, 0xF0] (Decimal: [239, 16, 240, 240])
        // Rs2 = 0x01FF1011 -> [0x01, 0xFF, 0x10, 0x11] (Decimal: [1, 255, 16, 17])
        // Expected Rd = 0xF00F0001 -> [0xF0, 0x0F, 0x00, 0x01] (Decimal: [240, 15, 0, 1])
        (ALUops.PADDB, "hEF_10_F0_F0".U, "h01_FF_10_11".U, "hF0_0F_00_01".U, 0.U) 


      )

      // Test each case
      for ((operation, rs1, rs2, expectedRd, expectedVxsat) <- testCases) {
        // Apply inputs
        dut.io.Rs1.poke(rs1)
        dut.io.Rs2.poke(rs2)
        dut.io.operation.poke(operation)
        
        // Step the clock and check the result
        dut.clock.step(1)

        // Expect values
        dut.io.Rd.expect(expectedRd, s"Failed on operation $operation with Rs1=$rs1, Rs2=$rs2")
        dut.io.vxsat.expect(expectedVxsat, s"Overflow flag failed on operation $operation with Rs1=$rs1, Rs2=$rs2")
        
        // Log information for clarity
        println(s"Tested $operation with Rs1 = ${rs1.litValue}, Rs2 = ${rs2.litValue}")
        println(s"Expected Rd = ${expectedRd.litValue}, Got Rd = ${dut.io.Rd.peek().litValue}")
        println(s"Expected vxsat = ${expectedVxsat.litValue}, Got vxsat = ${dut.io.vxsat.peek().litValue}")

        // Extra step for waveform clarity
        dut.clock.step(1)
      }
    }
  }
}
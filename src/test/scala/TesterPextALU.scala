import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class PextALUWrapperTester extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "PextALU"

  it should "perform each operation with predefined inputs and check expected results" in {
    test(new PextALUWrapper) { dut =>

      // Define test cases with specific Rs1, Rs2 values and expected Rd, vxsat for each operation
      val testCases = Seq(
        
        //============================
        //PADD.B -- SIMD 8bit Addition
        //============================
        // Rs1 = 0xEF10F0F0 -> [0xFE, 0x10, 0xF0, 0xF0]         (Decimal: [254, 16, 240, 240])
        // Rs2 = 0x01FF1011 -> [0x01, 0xFF, 0x10, 0x11]         (Decimal: [1, 255, 16, 17])
        // Expected Rd = 0xFF0F0001 -> [0xFF, 0x0F, 0x00, 0x01] (Decimal: [255, 15, 0, 1])
        (ALUops.PADDB, "hFE_10_F0_F0".U, "h01_FF_10_11".U, "hFF_0F_00_01".U, 0.U), 
        
        // SIGNED//
        // Rs1 = 0x9B47E3B2 -> [0x9B, 0x47, 0xE3, 0xB2]         (Decimal: [-101,  71, -29, -78])
        // Rs2 = 0x2D98C4FF -> [0x2D, 0x98, 0xC4, 0xFF]         (Decimal: [  45, -104, -60,  -1])
        // Expected Rd = 0xC1DFA7B1 -> [0xC1, 0xDF, 0xA7, 0xB1] (Decimal: [-63, -33, -89, -79])
        //UNSIGNED//
        // Rs1 = h9B_47_E3_B2 -> [0x9B, 0x47, 0xE3, 0xB2]         (Decimal: [155,  71, 227, 178])
        // Rs2 = h2D_98_C4_FF -> [0x2D, 0x98, 0xC4, 0xFF]         (Decimal: [ 45, 152, 196, 255])
        // Expected Rd = hC8_DF_A1_B1 -> [0xC8, 0xDF, 0xA7, 0xB1] (Decimal: [193, 223, 161, 177])
        (ALUops.PADDB, "h9B_47_E3_B2".U, "h2D_98_C4_FF".U, "hC8_DF_A7_B1".U, 0.U),
        
        //===============================================
        //PAADD.B -- SIMD 8-bit Signed Averaging Addition
        //===============================================  
        // Rs1 = 0x7F804000 -> [0x7F, 0x80, 0x40, 0x00]         (Decimal: [ 127, -128,  64,   0])
        // Rs2 = 0x7F808000 -> [0x7F, 0x80, 0x80,  0x00]        (Decimal: [ 127, -128, -128,  0])
        // Expected Rd = 0x7F803000 -> [0x7F, 0x80, 0xE0, 0x00] (Decimal: [ 127, -128,  -32,   0])
        (ALUops.PAADDB, "h7F_80_40_00".U, "h7F_80_80_00".U, "h7F_80_E0_00".U, 0.U),

        //==================================================
        //PAADDU.B -- SIMD 8-bit Unsigned Averaging Addition
        //==================================================
        // Rs1 = 0x7F804000 -> [0x7F, 0x80, 0x40, 0x00]         (Decimal: [ 127,  128,  64,   0])       SPEC TEST CASE
        // Rs2 = 0x7F804000 -> [0x7F, 0x80, 0x80,  0x00]        (Decimal: [ 127,  128,  128,  0])
        // Expected Rd = 0x7F806000 -> [0x7F, 0x80, 0x60, 0x00] (Decimal: [ 127,  128,   96,   0])
        (ALUops.PAADDUB, "h7F_80_40_00".U, "h7F_80_80_00".U, "h7F_80_60_00".U, 0.U),

        //==================================================
        //PSADDU.B -- SIMD 8Bit Unsigned Saturating Addition
        //==================================================
        // Rs1 = hFF_FF_80_40 -> [0xFF, 0xFF, 0x80, 0x40]         (Decimal: [255, 255, 128,  64])
        // Rs2 = h02_01_80_80 -> [0x02, 0x01, 0x80, 0x80]         (Decimal: [  2,   1, 128, 128])
        // Expected Rd = hFF_FF_FF_C0 -> [0xFF, 0xFF, 0xFF, 0xC0] (Decimal: [255, 255, 255, 192])
        // vxsat = 0x00000001 
        (ALUops.PSADDUB, "hFF_FF_80_40".U, "h02_01_80_80".U, "hFF_FF_FF_C0".U, "h00000001".U),

        //===============================
        //PSUB.B -- SIMD 8Bit Subtraction
        //===============================
        // Rs1 = 0xFF_F1_EF_DF -> [0xFF, 0xF1, 0xEF, 0xDF]         (Decimal: [ 255, -15, -17, -33])
        // Rs2 = 0x00_0D_F3_02 -> [0x00, 0x0D, 0xF3, 0x02]         (Decimal: [   0,  13, -13,   2])
        // Expected Rd = 0x02_E4_FC_DD -> [0x02, 0xE4, 0xFC, 0xDD] (Decimal: [  255, -28,  -4, -35])
        // vxsat = 0x00000000 
        (ALUops.PSUBB, "hFF_F1_EF_DF".U, "h00_0D_F3_02".U, "hFF_E4_FC_DD".U, "h00000001".U),       //Here vxsat is one because this operation retains the vxsat_in value

        //=================================================
        //PASUB.B -- SIMD 8Bit Signed Averaging Subtraction
        //=================================================
        // Rs1 = 0x7F_80_80_80 -> [0x7F, 0x80, 0x80, 0x80]         (Decimal: [ 127, -128, -128, -128])    SPEC TEST CASE
        // Rs2 = 0x80_7F_40_80 -> [0x80, 0x7F, 0x40, 0x80]         (Decimal: [-128,  127,   64, -128])
        // Expected Rd = 0x7F_80_A0_80 -> [0x7F, 0x80, 0xA0, 0x00] (Decimal: [ 127, -128,  -96,   0])
        // vxsat = 0x00000000
        (ALUops.PASUBB, "h7F_80_80_80".U, "h80_7F_40_80".U, "h7F_80_A0_00".U, "h00000001".U),     //Here vxsat is one because this operation retains the vxsat_in value
 
        //====================================================
        //PASUBU.B -- SIMD 8Bit Unsigned Averaging Subtraction  
        //====================================================
        // Rs1 = 0x7F_80_80_81 -> [0x7F, 0x80, 0x80, 0x81]         (Decimal: [ 127, -128, -128, -127])    SPEC TEST CASE
        // Rs2 = 0x80_7F_40_01 -> [0x80, 0x7F, 0x40, 0x01]         (Decimal: [-128,  127,   64,    1])
        // Expected Rd = 0xFF_00_20_40 -> [0xFF, 0x00, 0x20, 0x40] (Decimal: [ 255,    0,   32,   64])
        // vxsat = 0x00000000 
        (ALUops.PASUBUB, "h7F_80_80_81".U, "h80_7F_40_01".U, "hFF_00_20_40".U, "h00000001".U),    //Here vxsat is one because this operation retains the vxsat_in value

        //==================================================
        //PSSUB.B -- SIMD 8Bit Signed Saturating Subtraction
        //==================================================        
        // Rs1 = 0x7F_80_20_FF -> [0x00, 0x80, 0x20, 0xFF]         (Decimal: [ 0, -128,  32,  -1])
        // Rs2 = 0x81_80_30_01 -> [0x81, 0x80, 0x30,  0x01]        (Decimal: [-127, -128,  48,   1])
        // Expected Rd = 0x7F_00_F0_FE -> [0x7F, 0x00, 0xF0, 0xFE] (Decimal: [ 127,    0, -16,  -2])
        // vxsat = 0x00000000 
        (ALUops.PSSUBB, "h00_80_20_FF".U, "h81_80_30_01".U, "h7F_00_F0_FE".U, "h00000000".U),       // vxsat now is updated with value of 0
        // Rs1 = 0x40_7F_81_80 -> [0x40, 0x7F, 0x81, 0x80]         (Decimal: [  64,  127, -127, -128])
        // Rs2 = 0xC0_7F_7F_00 -> [0xC0, 0x7F, 0x7F,  0x00]        (Decimal: [ -64,  127,  127,    0])
        // Expected Rd = 0x7F_00_80_80 -> [0x7F, 0x00, 0x80, 0x80] (Decimal: [ 127,    0, -128, -128])
        // vxsat = 0x00000001 (Overflow occurred)
        (ALUops.PSSUBB, "h40_7F_81_80".U, "hC0_7F_7F_00".U, "h7F_00_80_80".U, "h00000001".U),     //  vxsat updates to 1 

        //=======================================================
        // PSSUBU.B -- SIMD 8-bit Unsigned Saturating Subtraction
        //=======================================================
        // Rs1 = 0x50_20_FF_10 -> [0x50, 0x40, 0xFF, 0x10]         (Decimal: [ 80,  64, 255,  16])
        // Rs2 = 0x30_40_80_10 -> [0x30, 0x20, 0x00, 0x10]         (Decimal: [ 48,  32, 0,  16])
        // Expected Rd = 0x20_20_7F_00 -> [0x20, 0x20, 0xFF, 0x00] (Decimal: [ 32,   32, 255,   0])
        // vxsat = 0x00000000
        (ALUops.PSSUBUB, "h50_40_FF_10".U, "h30_20_00_10".U, "h20_20_FF_00".U, "h00000000".U),    //  vxsat updates to 0 
        // Rs1 = 0x05_64_A0_F0 -> [0x05, 0x64, 0xA0, 0xF0]         (Decimal: [  5, 100, 160, 240])
        // Rs2 = 0x10_32_B0_FF -> [0x10, 0x32, 0xB0, 0xFF]         (Decimal: [ 16,  50, 176, 255])
        // Expected Rd = 0x00_32_00_00 -> [0x00, 0x32, 0x00, 0x00] (Decimal: [  0,  50,   0,   0])
        // vxsat = 0x00000001 (Saturation occurred)
        (ALUops.PSSUBUB, "h05_64_A0_F0".U, "h10_32_B0_FF".U, "h00_32_00_00".U, "h00000001".U),     //  vxsat updates to 1 

        //====================================================16 Bit Operations====================================================// 
    
        //==============================
        //PADD.H -- SIMD 16-bit Addition
        //==============================
        // Rs1 = 0x1234_5678 -> [0x1234, 0x5678]          (Decimal: [4660, 22136])
        // Rs2 = 0x1111_2222 -> [0x1111, 0x2222]          (Decimal: [4369, 8738])
        // Expected Rd = 0x2345_789A -> [0x2345, 0x789A]  (Decimal: [9030, 30874])
        // vxsat = 0x00000000 (No overflow for this instruction)
        (ALUops.PADDH, "h1234_5678".U, "h1111_2222".U, "h2345_789A".U, "h00000001".U),      // Status reg retains the previous value in this operation

        //================================================
        //PAADD.H -- SIMD 16-bit Signed Averaging Addition
        //================================================ 
        // Rs1 = 0x7FFF_8000 -> [0x7FFF, 0x8000]          (Decimal: [ 32767, -32768])            SPEC TEST CASE
        // Rs2 = 0x7FFF_8000 -> [0x7FFF, 0x8000]          (Decimal: [ 32767, -32768])
        // Expected Rd = 0x7FFF_8000 -> [0x7FFF, 0x8000]  (Decimal: [ 32767, -32768])
        // vxsat = 0x00000000 (No overflow for averaging addition)
        (ALUops.PAADDH, "h7FFF_8000".U, "h7FFF_8000".U, "h7FFF_8000".U, "h00000001".U),     // Status reg retains the previous value in this operation
        // Rs1 = 0x4000_FF01 -> [0x4000, 0xFF01]         (Decimal: [ 16384,   -255])
        // Rs2 = 0x8000_00FE -> [0x8000, 0x00FE]         (Decimal: [-32768,    254])
        // Expected Rd = 0xE000_FF00 -> [0xE000, 0xFFFF] (Decimal: [ -8192,   -1])
        // vxsat = 0x00000000 (No overflow for averaging addition)    
        (ALUops.PAADDH, "h4000_FF01".U, "h8000_00FE".U, "hE000_FFFF".U, "h00000001".U),     // Status reg retains the previous value in this operation


        //===================================================
        //PAADDU.H -- SIMD 16-bit Unsigned Averaging Addition
        //===================================================
        // Rs1 = 0x7FFF_8000 -> [0x7FFF, 0x8000]          (Decimal: [32767, 32768])               SPEC TEST CASE
        // Rs2 = 0x7FFF_8000 -> [0x7FFF, 0x8000]          (Decimal: [32767, 32768])
        // Expected Rd = 0x7FFF_8000 -> [0x7FFF, 0x8000]  (Decimal: [32767, 32768])
        // vxsat = 0x00000000 (No overflow for unsigned averaging addition)
        (ALUops.PAADDUH, "h7FFF_8000".U, "h7FFF_8000".U, "h7FFF_8000".U, "h00000001".U),    // Status reg retains the previous value in this operation
        // Rs1 = 0x4000_1234 -> [0x4000, 0x1234]          (Decimal: [16384, 4660])                 SPEC TEST CASE
        // Rs2 = 0x8000_5678 -> [0x8000, 0x5678]          (Decimal: [32768, 22136])
        // Expected Rd = 0x6000_3456 -> [0x6000, 0x3456]  (Decimal: [24576, 13398])
        // vxsat = 0x00000000 (No overflow for unsigned averaging addition)
        (ALUops.PAADDUH, "h4000_1234".U, "h8000_5678".U, "h6000_3456".U, "h00000001".U),     // Status reg retains the previous value in this operation

        //===================================================
        //PSADD.H -- SIMD 16-bit Signed Saturating Addition
        //===================================================
        // Rs1 = 0x7000_8000 -> [0x7000, 0x8000]          (Decimal: [28672, -32768])
        // Rs2 = 0x9000_1000 -> [0x9000, 0x1000]          (Decimal: [-28672, 4096])
        // Expected Rd = 0x0000_9000 -> [0x0000, 0x9000]  (Decimal: [0, -28672])
        // vxsat = 0x00000000 (No saturation occurred)          
        (ALUops.PSADDH, "h7000_8000".U, "h9000_1000".U, "h0000_9000".U, "h00000000".U),     // Status reg updates to 0
        // Rs1 = 0x7FFF_8000 -> [0x7FFF, 0x8000]          (Decimal: [32767, -32768])
        // Rs2 = 0x0001_FFFF -> [0x0001, -1]              (Decimal: [1, -1])
        // Expected Rd = 0x7FFF_8000 -> [0x7FFF, 0x8000]  (Decimal: [32767, -32768])
        // vxsat = 0x00000001 (Saturation occurred)
        (ALUops.PSADDH, "h7FFF_8000".U, "h0001_FFFF".U, "h7FFF_8000".U, "h00000001".U),       // Status reg updates to 1

        //====================================================
        //PSADDU.H -- SIMD 16-bit Unsigned Saturating Addition
        //====================================================
        // Rs1 = 0x2000_3000 -> [0x2000, 0x3000]          (Decimal: [8192, 12288])
        // Rs2 = 0x1000_4000 -> [0x1000, 0x4000]          (Decimal: [4096, 16384])
        // Expected Rd = 0x3000_7000 -> [0x3000, 0x7000]  (Decimal: [12288, 28672])
        // vxsat = 0x00000000 (No saturation occurred)
        (ALUops.PSADDUH, "h2000_3000".U, "h1000_4000".U, "h3000_7000".U, "h00000000".U),       // Status reg updates to 0
        // Rs1 = 0x2000_FFFF -> [0x2000, 0xFFFF]          (Decimal: [8192, 65535])
        // Rs2 = 0x3000_0001 -> [0x3000, 0x0001]          (Decimal: [12288, 1])
        // Expected Rd = 0x5000_FFFF -> [0x5000, 0xFFFF]  (Decimal: [20480, 65535])
        // vxsat = 0x00000001 (Saturation occurred in the second segment)
        (ALUops.PSADDUH, "h2000_FFFF".U, "h3000_0001".U, "h5000_FFFF".U, "h00000001".U),        // Status reg updates to 1
        // Rs1 = 0x7000_8000 -> [0x7000, 0x8000]          (Decimal: [28672, 32768])
        // Rs2 = 0x1000_9000 -> [0x1000, 0x9000]          (Decimal: [4096, 36864])
        // Expected Rd = 0x8000_FFFF -> [0x8000, 0xFFFF]  (Decimal: [32768, 65535])
        // vxsat = 0x00000001 (Saturation occurred in the second segment)
        (ALUops.PSADDUH, "h7000_8000".U, "h1000_9000".U, "h8000_FFFF".U, "h00000001".U),         // Sets status register
        
        //===================================
        //PSUB.H -- SIMD 16-bit Subtraction
        //===================================
        // Rs1 = 0x1030_FF10 -> [0x1030, hFF10]          (Decimal: [4144, -240])
        // Rs2 = 0x0800_FF20 -> [0x0800, hFF20]          (Decimal: [2048, -224])
        // Expected Rd = 0x0830_00F0 -> [0x0830, hFFF0]   (Decimal: [2096,  -16])
        // vxsat = 0x00000000 (No status update required)
        (ALUops.PSUBH, "h1030_FF10".U, "h0800_FF20".U, "h0830_FFF0".U, "h00000001".U),          // Status register retains the previous value of 1
        // Rs1 = 0x8000_8000 -> [0x8000, 0x8000]          (Decimal: [-32768, -32768])
        // Rs2 = 0x7FFF_0001 -> [0x7FFF, 0x0001]          (Decimal: [32767, 1])
        // Expected Rd = 0x0001_7FFF -> [0x0001, 0x7FFF]  (Decimal: [1, 32767])   // Actual results would be F0001 and F7FFF
         // vxsat = 0x00000000 (No status update required)
        (ALUops.PSUBH, "h8000_8000".U, "h7FFF_0001".U, "h0001_7FFF".U, "h00000001".U),           // Status register retains the previous value of 1

        //===================================================
        //PASUB.H -- SIMD 16-bit Signed Averaging Subtraction
        //===================================================
        // Rs1 = 0x7FFF_8000 -> [0x7FFF, 0x8000]          (Decimal: [32767, -32768])      // SPEC TEST CASES
        // Rs2 = 0x8000_7FFF -> [0x8000, 0x7FFF]          (Decimal: [-32768, 32767])
        // Expected Rd = 0x7FFF_8000 -> [0x7FFF, 0x8000]  (Decimal: [32767, -32768])
        // vxsat = 0x00000000 (No saturation occurs in averaging subtraction)
        (ALUops.PASUBH, "h7FFF_8000".U, "h8000_7FFF".U, "h7FFF_8000".U, "h00000001".U),          // Status register retains the previous value of 1
        // Rs1 = 0x8000_7FFF -> [0x8000, 0x7FFF]          (Decimal: [-32768, 32767])
        // Rs2 = 0x4000_2000 -> [0x4000, 0x2000]          (Decimal: [16384, 8192])
        // Expected Rd = 0xA000_2FFF -> [0xA000, 0x2FFF]  (Decimal: [-24576, 12287])
        // vxsat = 0x00000000 (No saturation occurs in averaging subtraction)
        (ALUops.PASUBH, "h8000_7FFF".U, "h4000_2000".U, "hA000_2FFF".U, "h00000001".U),

        //======================================================
        //PASUBU.H -- SIMD 16-bit Unsigned Averaging Subtraction
        //====================================================== 
        // Rs1 = 0x8000_8001 -> [0x8000, 0x8001]          (Decimal: [32768, 32769])       // SPEC TEST CASES
        // Rs2 = 0x4000_0001 -> [0x4000, 0x0001]          (Decimal: [16384, 1])
        // Expected Rd = 0x2000_4000 -> [0x2000, 0x4000]  (Decimal: [8192, 16384])
        (ALUops.PASUBUH, "h8000_8001".U, "h4000_0001".U, "h2000_4000".U, "h00000001".U),         // Status register retains the previous value of 1
        // Rs1 = 0x7FFF_8000 -> [0x7FFF, 0x8000]          (Decimal: [32767, 32768])
        // Rs2 = 0x8000_7FFF -> [0x8000, 0x7FFF]          (Decimal: [32768, 32767])
        // Expected Rd = 0x7FFF_0000 -> [0x7FFF, 0x0000]  (Decimal: [-1,       0])
        (ALUops.PASUBUH, "h7FFF_8000".U, "h8000_7FFF".U, "hFFFF_0000".U, "h00000001".U),          // Status register retains the previous value of 1

        //======================================================
        //PSSUB.H -- SIMD 16-bit Signed Saturating Subtraction
        //======================================================        
        // Rs1 = 0x3F2E_5D4C -> [0x3F2E, 0x5D4C]          (Decimal: [16174, 23884])
        // Rs2 = 0x1A1B_2C3D -> [0x1A1B, 0x2C3D]          (Decimal: [6683, 11325])
        // Expected Rd = 0x2513_310F -> [0x2513, 0x310F]  (Decimal: [9491, 12559])
        // vxsat = 0x00000000 (No saturation occurred)
        (ALUops.PSSUBH, "h3F2E_5D4C".U, "h1A1B_2C3D".U, "h2513_310F".U, "h00000000".U),         // Status register updates to 0
        // Rs1 = 0x7FFF_8000 -> [0x7FFF, 0x8000]          (Decimal: [32767, -32768])
        // Rs2 = 0x8000_7FFF -> [0x8000, 0x7FFF]          (Decimal: [-32768, 32767])
        // Expected Rd = 0x7FFF_8000 -> [0x7FFF, 0x8000]  (Decimal: [32767, -32768])
        // vxsat = 0x00000001 (Overflow occurred)
        (ALUops.PSSUBH, "h7FFF_8000".U, "h8000_7FFF".U, "h7FFF_8000".U, "h00000001".U),            // Status register updates to 1
        // Rs1 = 0x4000_8001 -> [0x4000, 0x8001]          (Decimal: [16384, -32767])
        // Rs2 = 0x2000_0001 -> [0x2000, 0x0001]          (Decimal: [8192, 1])
        // Expected Rd = 0x2000_8000 -> [0x2000, 0x8000]  (Decimal: [8192, -32768])
        // vxsat = 0x00000000 (No overflow occurred)       
        (ALUops.PSSUBH, "h4000_8001".U, "h2000_0001".U, "h2000_8000".U, "h00000000".U),          // Status register updates to 0
        
        //=======================================================
        //PSSUBU.H -- SIMD 16-bit Unsigned Saturating Subtraction
        //=======================================================
        // Rs1 = 0x7A5B_1357 -> [0x7A5B, 0x1357]          (Decimal: [31323, 4951])
        // Rs2 = 0x6A5C_2468 -> [0x6A5C, 0x2468]          (Decimal: [27228, 9320])
        // Expected Rd = 0x0FFF_0000 -> [0x0FFF, 0x0000]  (Decimal: [4095, 0])
        // vxsat = 0x00000001 (Saturation occurred)
        (ALUops.PSSUBUH, "h7A5B1357".U, "h6A5C2468".U, "h0FFF0000".U, "h00000001".U),            // Status register updates to 1
        // Rs1 = 0x1234_ABCD -> [0x1234, 0xABCD]          (Decimal: [4660, 43981])
        // Rs2 = 0x5678_DCBA -> [0x5678, 0xDCBA]          (Decimal: [22136, 56506])
        // Expected Rd = 0x0000_0000 -> [0x0000, 0x0000]  (Decimal: [0, 0])
        // vxsat = 0x00000001 (Saturation occurred)
        (ALUops.PSSUBUH, "h1234ABCD".U, "h5678DCBA".U, "h00000000".U, "h00000001".U),


          








 







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
import chisel3._
import chisel3.util._
//import Bits8._
//import scala.annotation.switch
//import chisel3.experimental.ChiselEnum

// Defining ALU operations as enumeration type using ChiselEnum
object ALUops extends ChiselEnum {
    val PADDB,PAADDB,PAADDUB,PSADDUB , PSUBB,PASUBB,PASUBUB,PSSUBB,PSSUBUB , PADDH,PAADDH,PAADDUH,PSADDH,PSADDUH , PSUBH,PASUBH,PASUBUH,PSSUBH,PSSUBUH, PASHX,PAASHX,PSASHX,PSAHX,PASAHX,PSSAHX  = Value
}

//================================
// 4x8bit ADDER module
//================================
class AdderALU extends Module {
    val io = IO(new Bundle {
      val a        = Input(Vec(4, UInt(9.W)))    // Four 8-bit inputs for Rs1
      val b        = Input(Vec(4, UInt(9.W)))    // Four 8-bit inputs for Rs2
      val carryIn  = Input(Vec(4, UInt(1.W)))      // Carry-in for each byte addition
      val sum      = Output(Vec(4, UInt(9.W)))  // 8-bit sum outputs for each byte
      val carryOut = Output(Vec(4, UInt(1.W)))     // Carry-out for each byte
    })

    val interResult = Wire(Vec(4,UInt(9.W)))
    for(x <- 0 until 4) {   
        // Inputs in UInt and result in UInt
        interResult(x) := io.a(x) +& io.b(x) +& io.carryIn(x)
        io.sum(x)      := interResult(x)
        io.carryOut(x) := interResult(x)(8)
    }     
  }

//==================================================================================
// Configurable Two's Complement generator module for SIMD 8bit and 16bit operations
//==================================================================================
class TwosComplementGenerator extends Module {      
    val io =IO(new Bundle {
        val input    = Input(Vec(4, UInt(9.W)))
        val output   = Output(Vec(4, UInt(9.W))) 
        val widthSel = Input(Bool())    // Configurable for 8bit and 16bit operations. ture.B => 8bit, false.B => 16bit
    })
    // Function of class TwosComplementGenerator
    val complementValue = Wire(Vec(4,UInt(9.W)))
    //******************************************************
    //For 4x8bit Twos Complement generators -> false = 8bits
    //******************************************************
    when(io.widthSel === false.B) {      
        for (m <- 0 until 4) {
            complementValue(m) := ~(io.input(m)) + 1.U     // One's complement of operand B is added 1 to get 2's complement. Concatenation 0 is done to get correct 2's complement for values greater than 127       
        }  
    //******************************************************
    //For 2x16bit Twos Complement generators -> true = 16bits
    //******************************************************
    }.otherwise {   
        val lower16 = Cat(io.input(1) , io.input(0)(7,0))   // input(1) is received as 9bits. input(0) is received as 9bits but only need its 8bits. lower16 is 17bits -> concatenation with MSB of 16bit half word
        val upper16 = Cat(io.input(3) , io.input(2)(7,0))   // Similar as above. For MSB concatenation, check the respective operation.
        //   In detail, this is the concatenation happening :     Cat(io.input(1)(8), io.input(1)(7, 0), io.input(0)(7, 0))

        val complementLower16 = ~lower16 + 1.U
        val complementUpper16 = ~upper16 + 1.U

        // Split the complemented 17-bit results back into 9-bit and 8-bit parts (= 17bits. Takes care of sign extension as well)
        complementValue(0) := complementLower16(7, 0)       // Complemented form of lower 8 bits goes into (9.W) complementValue. 
        complementValue(1) := complementLower16(16, 8)      // Upper 9bits of [Sign Extended to 17bit complemented value] goes into (9.W) complementValue
        complementValue(2) := complementUpper16(7, 0)   // Lower 8 bits of upper half
        complementValue(3) := complementUpper16(16, 8)   // Upper 9 bits with sign
    }
    
    io.output       := complementValue
}
  
//================================
// P Extension ALU
//================================
// Port declaration of ALU
class PextALU extends Module { 
    val io = IO(new Bundle {
        val Rs1       = Input(UInt(32.W))
        val Rs2       = Input(UInt(32.W))
        val operation = Input(ALUops())
        val Rd        = Output(UInt(32.W))
        val vxsat_in  = Input(UInt(32.W))
        val vxsat_out = Output(UInt(32.W))
    })

    // Instantiation
    val fourByteAdder    = Module(new AdderALU())
    val twosComplement   = Module(new TwosComplementGenerator())
    val sumWires         = Wire(Vec(4, UInt(9.W))) 
    val overflowDetected = VecInit(Seq.fill(4)(WireDefault(false.B)))   // A vector of 4 wires to capture overflow info from four adders. Default value is 0

    // Default values
    fourByteAdder.io.a         := VecInit(Seq.fill(4)(0.U(9.W)))
    fourByteAdder.io.b         := VecInit(Seq.fill(4)(0.U(9.W)))
    fourByteAdder.io.carryIn   := VecInit(Seq.fill(4)(0.U(1.W)))
    twosComplement.io.input    := VecInit(Seq.fill(4)(0.U(9.W)))
    twosComplement.io.widthSel := false.B
    sumWires         := VecInit(Seq.fill(4)(0.U(9.W)))
    io.Rd        := 0.U
    io.vxsat_out := 0.U

    // ALU operation selection 

    //============================
    //PADD.B -- SIMD 8bit Addition
    //============================
    when(io.operation === ALUops.PADDB) {                      
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(0)
        // Adder 1
        fourByteAdder.io.a(1)       := io.Rs1(15 , 8)
        fourByteAdder.io.b(1)       := io.Rs2(15 , 8)
        fourByteAdder.io.carryIn(1) := 0.U
        sumWires(1)                 := fourByteAdder.io.sum(1)
        //Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U  
        sumWires(2)                 := fourByteAdder.io.sum(2)
        // Adder 3
        fourByteAdder.io.a(3)       := io.Rs1(31 , 24)
        fourByteAdder.io.b(3)       := io.Rs2(31 , 24)
        fourByteAdder.io.carryIn(3) := 0.U
        sumWires(3)                 := fourByteAdder.io.sum(3)
        // Concatenate sum from four adders                     
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := io.vxsat_in  
        //There is no overflow information. Status register is left untouched.

    //===============================================
    //PAADD.B -- SIMD 8-bit Signed Averaging Addition
    //===============================================    
    }.elsewhen(io.operation === ALUops.PAADDB) {            
        // Loop through each 8-bit segment (0 to 3) and assign Rs1 and Rs2 parts
        for (i <- 0 until 4) {
            fourByteAdder.io.a(i)       := Cat(io.Rs1(i*8+7) , io.Rs1((i*8+7) , (i*8+0))) // input 8bit signed num is concatenated with MSB of 8bit input
            fourByteAdder.io.b(i)       := Cat(io.Rs2(i*8+7) , io.Rs2((i*8+7) , (i*8+0)))
            fourByteAdder.io.carryIn(i) := 0.U
            sumWires(i)                 := fourByteAdder.io.sum(i)
        }
        // Concatenate just the upper 8 bits which will take care of the shift right operation
        io.Rd        := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat_out := io.vxsat_in  
        //There is no overflow information. Status register is left untouched and hence the overflow flag as well.

    //==================================================
    //PAADDU.B -- SIMD 8-bit Unsigned Averaging Addition
    //==================================================                
    }.elsewhen(io.operation === ALUops.PAADDUB) {    
        for (i <- 0 until 4) {
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0)) // 8bit Unsigned Input from Rs1 given to 9bit Unsigned adder input port a 
            fourByteAdder.io.b(i)       := io.Rs2((i*8+7) , (i*8+0)) // 8bit Unsigned Input from Rs2 given to 9bit Unsigned adder input port b
            fourByteAdder.io.carryIn(i) := 0.U     
            sumWires(i)                 := fourByteAdder.io.sum(i)      
        }
        // Concatenate just the upper 8 bits which will take care of the shift right operation
        io.Rd        := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat_out := io.vxsat_in     // Status register 
        //There is no overflow information. Status register left untouched
    
    //==================================================
    //PSADDU.B -- SIMD 8Bit Unsigned Saturating Addition
    //==================================================
    }.elsewhen(io.operation === ALUops.PSADDUB) {
        for (i <- 0 until 4) {
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0)) // 8bit Unsigned Input from Rs1 given to 9bit Unsigned adder input port a 
            fourByteAdder.io.b(i)       := io.Rs2((i*8+7) , (i*8+0)) // 8bit Unsigned Input from Rs2 given to 9bit Unsigned adder input port b
            fourByteAdder.io.carryIn(i) := 0.U 

            when(fourByteAdder.io.sum(i)(8) === 1.U) {
                sumWires(i)         := 255.U
                overflowDetected(i) := true.B           // Set overflow flag 
            }.otherwise {
                sumWires(i)         := fourByteAdder.io.sum(i)
                overflowDetected(i) := false.B         // Unset overflow flag

            } 
        }
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
       // io.vxsat_out := Cat(io.vxsat_in(31, 1), io.vxsat_in(0) | overflowDetected)  // logically OR the incoming overflow information (vxsat_in(0)) with the new overflowDetected result
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3))

    //===============================
    //PSUB.B -- SIMD 8Bit Subtraction
    //===============================
    }.elsewhen(io.operation === ALUops.PSUBB) {
        twosComplement.io.widthSel := false.B
        for(i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0))    // No concat cuz i think since only 8bit result is needed. Whether signed or unsigned, the user knows the values
            twosComplement.io.input(i)  := Cat(io.Rs2(i*8+7) , io.Rs2((i*8+7) , (i*8+0)))
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)
            sumWires(i)                 := fourByteAdder.io.sum(i)
        }
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched
    
    //=================================================
    //PASUB.B -- SIMD 8Bit Signed Averaging Subtraction
    //=================================================
    }.elsewhen(io.operation === ALUops.PASUBB) {
        twosComplement.io.widthSel := false.B
        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := Cat(io.Rs1(i*8+7) , io.Rs1((i*8+7) , (i*8+0)))   //Concat done here cuz in right shifting sign bits must be preserved.
            twosComplement.io.input(i)  := Cat(io.Rs2(i*8+7) , io.Rs2((i*8+7) , (i*8+0)))
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)
            sumWires(i)                 := fourByteAdder.io.sum(i)
        }
        
        io.Rd        := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched

    //====================================================
    //PASUBU.B -- SIMD 8Bit Unsigned Averaging Subtraction
    //====================================================
    }.elsewhen(io.operation === ALUops.PASUBUB) {
        twosComplement.io.widthSel := false.B
        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0))    // No concat cuz unsigned. Zero gets padded anyway due to 9.W
            twosComplement.io.input(i)  := Cat(0.U , io.Rs2((i*8+7) , (i*8+0)))
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)
            sumWires(i)                 := fourByteAdder.io.sum(i)
        }
        io.Rd        := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched

    //====================================================
    //PSSUB.B -- SIMD 8Bit Signed Saturating Subtraction
    //====================================================
    }.elsewhen(io.operation === ALUops.PSSUBB) {
        twosComplement.io.widthSel := false.B
        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := Cat(io.Rs1(i*8+7) , io.Rs1((i*8+7) , (i*8+0)))
            twosComplement.io.input(i)  := Cat(io.Rs2(i*8+7) , io.Rs2((i*8+7) , (i*8+0)))
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)

            when((fourByteAdder.io.sum(i)).asSInt < -128.S) { 
                sumWires(i)         := (-128.S(9.W)).asUInt       // saturate the result
                overflowDetected(i) := true.B           // Set overflow flag 
            }.elsewhen((fourByteAdder.io.sum(i)).asSInt > 127.S) {
                sumWires(i)         := (127.S(9.W)).asUInt
                overflowDetected(i) := true.B           // Set overflow flag 
            }.otherwise {
                sumWires(i)         := fourByteAdder.io.sum(i)
                overflowDetected(i) := false.B
            }
        }
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3))  // XLEN concatenated with OV flag to form 32bit Status register, vxsat

    //=====================================================
    //PSSUBU.B -- SIMD 8Bit Unsigned Saturating Subtraction
    //===================================================== 
    }.elsewhen(io.operation === ALUops.PSSUBUB) {
        twosComplement.io.widthSel := false.B
        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0))
            twosComplement.io.input(i)  := Cat(0.U , io.Rs2((i*8+7) , (i*8+0)))
            fourByteAdder.io.b(i)       := twosComplement.io.output(i) 

            when((fourByteAdder.io.sum(i)).asSInt < 0.S) {    // Result is in 2'complement. Comparing in Signed form  
                sumWires(i)         := 0.U       // Saturate the result
                overflowDetected(i) := true.B           // Set overflow flag 
            }.otherwise {
                sumWires(i)         := fourByteAdder.io.sum(i)      //else get the sum 
                overflowDetected(i) := false.B
            }
        }
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3))  // XLEN concatenated with OV flag to form 32bit Status register, vxsat
        
        //====================================================16 Bit Operations====================================================// 
    
    //==============================
    //PADD.H -- SIMD 16-bit Addition
    //==============================
    }.elsewhen(io.operation === ALUops.PADDH) {
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(0)
        // Adder 1
        fourByteAdder.io.a(1)       := io.Rs1(15 , 8)
        fourByteAdder.io.b(1)       := io.Rs2(15 , 8)
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        sumWires(1)                 := fourByteAdder.io.sum(1)
        //Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U  
        sumWires(2)                 := fourByteAdder.io.sum(2)
        // Adder 3
        fourByteAdder.io.a(3)       := io.Rs1(31 , 24)
        fourByteAdder.io.b(3)       := io.Rs2(31 , 24)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(3)                 := fourByteAdder.io.sum(3)

        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched
    
    //================================================
    //PAADD.H -- SIMD 16-bit Signed Averaging Addition
    //================================================    
    }.elsewhen(io.operation === ALUops.PAADDH) {
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(0)       
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))
        fourByteAdder.io.b(1)       := Cat(io.Rs2(15) , io.Rs2(15 , 8))
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        sumWires(1)                 := fourByteAdder.io.sum(1)
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(2)       
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))
        fourByteAdder.io.b(3)       := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(3)                 := fourByteAdder.io.sum(3)

        io.Rd        := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched

    //===================================================
    //PAADDU.H -- SIMD 16-bit Unsigned Averaging Addition
    //===================================================
    }.elsewhen(io.operation === ALUops.PAADDUH) {
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(0)
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(0.U , io.Rs1(15 , 8))
        fourByteAdder.io.b(1)       := Cat(0.U , io.Rs2(15 , 8))
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        sumWires(1)                 := fourByteAdder.io.sum(1)
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(2)
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(0.U , io.Rs1(31 , 24))
        fourByteAdder.io.b(3)       := Cat(0.U , io.Rs2(31 , 24))
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(3)                 := fourByteAdder.io.sum(3)

        io.Rd        := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched

    //===================================================
    //PSADD.H -- SIMD 16-bit Signed Saturating Addition
    //===================================================    
    }.elsewhen(io.operation === ALUops.PSADDH) {
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))
        fourByteAdder.io.b(1)       := Cat(io.Rs2(15) , io.Rs2(15 , 8))
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))
        fourByteAdder.io.b(3)       := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)

        when((fourByteAdder.io.sum(1)).asSInt < -128.S) { 
            sumWires(0)         := 0.U       // saturate the result
            sumWires(1)         := (-128.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag 
        }.elsewhen((fourByteAdder.io.sum(1)).asSInt > 127.S) {
            sumWires(0)         := 255.U
            sumWires(1)         := (127.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(0)
            sumWires(1)         := fourByteAdder.io.sum(1)
            overflowDetected(1) := false.B         // Unset overflow flag
        }
 
        when((fourByteAdder.io.sum(3)).asSInt < -128.S) {
            sumWires(2)         := 0.U
            sumWires(3)         := (127.S(9.W)).asUInt
            overflowDetected(3) := true.B           // Set overflow flag 
        }.elsewhen((fourByteAdder.io.sum(3)).asSInt > 127.S) {
            sumWires(2)         := 255.U
            sumWires(3)         := (127.S(9.W)).asUInt
            overflowDetected(3) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(2)         := fourByteAdder.io.sum(2)
            sumWires(3)         := fourByteAdder.io.sum(3)
            overflowDetected(3) := false.B         // Unset overflow flag
        }

        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3)) 
 
    
    //====================================================
    //PSADDU.H -- SIMD 16-bit Unsigned Saturating Addition
    //====================================================
    }.elsewhen(io.operation === ALUops.PSADDUH) {
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(0.U , io.Rs1(15 , 8))
        fourByteAdder.io.b(1)       := Cat(0.U , io.Rs2(15 , 8))
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(0.U , io.Rs1(31 , 24))
        fourByteAdder.io.b(3)       := Cat(0.U , io.Rs2(31 , 24))
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)

        when(fourByteAdder.io.sum(1)(8) === 1.U) {
            sumWires(0)         := 255.U
            sumWires(1)         := 255.U
            overflowDetected(1) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(0)
            sumWires(1)         := fourByteAdder.io.sum(1)
            overflowDetected(1) := false.B         // Unset overflow flag
        }

        when(fourByteAdder.io.sum(3)(8) === 1.U) {
            sumWires(2)         := 255.U
            sumWires(3)         := 255.U
            overflowDetected(3) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(2)         := fourByteAdder.io.sum(2)
            sumWires(3)         := fourByteAdder.io.sum(3)
            overflowDetected(3) := false.B         // Unset overflow flag
        } 
        
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3))  
 
    
    //===================================
    //PSUB.H -- SIMD 16-bit Subtraction
    //===================================
    }.elsewhen(io.operation === ALUops.PSUBH) {
        // 2x16bit Twos Complement generator selection
        twosComplement.io.widthSel  := true.B
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        twosComplement.io.input(0)  := io.Rs2(7 , 0)    // lower 8 bit sent as is to 16bit Twos Complement generator
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)  // Complemented lower 8bits and a left padded 0 sent to Adder0
        fourByteAdder.io.carryIn(0) := 0.U      // Carryin is 0 for Adder0, since 16bit additions
        sumWires(0)                 := fourByteAdder.io.sum(0)
        // Adder 1
        fourByteAdder.io.a(1)       := io.Rs1(15 , 8)
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))     // Upper 8bits concatenated with MSB in order to get 16bit complement value
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Complemented 9bit value sent to Adder1. But result extracts only 8 bits, thus fine
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)     // Carryout from previous byte result added to next byte. We are interested in 16bit result
        sumWires(1)                 := fourByteAdder.io.sum(1)
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        twosComplement.io.input(2)  := io.Rs2(23 , 16)
        fourByteAdder.io.b(2)       := twosComplement.io.output(2)
        fourByteAdder.io.carryIn(2) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(2)
        // Adder 3
        fourByteAdder.io.a(3)       := io.Rs1(31 , 24)
        twosComplement.io.input(3)  := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.b(3)       := twosComplement.io.output(3)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(3)                 := fourByteAdder.io.sum(3)

        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := io.vxsat_in     // Status register. No extra overflow information coming out of this operation
    
    //===================================================
    //PASUB.H -- SIMD 16-bit Signed Averaging Subtraction
    //===================================================
    }.elsewhen(io.operation === ALUops.PASUBH) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)        // Does not need any concat. For first byte in 16bit, we are not concerned with 9th bit. Adder-a input will be of 9bits though
        twosComplement.io.input(0)  := Cat(io.Rs2(7) , io.Rs2(7 , 0))       // Need concat because complement value is to be obtained
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)      // lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class). Left padded 0 in order to preserve any carry. 
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(0)      // carryout , lower 8bit 2's complement result = 9bit output. But after s>>1, (7,1) bits are needed of this result 
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))     // Sign Extension for first input in order to get SE17
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))     // Sign Extended second input sent to 16bit complement generator
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class).
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)     // Carry from previous byte
        sumWires(1)                 := fourByteAdder.io.sum(1)      // 9bit sum value in 2's complement form. All 9bits needed after s>>1
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        twosComplement.io.input(2)  := Cat(io.Rs2(23) , io.Rs2(23 , 16))
        fourByteAdder.io.b(2)       := twosComplement.io.output(2)
        fourByteAdder.io.carryIn(2) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(2)
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))
        twosComplement.io.input(3)  := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.b(3)       := twosComplement.io.output(3)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(3)                 := fourByteAdder.io.sum(3)

        io.Rd        := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))
        io.vxsat_out := io.vxsat_in     // Status register. No extra overflow information coming out of this operation
    
    //======================================================
    //PASUBU.H -- SIMD 16-bit Unsigned Averaging Subtraction
    //======================================================    
    }.elsewhen(io.operation === ALUops.PASUBUH) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        twosComplement.io.input(0)  := Cat(0.U , io.Rs2(7 , 0))     // Zero concat in order to get complemented value for unsigned subtraction
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)      // lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(0)      // carryout , lower 8bit 2's complement result = 9bit output
        // Adder 1
        fourByteAdder.io.a(1)       := io.Rs1(15 , 8)
        twosComplement.io.input(1)  := Cat(0.U , io.Rs2(15 , 8))
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)     
        sumWires(1)                 := fourByteAdder.io.sum(1)
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        twosComplement.io.input(2)  := Cat(0.U , io.Rs2(23 , 16))
        fourByteAdder.io.b(2)       := twosComplement.io.output(2)
        fourByteAdder.io.carryIn(2) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(2)
        // Adder 3
        fourByteAdder.io.a(3)       := io.Rs1(31 , 24)
        twosComplement.io.input(3)  := Cat(0.U , io.Rs2(31 , 24))
        fourByteAdder.io.b(3)       := twosComplement.io.output(3)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(3)                 := fourByteAdder.io.sum(3)

        io.Rd        := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))
        io.vxsat_out := io.vxsat_in     // Status register.  No extra overflow information coming out of this operation

    //======================================================
    //PSSUB.H -- SIMD 16-bit Signed Saturating Subtraction
    //====================================================== 
    }.elsewhen(io.operation === ALUops.PSSUBH) {
       twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        twosComplement.io.input(0)  := Cat(io.Rs2(7) , io.Rs2(7 , 0))       // Need concat because complement value is to be obtained
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)      // lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class). Left padded 0 in order to preserve any carry. 
        fourByteAdder.io.carryIn(0) := 0.U
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)     
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        twosComplement.io.input(2)  := Cat(io.Rs2(23) , io.Rs2(23 , 16))
        fourByteAdder.io.b(2)       := twosComplement.io.output(2)
        fourByteAdder.io.carryIn(2) := 0.U
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))
        twosComplement.io.input(3)  := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.b(3)       := twosComplement.io.output(3)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)

         when((fourByteAdder.io.sum(1)).asSInt < -128.S) {                     // Saturating condition for lower 16bit result
            sumWires(0)         := 0.U    // saturate the result
            sumWires(1)         := (-128.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag 
        }.elsewhen((fourByteAdder.io.sum(1)).asSInt > 127.S) {
            sumWires(0)         := 255.U     // saturate the result
            sumWires(1)         := (127.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(0)
            sumWires(1)         := fourByteAdder.io.sum(1)
            overflowDetected(1) := false.B           // Unset overflow flag 
        }
 
        when((fourByteAdder.io.sum(3)).asSInt < -128.S) {                      // Saturating condition for lower 16bit result
            sumWires(2)         := 0.U
            sumWires(3)         := (-128.S(9.W)).asUInt
            overflowDetected(3) := true.B           // Set overflow flag 
        }.elsewhen((fourByteAdder.io.sum(3)).asSInt > 127.S) {
            sumWires(2)         := 255.U
            sumWires(3)         := (127.S(9.W)).asUInt
            overflowDetected(3) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(2)         := fourByteAdder.io.sum(2)
            sumWires(3)         := fourByteAdder.io.sum(3)
            overflowDetected(3) := false.B           // Unset overflow flag
        }

        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3)) 

    //=======================================================
    //PSSUBU.H -- SIMD 16-bit Unsigned Saturating Subtraction
    //=======================================================    
    }.elsewhen(io.operation === ALUops.PSSUBUH) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        twosComplement.io.input(0)  := Cat(0.U , io.Rs2(7 , 0))       // Need concat because complement value is to be obtained
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)      // lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class). Left padded 0 in order to preserve any carry. 
        fourByteAdder.io.carryIn(0) := 0.U
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(0.U , io.Rs1(15 , 8))        // *****CONCAT DOES NOT SEEM TO BE NEEDED. CHECK WHEN WRITING THE TESTS*****
        twosComplement.io.input(1)  := Cat(0.U , io.Rs2(15 , 8))
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)    
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        twosComplement.io.input(2)  := Cat(0.U , io.Rs2(23 , 16))
        fourByteAdder.io.b(2)       := twosComplement.io.output(2)
        fourByteAdder.io.carryIn(2) := 0.U
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(0.U , io.Rs1(31 , 24))
        twosComplement.io.input(3)  := Cat(0.U , io.Rs2(31 , 24))
        fourByteAdder.io.b(3)       := twosComplement.io.output(3)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2) 

        when((fourByteAdder.io.sum(1)).asSInt < 0.S) {      // Saturating condition for lower 16bit result
            sumWires(0)         := 0.U
            sumWires(1)         := 0.U
            overflowDetected(1) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(0)
            sumWires(1)         := fourByteAdder.io.sum(1)
            overflowDetected(1) := false.B           // Unset overflow flag
        }

        when((fourByteAdder.io.sum(3)).asSInt < 0.S) {      // Saturating condition for upper 16bit result
            sumWires(2)         := 0.U
            sumWires(3)         := 0.U
            overflowDetected(3) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(2)         := fourByteAdder.io.sum(2)
            sumWires(3)         := fourByteAdder.io.sum(3)
            overflowDetected(3) := false.B           // Unset overflow flag
        } 
        
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3))  
    
    //==================================================
    //PAS.HX -- SIMD 16-bit Cross Addition & Subtraction
    //================================================== 
    }.elsewhen(io.operation === ALUops.PASHX) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0      ===ADDITION===
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(0)
        // Adder 1
        fourByteAdder.io.a(1)       := io.Rs1(31 , 24)
        fourByteAdder.io.b(1)       := io.Rs2(15 , 8)
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        sumWires(3)                 := fourByteAdder.io.sum(1)
       // Adder 2       ===SUBTRACTION===
       fourByteAdder.io.a(2)       := io.Rs1(7 , 0)
       twosComplement.io.input(2)  := io.Rs2(23 , 16)
       fourByteAdder.io.b(2)       := twosComplement.io.output(2)
       fourByteAdder.io.carryIn(2) := 0.U
       sumWires(0)                 := fourByteAdder.io.sum(2)
       // Adder 3
       fourByteAdder.io.a(3)       := io.Rs1(15 , 8)
       twosComplement.io.input(3)  := Cat(io.Rs2(31) , io.Rs2(31 , 24))
       fourByteAdder.io.b(3)       := twosComplement.io.output(3)
       fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
       sumWires(1)                 := fourByteAdder.io.sum(3)

       io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
       io.vxsat_out := io.vxsat_in     // Status register

    //===================================================================
    //PAAS.HX -- SIMD 16-bit Signed Averaging Cross Addition & Subtraction
    //===================================================================
    }.elsewhen(io.operation === ALUops.PAASHX) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0          ===ADDITION===
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(0)       
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))
        fourByteAdder.io.b(1)       := Cat(io.Rs2(15) , io.Rs2(15 , 8))
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        sumWires(3)                 := fourByteAdder.io.sum(1)
        // Adder 2          ===SUBTRACTION===
        fourByteAdder.io.a(2)       := io.Rs1(7 , 0)
        twosComplement.io.input(2)  := Cat(io.Rs2(23) , io.Rs2(23 , 16))
        fourByteAdder.io.b(2)       := twosComplement.io.output(2)
        fourByteAdder.io.carryIn(2) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(2)
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))
        twosComplement.io.input(3)  := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.b(3)       := twosComplement.io.output(3)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(1)                 := fourByteAdder.io.sum(3)

        io.Rd        := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))
        io.vxsat_out := io.vxsat_in     // Status register. No extra overflow information coming out of this operation

    //=====================================================================
    //PSAS.HX -- SIMD 16-bit Signed Saturating Cross Addition & Subtraction
    //=====================================================================
    }.elsewhen(io.operation === ALUops.PSASHX) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0          ===ADDITION===
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))
        fourByteAdder.io.b(1)       := Cat(io.Rs2(15) , io.Rs2(15 , 8))
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        // Adder 2          ===SUBTRACTION===
        fourByteAdder.io.a(2)       := io.Rs1(7 , 0)
        twosComplement.io.input(2)  := Cat(io.Rs2(23) , io.Rs2(23 , 16))
        fourByteAdder.io.b(2)       := twosComplement.io.output(2)
        fourByteAdder.io.carryIn(2) := 0.U
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))
        twosComplement.io.input(3)  := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.b(3)       := twosComplement.io.output(3)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)

        when((fourByteAdder.io.sum(1)).asSInt < -128.S) { 
            sumWires(2)         := 0.U       // saturate the result
            sumWires(3)         := (-128.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag 
        }.elsewhen((fourByteAdder.io.sum(1)).asSInt > 127.S) {
            sumWires(2)         := 255.U
            sumWires(3)         := (127.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(2)         := fourByteAdder.io.sum(0)
            sumWires(3)         := fourByteAdder.io.sum(1)
            overflowDetected(1) := false.B         // Unset overflow flag
        }

        when((fourByteAdder.io.sum(3)).asSInt < -128.S) {                      
            sumWires(0)         := 0.U
            sumWires(1)         := (-128.S(9.W)).asUInt
            overflowDetected(3) := true.B           // Set overflow flag 
        }.elsewhen((fourByteAdder.io.sum(3)).asSInt > 127.S) {
            sumWires(0)         := 255.U
            sumWires(1)         := (127.S(9.W)).asUInt
            overflowDetected(3) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(2)
            sumWires(1)         := fourByteAdder.io.sum(3)
            overflowDetected(3) := false.B           // Unset overflow flag
        }

        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3)) 
    
    //===================================================
    //PSA.HX -- SIMD 16-bit Cross Subtraction & Addition
    //===================================================
    }.elsewhen(io.operation === ALUops.PSAHX) {
        twosComplement.io.widthSel  := true.B
        // Adder 0          ===SUBTRACTION===
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)
        twosComplement.io.input(0)  := io.Rs2(7 , 0)    // lower 8 bit sent as is to 16bit Twos Complement generator
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)  // Complemented lower 8bits and a left padded 0 sent to Adder0
        fourByteAdder.io.carryIn(0) := 0.U      // Carryin is 0 for Adder0, since 16bit additions
        sumWires(2)                 := fourByteAdder.io.sum(0)
        // Adder 1
        fourByteAdder.io.a(1)       := io.Rs1(31 , 24)
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))     // Upper 8bits concatenated with MSB in order to get 16bit complement value
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Complemented 9bit value sent to Adder1. But result extracts only 8 bits, thus fine
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)     // Carryout from previous byte result added to next byte. We are interested in 16bit result
        sumWires(3)                 := fourByteAdder.io.sum(1)
        //Adder 2           ===ADDITION===
        fourByteAdder.io.a(2)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U  
        sumWires(0)                 := fourByteAdder.io.sum(2)
        // Adder 3
        fourByteAdder.io.a(3)       := io.Rs1(15 , 8)
        fourByteAdder.io.b(3)       := io.Rs2(31 , 24)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(1)                 := fourByteAdder.io.sum(3)

        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched
    
    //===================================================================
    //PASA.HX -- SIMD 16-bit Signed Averaging Cross Subtraction & Addition
    //===================================================================
    }.elsewhen(io.operation === ALUops.PASAHX) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder0           ===SUBTRACTION===
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)        // Does not need any concat. For first byte in 16bit, we are not concerned with 9th bit. Adder-a input will be of 9bits though
        twosComplement.io.input(0)  := Cat(io.Rs2(7) , io.Rs2(7 , 0))       // Need concat because complement value is to be obtained
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)      // lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class). Left padded 0 in order to preserve any carry. 
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(0)      // carryout , lower 8bit 2's complement result = 9bit output. But after s>>1, (7,1) bits are needed of this result 
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))     // Sign Extension for first input in order to get SE17
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))     // Sign Extended second input sent to 16bit complement generator
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class).
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)     // Carry from previous byte
        sumWires(3)                 := fourByteAdder.io.sum(1)      // 9bit sum value in 2's complement form. All 9bits needed after s>>1
        // Adder 2          ===ADDITION===
        fourByteAdder.io.a(2)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(2)       
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))
        fourByteAdder.io.b(3)       := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(1)                 := fourByteAdder.io.sum(3)

        io.Rd        := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched
    
    //=====================================================================
    //PSSA.HX -- SIMD 16-bit Signed Saturating Cross Subtraction & Addition
    //=====================================================================
    }.elsewhen(io.operation === ALUops.PSSAHX) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)
        twosComplement.io.input(0)  := Cat(io.Rs2(7) , io.Rs2(7 , 0))       // Need concat because complement value is to be obtained
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)      // lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class). Left padded 0 in order to preserve any carry. 
        fourByteAdder.io.carryIn(0) := 0.U
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)  
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))
        fourByteAdder.io.b(3)       := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)

        when((fourByteAdder.io.sum(1)).asSInt < -128.S) {                     // Saturating condition for lower 16bit result
            sumWires(2)         := 0.U    // saturate the result
            sumWires(3)         := (-128.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag 
        }.elsewhen((fourByteAdder.io.sum(1)).asSInt > 127.S) {
            sumWires(2)         := 255.U     // saturate the result
            sumWires(3)         := (127.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(2)         := fourByteAdder.io.sum(0)
            sumWires(3)         := fourByteAdder.io.sum(1)
            overflowDetected(1) := false.B           // Unset overflow flag 
        }

        when((fourByteAdder.io.sum(3)).asSInt < -128.S) {
            sumWires(0)         := 0.U
            sumWires(1)         := (127.S(9.W)).asUInt
            overflowDetected(3) := true.B           // Set overflow flag 
        }.elsewhen((fourByteAdder.io.sum(3)).asSInt > 127.S) {
            sumWires(0)         := 255.U
            sumWires(1)         := (127.S(9.W)).asUInt
            overflowDetected(3) := true.B           // Set overflow flag 
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(2)
            sumWires(1)         := fourByteAdder.io.sum(3)
            overflowDetected(3) := false.B         // Unset overflow flag
        }
        
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3)) 
    
    }
}
 
object ALUMain extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new PextALU(), Array("--target-dir", "generated"))
}

// Wrapper Module for ALU Testing
class PextALUWrapper extends Module {
  val io = IO(new Bundle {
    val Rs1       = Input(UInt(32.W))
    val Rs2       = Input(UInt(32.W))
    val operation = Input(ALUops())
    val Rd        = Output(UInt(32.W))
    val vxsat     = Output(UInt(32.W))
  })

  // Simulated vxsat register (32-bit)
  val vxsat = RegInit(0.U(32.W))     // Default value is 0
  vxsat := 0.U


  // Instantiate the ALU
  val alu = Module(new PextALU)

  // Connections to the ALU
  alu.io.Rs1 := io.Rs1
  alu.io.Rs2 := io.Rs2
  alu.io.operation := io.operation
  alu.io.vxsat_in := vxsat

  io.Rd := alu.io.Rd

  // Update the simulated vxsat register
  vxsat := alu.io.vxsat_out

  io.vxsat := vxsat
}

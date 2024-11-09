import chisel3._
import chisel3.util._
//import Bits8._
//import scala.annotation.switch
//import chisel3.experimental.ChiselEnum

// Defining ALU operations as enumeration type using ChiselEnum
object ALUops extends ChiselEnum {
    val PADDB,PAADDB,PAADDUB,PADDW,PSADDUB , PSUBB,PASUBB,PASUBUB,PSSUBB,PSSUBUB , PADDH,PAADDH,PAADDUH,PSADDH,PSADDUH  = Value
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

//=========================================
// 4x8bit Two's Complement generator module
//=========================================
class TwosComplementGenerator extends Module {      // width will be fixed cuz we will only need 8 bit adders even to do 16bit arithmetics
    val io =IO(new Bundle {
        val input  = Input(Vec(4, UInt(9.W)))
        val output = Output(Vec(4, UInt(9.W))) 
    })

    val complementValue = Wire(Vec(4,UInt(9.W)))
    // Function of class TwosComplementGenerator 
    for (m <- 0 until 4) {
        complementValue(m) := ~(io.input(m)) + 1.U     // One's complement of operand B is added 1 to get 2's complement. Concatenation 0 is done to get correct 2's complement for values greater than 127
        io.output(m)       := complementValue(m)
    }  
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
        val vxsat     = Output(UInt(1.W))
    })

    // Instantiation
    val fourByteAdder  = Module(new AdderALU())
    val twosComplement = Module(new TwosComplementGenerator())
    val sumWires       = Wire(Vec(4, UInt(9.W))) 
    val vxsatOV        = RegInit(0.U(1.W))

    // Default values
    fourByteAdder.io.a       := VecInit(Seq.fill(4)(0.U(9.W)))
    fourByteAdder.io.b       := VecInit(Seq.fill(4)(0.U(9.W)))
    fourByteAdder.io.carryIn := VecInit(Seq.fill(4)(0.U(1.W)))
    twosComplement.io.input  := VecInit(Seq.fill(4)(0.U(9.W)))
    sumWires := VecInit(Seq.fill(4)(0.U(9.W)))
    io.Rd    := 0.U
    io.vxsat := 0.U

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
        io.Rd    := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat := vxsatOV  // Overflow flag

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
        io.Rd    := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat := vxsatOV  // Overflow flag

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
        io.Rd    := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat := vxsatOV  // Overflow flag  
    
    //==================================================
    //PSADDU.B -- SIMD 8Bit Unsigned Saturating Addition
    //==================================================
    }.elsewhen(io.operation === ALUops.PSADDUB) {
        for (i <- 0 until 4) {
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0)) // 8bit Unsigned Input from Rs1 given to 9bit Unsigned adder input port a 
            fourByteAdder.io.b(i)       := io.Rs2((i*8+7) , (i*8+0)) // 8bit Unsigned Input from Rs2 given to 9bit Unsigned adder input port b
            fourByteAdder.io.carryIn(i) := 0.U 

            when(fourByteAdder.io.sum(i)(8) === 1.U) {
                sumWires(i) := 255.U
                vxsatOV     := 1.U 
            }.otherwise {
                sumWires(i) := fourByteAdder.io.sum(i)
            } 
        }
        io.Rd    := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat := vxsatOV  // Overflow flag 

    //===============================
    //PSUB.B -- SIMD 8Bit Subtraction
    //===============================
    }.elsewhen(io.operation === ALUops.PSUBB) {
        for(i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0))
            twosComplement.io.input(i)  := Cat(io.Rs2(i*8+7) , io.Rs2((i*8+7) , (i*8+0)))
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)
            sumWires(i)                 := fourByteAdder.io.sum(i)
        }
        io.Rd    := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat := vxsatOV
    
    //=================================================
    //PASUB.B -- SIMD 8Bit Signed Averaging Subtraction
    //=================================================
    }.elsewhen(io.operation === ALUops.PASUBB) {
        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := Cat(io.Rs2(i*8+7) , io.Rs1((i*8+7) , (i*8+0)))
            twosComplement.io.input(i)  := Cat(io.Rs2(i*8+7) , io.Rs2((i*8+7) , (i*8+0)))
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)
            sumWires(i)                 := fourByteAdder.io.sum(i)
        }
        
        io.Rd    := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat := vxsatOV

    //====================================================
    //PASUBU.B -- SIMD 8Bit Unsigned Averaging Subtraction
    //====================================================
    }.elsewhen(io.operation === ALUops.PASUBUB) {
        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0))
            twosComplement.io.input(i)  := Cat(0.U , io.Rs2((i*8+7) , (i*8+0)))
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)
            sumWires(i)                 := fourByteAdder.io.sum(i)
        }
        io.Rd    := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat := vxsatOV

    //==================================================
    //PSSUB.B -- SIMD 8Bit Signed Saturating Subtraction
    //================================================== 
    }.elsewhen(io.operation === ALUops.PSSUBB) {
        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := Cat(io.Rs2(i*8+7) , io.Rs1((i*8+7) , (i*8+0)))
            twosComplement.io.input(i)  := Cat(io.Rs2(i*8+7) , io.Rs1((i*8+7) , (i*8+0)))
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)

            when((fourByteAdder.io.sum(i)).asSInt < -128.S) { 
                sumWires(i) := (-128.S(9.W)).asUInt       // saturate the result
                vxsatOV     := 1.U 
            }.elsewhen((fourByteAdder.io.sum(i)).asSInt > 127.S) {
                sumWires(i) := (127.S(9.W)).asUInt
                vxsatOV     := 1.U
            }.otherwise {
                sumWires(i) := fourByteAdder.io.sum(i)
            }
        }
        io.Rd    := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat := vxsatOV  // Overflow flag 

    //=====================================================
    //PSSUBU.B -- SIMD 8Bit Unsigned Saturating Subtraction
    //===================================================== 
    }.elsewhen(io.operation === ALUops.PSSUBUB) {
        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0))
            twosComplement.io.input(i)  := Cat(0.U , io.Rs1((i*8+7) , (i*8+0)))
            fourByteAdder.io.b(i)       := twosComplement.io.output(i) 

            when((fourByteAdder.io.sum(i)).asSInt < 0.S) {    // Result is in 2'complement. Comparing in Signed form  
                sumWires(i) := 0.U       // Saturate the result
                vxsatOV     := 1.U       // Set OV flag
            }.otherwise {
                sumWires(i) := fourByteAdder.io.sum(i)      //else get the sum 
            }
        }
        io.Rd    := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat := vxsatOV  // Overflow flag 
        
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

        io.Rd := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
    
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

        io.Rd := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))

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

        io.Rd := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))

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
            sumWires(0) := (-128.S(9.W)).asUInt       // saturate the result
            sumWires(1) := (-128.S(9.W)).asUInt
            vxsatOV     := 1.U // Rewrite the overflow information code
        }.elsewhen((fourByteAdder.io.sum(1)).asSInt > 127.S) {
            sumWires(0) := (127.S(9.W)).asUInt
            sumWires(1) := (127.S(9.W)).asUInt
            vxsatOV     := 1.U  // Rewrite the overflow information code
        }.otherwise {
            sumWires(0)  := fourByteAdder.io.sum(0)
            sumWires(1)  := fourByteAdder.io.sum(1)
        }
 
        when((fourByteAdder.io.sum(3)).asSInt < -128.S) {
            sumWires(2) := (127.S(9.W)).asUInt
            sumWires(3) := (127.S(9.W)).asUInt
        }.elsewhen((fourByteAdder.io.sum(3)).asSInt > 127.S) {
            sumWires(2) := (127.S(9.W)).asUInt
            sumWires(3) := (127.S(9.W)).asUInt
            vxsatOV     := 1.U  // Rewrite the overflow information code
        }.otherwise {
            sumWires(0)  := fourByteAdder.io.sum(0)
            sumWires(1)  := fourByteAdder.io.sum(1)
        }

        io.Rd := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
    
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
            sumWires(0) := 255.U
            sumWires(1) := 255.U
            vxsatOV     := 1.U  // Rewrite the overflow information
        }.otherwise {
            sumWires(0) := fourByteAdder.io.sum(0)
            sumWires(1) := fourByteAdder.io.sum(1)
        }

        when(fourByteAdder.io.sum(3)(8) === 1.U) {
            sumWires(2) := 255.U
            sumWires(3) := 255.U
            vxsatOV     := 1.U  // Rewrite the overflow information
        }.otherwise {
            sumWires(2) := fourByteAdder.io.sum(0)
            sumWires(3) := fourByteAdder.io.sum(1)
        } 
        
        io.Rd := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
    
    //===================================
    //PSUB.H -- SIMD 16-bit Subtraction
    //===================================
    }
}
 
object ALUMain extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new PextALU(), Array("--target-dir", "generated"))
}
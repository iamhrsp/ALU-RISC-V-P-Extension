package Bits8

import chisel3._
import chisel3.util._

 ///============================================N-bit Adder generator========================================///
/* -This is an Nbit Adder generator whose Input and Output types are decided based on the instruction it is used in. The port types are assigned when this module is instantiated in 
    different instructions.
   - Addition is performed on unsigned values irrespective of whether the inputs are signed or unsigned since internally all the bits undergo mere addition.
*/
class Adder[T <: Data](width: Int, gen: T) extends Module {
    val io = IO(new Bundle {
      val a    = Input(gen.cloneType) // Allow signed or unsigned input. 
      val b    = Input(gen.cloneType) 
      val cin  = Input(UInt(1.W))     // Carry-in input
      val sum  = Output(gen.cloneType)
      val cout = Output(UInt(1.W))     // Carry-out output
    })
  
    // Inputs converted to UInt for addition, regardless of whether they are signed or unsigned cuz internally binary addition simply adds the bits.
    val a_asUInt = io.a.asUInt
    val b_asUInt = io.b.asUInt
  
    // Perform the addition
    val result = a_asUInt + b_asUInt + io.cin    // 9 bit sum. bit width is picked up by input widths automatically which must be 9bits
  
    // Convert the result back to the original type (signed or unsigned)
    io.sum  := result.asTypeOf(io.a)    // result attains the type of input a of Adder 
    io.cout := result(width)        // MSB of result i.e., N+1th bit is assigned to carryout output.
  }

///==========================================PADD.B -- SIMD 8bit Addition=========================================///
/* - Four 8bit elements in 32bit Rs1 are added with corresponding four 8bit elements in Rs2. The results are stored in corresponding 8bit elements in Rd.
   - The specification mentions that this instruction is used for signed and unsigned addition.
   - 8bit signed Inputs are fed in 2's complement form. 8bit signed output elements are in 2'complement form
   - No Carry in, Carry out and Overflow information. 
   - Internally PADD.B addition operation is indifferent to whether the given data is signed or unsigned. It simply adds the binary values. 
     The interpretation as signed or unsigned depends on whether the values in the registers are treated as signed or unsigned by the user.
*/
class PADDB extends Module {
    val io = IO(new Bundle {
        val Rs1   = Input(UInt(32.W))
        val Rs2   = Input(UInt(32.W))
        val Rd    = Output(UInt(32.W))
        val vxsat = Output(UInt(1.W))       // vxsat CSR has OV information in LSB. vxsat[XLEN,1] bits are reserved.
    })

    val UA8     = Seq.fill(4)(Module(new Adder(8,UInt(9.W))))  // generate four 8bit unsigned. Internally adder should operate on 9bit data
    val vxsatOV = RegInit(0.U(1.W))        // Reg to store overflow across all clock cycles. Reset value of OV flag is 0.
    //Function of class PAdd8    
    for (x <- 0 until 4) { 
        UA8(x).io.cin    := 0.U    // Carryin not used
        UA8(x).io.a      := io.Rs1((x*8+7) , (x*8+0))     // 8bit elements from 32bit word assigned to their respective adders
        UA8(x).io.b      := io.Rs2((x*8+7) , (x*8+0))      // Data is 8bits wide. Ports are defined as UInt(9.W). Thus 9th bit is set as 0            
    }

    io.Rd := Cat(UA8(3).io.sum(7,0), UA8(2).io.sum(7,0), UA8(1).io.sum(7,0), UA8(0).io.sum(7,0))   // concatenate bit7 to bit0 from each adder to form the output result rd
    /* - Carry out not needed.
       - For signed inputs(in 2's complement form), 9th bit always being zero due to the 9bit wide ports does not affect the result. Since we are interested in lower 8bits only.
         eg: -10 + (-13) = -23 
             0_1111_0110  --> -10 in 2's complement
             0_1111_0011  --> -13 in 2's complement
             1_1110_1001  --> -23 in 2's complement
    */
    io.vxsat := vxsatOV
}


///============================PAADD.B -- SIMD 8-bit Signed Averaging Addition=========================///
/* - The input is given as a 32bit word which contains four 8bit elements. 
   - The user already knows that each 8bit element is a signed number. Hence, if in hex form, the 8bit inputs are fed as 2's complement form
   - Since 8bit signed elements are given to 9bit SInt input ports, Sign Extension to 9bit happens internally.
   - The resulting sum is a 9bit result in 2's complement form.
   - The upper 8bits are extracted and             
   - The final result is obtained by concatenation of four 8bit sum outputs.

   **Up for discussion:
   - type of Rs1, Rs2, Rd. Chosen SInt here just to test the decimal value test case in tester.
*/
class PAADDB extends Module {
    val io = IO(new Bundle{
        val Rs1   = Input(UInt(32.W))
        val Rs2   = Input(UInt(32.W))
        val Rd    = Output(UInt(32.W))
        val vxsat = Output(UInt(1.W))       // vxsat CSR has OV information in LSB. vxsat[XLEN,1] bits are reserved.
    })

    //function of class PAADD8
    val SA8      = Seq.fill(4)(Module(new Adder(8,SInt(9.W))))
    val vxsatOV  = RegInit(0.U(1.W))        // Reg to store overflow across all clock cycles. Reset value of OV flag is 0.
    //val shiftsum = Reg(Vec(4,SInt(9.W)))
    
    for (x <- 0 until 4) {
        SA8(x).io.cin := 0.U // carryin inputs assigned zero. Since not used.
        SA8(x).io.a   :=  io.Rs1((x*8+7) , (x*8+0)).asSInt  // 8bit signed value is given to a 9bit SInt input port, concatenation happens automatically
        SA8(x).io.b   :=  io.Rs2((x*8+7) , (x*8+0)).asSInt

        // s>>1 operation
        //ENABLE IF NEEDED
        // shiftsum acts as a temporary register which holds the 9bit value from shift right arithmetic operation.  
        // shiftsum(x)(8)   := A8(x).io.s(8) // 9th bit wire gets sign bit of the result
        // shiftsum(x)(7,0) := A8(x).io.s(8,1)    // right shift signed-sum-result by 1. Bit8 to Bit1 assigned to lower 8 bits of wire sumshift
    }
    // io.Rd := Cat(shiftsum(3)(7,0), shiftsum(2)(7,0), shiftsum(1)(7,0), shiftsum(0)(7,0))   // concatenate the wires to form rd
    io.Rd := Cat(SA8(3).io.sum(8,1), SA8(2).io.sum(8,1), SA8(1).io.sum(8,1), SA8(0).io.sum(8,1))
    /* - Extraction of upper 8 bits out of total 9bit result takes care of the shift arithmetic right operation.
       - Converting to asSInt is for testing the case of signed decimal input ( eg -8 + (-2) = -5 ) in tester.
    */
    io.vxsat := vxsatOV
}

///==========================================PAADDU.B -- SIMD 8-bit Unsigned Averaging Addition========================================///
class PAADDUB extends Module {
    val io = IO(new Bundle{
        val Rs1   = Input(UInt(32.W))
        val Rs2   = Input(UInt(32.W))
        val Rd    = Output(UInt(32.W)) 
        val vxsat = Output(UInt(1.W))       // vxsat CSR has OV information in LSB. vxsat[XLEN,1] bits are reserved.      
    })

    // function of class PAADD8U
    val UA8      = Seq.fill(4)(Module(new Adder(8,UInt(9.W))))
    val vxsatOV  = RegInit(0.U(1.W))        // Reg to store overflow across all clock cycles. Reset value of OV flag is 0.
    
    for (x <- 0 until 4) {
        UA8(x).io.cin := 0.U // carryin inputs assigned zero. Since not used.
        UA8(x).io.a   :=  io.Rs1((x*8+7) , (x*8+0))     // 8bit unsigned inputs assigned to 9bit adder input. Zero extended to 9bit implicitly. 
        UA8(x).io.b   :=  io.Rs2((x*8+7) , (x*8+0))
    }

     io.Rd := Cat(UA8(3).io.sum(8,1), UA8(2).io.sum(8,1), UA8(1).io.sum(8,1), UA8(0).io.sum(8,1))   
     /* - Addition of 9bit inputs with carryin = 0 takes place in the Unsigned-Adder-8 module
        - Upper 8bits from each adder sum are extracted. This takes care of the shift aritmetic right by 1 operation. 
    */
    io.vxsat := vxsatOV
}

//======================================PSADDU.B -- SIMD 8Bit Unsigned Saturating Addition=================================///
class PSADDUB extends Module{
    val io = IO(new Bundle {
        val Rs1   = Input(UInt(32.W))
        val Rs2   = Input(UInt(32.W))
        val Rd    = Output(UInt(32.W))
        val vxsat = Output(UInt(1.W))       // vxsat CSR has OV information in LSB. vxsat[XLEN,1] bits are reserved.
    })

    //function of Class PSADDUB
    val UA8      = Seq.fill(4)(Module(new Adder(8,UInt(9.W))))
    val vxsatOV  = RegInit(0.U(1.W))        // Reg to store overflow across all clock cycles. Reset value of OV flag is 0. 
    val sWire    = Wire(Vec(4, UInt(8.W)))

    for (x <- 0 until 4) {
        UA8(x).io.cin := 0.U
        UA8(x).io.a   := io.Rs1((x*8+7) , (x*8+0))    // 8bit element from Rs1 assigned to 9bit UInt Input port. Zero extended to 9bits.
        UA8(x).io.b   := io.Rs2((x*8+7) , (x*8+0))    // 8bit element from Rs2 assigned to 9bit UInt Input port. Zero extended to 9bits.
       //tReg := UA8(x).io.sum       // 9 bit result can be stored in temporary register. Need to declare a 32bit temp register and wires then
       
        when(UA8(x).io.sum(8) === 1.U) {    // if 9th bit of sum output i.e., Carryout bit is 1
            sWire(x) := 255.U       // saturate the result
            vxsatOV  := 1.U 
        }.otherwise {
            sWire(x) := UA8(x).io.sum(7,0)      //else get the sum 
        }
    }

    io.Rd    := Cat(sWire(3), sWire(2), sWire(1), sWire(0))    // Concatenate the result into a 32bit word.
    io.vxsat := vxsatOV
}


///==============================================PADD.W -- SIMD 32bit Addition=============================================///
/* - 32bit Rs1 is added with 32bit Rs2. The results are stored in 32bit Rd.
   - Addition is performed with 4x8bit adders.
   - The internal carrys generated are added to appropirate next 8bit data element.
   - No information about Overflow from 32nd bit.
   - The specification mentions this instruction is used for signed and unsigned addition.
   - The PADD32.W instruction is indifferent to whether the data is signed or unsigned. It simply adds the binary values. 
     The interpretation as signed or unsigned depends on whether the values in the registers are treated as signed or unsigned by the user.
*/
class PADDW extends Module {
    val io = IO(new Bundle {
        val Rs1   = Input(UInt(32.W))
        val Rs2   = Input(UInt(32.W))
        val Rd    = Output(UInt(32.W))
        val vxsat = Output(UInt(1.W))       // vxsat CSR has OV information in LSB. vxsat[XLEN,1] bits are reserved.
    })

    val UA8      = Seq.fill(4)(Module(new Adder(8,UInt(9.W))))  // generate four 8bit unsigned adders. Internally adder should operate on 9bit data
    val carryin  = Wire(Vec(5,UInt(1.W)))    // Generate five wires for internal carrys between adders. c0 to c4
    val vxsatOV  = RegInit(0.U(1.W))        // Reg to store overflow across all clock cycles. Reset value of OV flag is 0.
   
    //Function of class PAdd8 
    carryin(0) := 0.U   // initial carry for first Adder(8) 
    for (x <- 0 until 4) {
            
        UA8(x).io.a   := io.Rs1((x*8+7) , (x*8+0))     // 8bit elements from 32bit word assigned to their respective adders
        UA8(x).io.b   := io.Rs2((x*8+7) , (x*8+0))      // Since ports are unsigned, 9th bit is set as 0
        UA8(x).io.cin := carryin(x)      // assigned carryin input for the adder
        carryin(x+1)  := UA8(x).io.cout       // carryout output from current adder assigned to carryin input of next adder             
    }

    io.Rd := Cat(UA8(3).io.sum(7,0), UA8(2).io.sum(7,0), UA8(1).io.sum(7,0), UA8(0).io.sum(7,0))   // concatenate bit7 to bit0 from each adder to form the output result rd
    //cout := carryin(4)    //the carry out from the component to be assined depending on the instruction 
    io.vxsat := vxsatOV
}
//=============================================================SUBTRACTION=============================================================================================================//

///============================================N-bit Two's Complement generator========================================///
class TwosComplementGenerator(width: Int) extends Module {
    val io =IO(new Bundle {
        val input  = Input(UInt((width+1).W))
        val output = Output(UInt((width+1).W)) 
    })

    // Function of class TwosComplementGenerator 
    val complementValue = ~(io.input) + 1.U     // One's complement of operand B is added 1 to get 2's complement. Concatenation 0 is done to get correct 2's complement for values greater than 127
    io.output := complementValue
}

//============================================PSUB.B -- SIMD 8Bit Subtraction=================OK TESTED========================///
class PSUBB extends Module {
    val io =IO(new Bundle {
        val Rs1   = Input(UInt(32.W))
        val Rs2   = Input(UInt(32.W))
        val Rd    = Output(UInt(32.W))
        val vxsat = Output(UInt(1.W))
    })

    // Funtion of class PSUBB
    val twosComplement = Seq.fill(4)(Module(new TwosComplementGenerator(8)))     // Instantaition of 8bit two's complement generator for operand B
    val A8             = Seq.fill(4)(Module(new Adder(8,UInt(9.W))))        // Instantaition of four 8bit Adders with 9bit ports
    val vxsatOV        = RegInit(0.U(1.W))
    for(x <- 0 until 4) {
        A8(x).io.cin               := 0.U
        A8(x).io.a                 := io.Rs1((x*8+7) , (x*8+0))    // 8bit Operand A assigned to 9bit port of Adder8
        twosComplement(x).io.input := Cat(io.Rs2(x*8+7) , io.Rs2((x*8+7) , (x*8+0)))   // twos complement calculation for operand B
        A8(x).io.b                 := twosComplement(x).io.output     // 8bit Two's Complement of Operand B assigned to 9bit port of Adder8
    }
    io.Rd := Cat(A8(3).io.sum(7,0) , A8(2).io.sum(7,0) , A8(1).io.sum(7,0) , A8(0).io.sum(7,0) )
    io.vxsat := vxsatOV
}

//======================================PASUB.B -- SIMD 8Bit Signed Averaging Subtraction=================================///
class PASUBB extends Module {
    val io = IO(new Bundle {
        val Rs1   = Input(UInt(32.W))
        val Rs2   = Input(UInt(32.W))
        val Rd    = Output(UInt(32.W))
        val vxsat = Output(UInt(1.W))
    })

    // Function of class PASUBB
    val SA8            = Seq.fill(4)(Module(new Adder(8,SInt(9.W))))
    val twosComplement = Seq.fill(4)(Module(new TwosComplementGenerator(8)))
    val vxsatOV        = RegInit(0.U(1.W))
    //val tempResult     = Wire(Vec(4,SInt(9.W)))

    for(x <- 0 until 4) {
        SA8(x).io.cin               := 0.U
        SA8(x).io.a                 := (io.Rs1((x*8+7) , (x*8+0))).asSInt
        twosComplement(x).io.input  := Cat(io.Rs2(x*8+7) , io.Rs2((x*8+7) , (x*8+0)))
        SA8(x).io.b                 := (twosComplement(x).io.output).asSInt
    }
    io.Rd   := Cat(SA8(3).io.sum(8,1) , SA8(2).io.sum(8,1) , SA8(1).io.sum(8,1) , SA8(0).io.sum(8,1))
    io.vxsat := vxsatOV
}
//======================================PASUBU.B -- SIMD 8Bit Unsigned Averaging Subtraction=================================///
class PASUBUB extends Module {
    val io = IO(new Bundle{
        val Rs1   = Input(UInt(32.W))
        val Rs2   = Input(UInt(32.W))
        val Rd    = Output(UInt(32.W)) 
        val vxsat = Output(UInt(1.W))       // vxsat CSR has OV information in LSB. vxsat[XLEN,1] bits are reserved.      
    })

    // Function of class PASUBUB
    val UA8            = Seq.fill(4)(Module(new Adder(8,UInt(9.W))))
    val twosComplement = Seq.fill(4)(Module(new TwosComplementGenerator(8)))
    val vxsatOV        = RegInit(0.U(1.W))        // Reg to store overflow across all clock cycles. Reset value of OV flag is 0.
    
    for (x <- 0 until 4) {
        UA8(x).io.cin              := 0.U // carryin inputs assigned zero. Since not used.
        UA8(x).io.a                := io.Rs1((x*8+7) , (x*8+0))     // 8bit unsigned inputs assigned to 9bit adder input. Zero extended to 9bit implicitly. 
        twosComplement(x).io.input := Cat(0.U, io.Rs2((x*8+7) , (x*8+0)))
        UA8(x).io.b                := twosComplement(x).io.output
    }

     io.Rd     := Cat(UA8(3).io.sum(8,1), UA8(2).io.sum(8,1), UA8(1).io.sum(8,1), UA8(0).io.sum(8,1)) 
     io.vxsat := vxsatOV
}

//======================================PSSUB.B -- SIMD 8Bit Signed Saturating Subtraction=================================///
class PSSUBB extends Module{
    val io = IO(new Bundle {
        val Rs1   = Input(UInt(32.W))
        val Rs2   = Input(UInt(32.W))
        val Rd    = Output(UInt(32.W))
        val vxsat = Output(UInt(1.W))       // vxsat CSR has OV information in LSB. vxsat[XLEN,1] bits are reserved.
    })

    //function of Class PSSUBB
    val SA8            = Seq.fill(4)(Module(new Adder(8,SInt(9.W))))
    val twosComplement = Seq.fill(4)(Module(new TwosComplementGenerator(8)))
    val vxsatOV        = RegInit(0.U(1.W))        // Reg to store overflow across all clock cycles. Reset value of OV flag is 0. 
    val sWire          = Wire(Vec(4, SInt(8.W)))

    for (x <- 0 until 4) {
        SA8(x).io.cin              := 0.U
        SA8(x).io.a                := io.Rs1((x*8+7) , (x*8+0)).asSInt    // 8bit element from Rs1 assigned to 9bit UInt Input port. Zero extended to 9bits.
        twosComplement(x).io.input := Cat(io.Rs2(x*8+7) , io.Rs2((x*8+7) , (x*8+0)))
        SA8(x).io.b                := (twosComplement(x).io.output).asSInt

        when((SA8(x).io.sum).asSInt < -128.S) {    // 
            sWire(x) := -128.S       // saturate the result
            vxsatOV  := 1.U 
        }.elsewhen((SA8(x).io.sum).asSInt > 127.S) {
            sWire(x) := 127.S
            vxsatOV  := 1.U 
        }.otherwise{
            sWire(x) := (SA8(x).io.sum(7,0)).asSInt      //else get the sum 
        }
    }

    io.Rd    := Cat(sWire(3), sWire(2), sWire(1), sWire(0))    // Concatenate the result into a 32bit word.  // REMOVE THE asSInt LATER
    io.vxsat := vxsatOV
}


//======================================PSSUBU.B -- SIMD 8Bit Unsigned Saturating Subtraction=================================///
class PSSUBUB extends Module{
    val io = IO(new Bundle {
        val Rs1   = Input(UInt(32.W))
        val Rs2   = Input(UInt(32.W))
        val Rd    = Output(UInt(32.W))
        val vxsat = Output(UInt(1.W))       // vxsat CSR has OV information in LSB. vxsat[XLEN,1] bits are reserved.
    })

    //function of Class PSSUBUB
    val UA8            = Seq.fill(4)(Module(new Adder(8,UInt(9.W))))
    val twosComplement = Seq.fill(4)(Module(new TwosComplementGenerator(8))) 
    val vxsatOV        = RegInit(0.U(1.W))        // Reg to store overflow across all clock cycles. Reset value of OV flag is 0. 
    val sWire          = Wire(Vec(4, UInt(8.W)))

    for (x <- 0 until 4) {
        UA8(x).io.cin              := 0.U
        UA8(x).io.a                := io.Rs1((x*8+7) , (x*8+0))    // 8bit element from Rs1 assigned to 9bit UInt Input port. Zero extended to 9bits.
        twosComplement(x).io.input := Cat(0.U , io.Rs2((x*8+7) , (x*8+0)))
        UA8(x).io.b                := twosComplement(x).io.output    // 9bit element from Rs2 assigned to 9bit UInt Input port. Zero extended to 9bits.
       
        // Overflow occurs when result is less than zero
        when((UA8(x).io.sum).asSInt < 0.S) {    // Result is in 2'complement. Comparing in Signed form  
            sWire(x) := 0.U       // Saturate the result
            vxsatOV  := 1.U       // Set OV flag
        }.otherwise {
            sWire(x) := UA8(x).io.sum(7,0)      //else get the sum 
        }
    }

    io.Rd    := Cat(sWire(3), sWire(2), sWire(1), sWire(0))    // Concatenate the result into a 32bit word.
    io.vxsat := vxsatOV
}

object GenerateVerilog extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new PADDB(), Array("--target-dir", "generated"))
}
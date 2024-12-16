import chisel3._
import chisel3.util._
//import chisel3.experimental.ChiselEnum

// Defining ALU operations as enumeration type using ChiselEnum
object ALUops extends ChiselEnum {
    val PADDB,PAADDB,PAADDUB,PSADDUB , PSUBB,PASUBB,PASUBUB,PSSUBB,PSSUBUB , PADDH,PAADDH,PAADDUH,PSADDH,PSADDUH , PSUBH,PASUBH,PASUBUH,PSSUBH,PSSUBUH , PASHX,PAASHX,PSASHX,PSAHX,PASAHX,PSSAHX , PMSEQH,PMSLTH,PMSLTUH,PMSLEH,PMSLEUH , PMINH,PMINUH,PMAXH,PMAXUH , PCLIPH,PCLIPUH,PABSH = Value
}

//================================
// 4x8bit ADDER module
//================================
class AdderALU extends Module {
    // Define the I/O interface
    val io = IO(new Bundle {
      val a        = Input(Vec(4, UInt(9.W)))    // Four 9-bit inputs representing Rs1, with an extra bit to handle carry propagation
      val b        = Input(Vec(4, UInt(9.W)))    // Four 9-bit inputs representing Rs2, with an extra bit to handle carry propagation
      val carryIn  = Input(Vec(4, UInt(1.W)))    // Four 1-bit carry-in values, one for each 8-bit addition
      val sum      = Output(Vec(4, UInt(9.W)))   // Four 9-bit outputs to store the sum of each byte addition (includes carry)
      val carryOut = Output(Vec(4, UInt(1.W)))   // Four 1-bit outputs to store carry-out for each byte addition
    })

    // Intermediate result wire to hold the 9-bit addition results before splitting into sum and carry-out
    val interResult = Wire(Vec(4,UInt(9.W)))

    // Perform SIMD-style addition for 4 independent 8-bit elements
    for(x <- 0 until 4) {   
        // Perform 9-bit addition for each element: a(x) + b(x) + carryIn(x)
        interResult(x) := io.a(x) +& io.b(x) +& io.carryIn(x)       // `+&` ensures carry propagation beyond 8 bits
        // Assign the lower 9 bits of the result to the sum output
        io.sum(x)      := interResult(x)
         // Extract the 9th bit (carry-out) and assign it to the corresponding carryOut output
        io.carryOut(x) := interResult(x)(8)
    }     
}

//==================================================================================
// Configurable Two's Complement generator module for SIMD 8bit and 16bit operations
//==================================================================================
class TwosComplementGenerator extends Module {      
    val io =IO(new Bundle {
        val input    = Input(Vec(4, UInt(9.W)))     // Four 9-bit input values for SIMD two's complement generation
        val output   = Output(Vec(4, UInt(9.W)))    // Four 9-bit output values after two's complement generation 
        val widthSel = Input(Bool())                // Configurable for 8bit and 16bit operations. ture.B => 8bit, false.B => 16bit
    })
    
    // Intermediate wire to hold the results of two's complement calculations
    val complementValue = Wire(Vec(4,UInt(9.W)))

    //******************************************************
    //For 4x8-bit Two's Complement Generation -> false.B
    //******************************************************
    when(io.widthSel === false.B) {      
        for (m <- 0 until 4) {
            // Compute two's complement: one's complement (~io.input(m)) + 1
            complementValue(m) := ~(io.input(m)) + 1.U           
        }  
    //******************************************************
    //For 2x16-bit Two's Complement Generation -> true.B
    //******************************************************
    }.otherwise {   
        val lower16 = Cat(io.input(1) , io.input(0)(7,0))   // input(1) is received as 9bits with Sign Extension.  Concatenates input(1) and lower 8 bits of input(0) to form a 17-bit value
        val upper16 = Cat(io.input(3) , io.input(2)(7,0))   // Similar as above. For 17th bit MSB Sign Extension, check the respective operation.
        //   In detail, this is the concatenation happening :     Cat(io.input(1)(8), io.input(1)(7, 0), io.input(0)(7, 0))

        // Compute two's complement for each 17-bit concatenated value
        val complementLower16 = ~lower16 + 1.U
        val complementUpper16 = ~upper16 + 1.U

        // Split the complemented 17-bit results back into 9-bit and 8-bit parts (= 17bits. Takes care of sign extension as well)
        complementValue(0) := complementLower16(7, 0)       // Complemented form of lower 8 bits goes into (9.W) complementValue. 
        complementValue(1) := complementLower16(16, 8)      // Upper 9bits of [Sign Extended to 17bit complemented value] goes into (9.W) complementValue
        complementValue(2) := complementUpper16(7, 0)       // Lower 8 bits of upper halfword
        complementValue(3) := complementUpper16(16, 8)      // Upper 9 bits of upper halfword (includes sign)
    }
    
    io.output := complementValue
}
  
//=============================================================
// Mux module for SIMD compare, max-min and absolute operations
//=============================================================
class SimdMuxHalf extends Module {
    val io = IO(new Bundle {
        val cond     = Input(Vec(2, Bool()))       // Conditions for lower and upper halves
        val trueVal  = Input(Vec(2, UInt(16.W)))   // True values for lower and upper halves
        val falseVal = Input(Vec(2, UInt(16.W)))   // False values for lower and upper halves
        val out      = Output(Vec(2, UInt(16.W)))  // Outputs for lower and upper halves
    })
    
    io.out(0) := Mux(io.cond(0), io.trueVal(0), io.falseVal(0)) // Lower half
    io.out(1) := Mux(io.cond(1), io.trueVal(1), io.falseVal(1)) // Upper half
}

//=======================================================
// Module handing the signed and unsigned clip operations
//=======================================================
class SimdClipHalf extends Module {
    val io = IO(new Bundle {
        val in         = Input(Vec(2, SInt(16.W)))  // Input values (lower and upper halves)
        val upperLimit = Input(SInt(16.W))          // Upper limit for clipping
        val lowerLimit = Input(SInt(16.W))          // Lower limit for clipping
        val out        = Output(Vec(2, SInt(16.W))) // Outputs for lower and upper halves
        val overflow   = Output(Vec(2, Bool()))     // Overflow detection for lower and upper halves
    })

    // Clipping logic
    for (i <- 0 until 2) {
        when(io.in(i) > io.upperLimit) {
            io.out(i)      := io.upperLimit
            io.overflow(i) := true.B
        }.elsewhen(io.in(i) < io.lowerLimit) {
            io.out(i)      := io.lowerLimit
            io.overflow(i) := true.B
        }.otherwise {
            io.out(i)      := io.in(i)
            io.overflow(i) := false.B
        }
    }
}

//================================
// ------P Extension ALU----------
//================================
// Port declaration of ALU
class PextALU extends Module { 
    val io = IO(new Bundle {
        val Rs1       = Input(UInt(32.W))       // 32-bit input operand Rs1
        val Rs2       = Input(UInt(32.W))       // 32-bit input operand Rs2
        val operation = Input(ALUops())         // ENUM data-type: Operation code to select the ALU operation
        val Rd        = Output(UInt(32.W))      // 32-bit output result Rd
        val vxsat_in  = Input(UInt(32.W))       // Input for the overflow status register
        val vxsat_out = Output(UInt(32.W))      // Output for the updated overflow status register
    })

    // Instantiation
    val fourByteAdder    = Module(new AdderALU())                   // 4x8-bit adder module
    val twosComplement   = Module(new TwosComplementGenerator())    // Two's Complement generator module
    val simdMux          = Module(new SimdMuxHalf())                // CMP,MAX-MIN,ABS module
    val simdClip         = Module(new SimdClipHalf())               // Module for signed and unsigned clip operations

    // Internal Wires
    val sumWires         = Wire(Vec(4, UInt(9.W)))      // 9-bit wires to hold results from four-byte adder
    val overflowDetected = VecInit(Seq.fill(4)(WireDefault(false.B)))   // A vector of 4 wires to capture overflow info for each 8-bit adder. Default value is 0
    val lowerHalf        = WireDefault(0.U(16.W))       // 16-bit wire for the lower half of comparison operations
    val upperHalf        = WireDefault(0.U(16.W))       // 16-bit wire for the upper half of comparison operations

    // Default values
    // Initialize all inputs of `fourByteAdder` to zero by default
    fourByteAdder.io.a         := VecInit(Seq.fill(4)(0.U(9.W)))
    fourByteAdder.io.b         := VecInit(Seq.fill(4)(0.U(9.W)))
    fourByteAdder.io.carryIn   := VecInit(Seq.fill(4)(0.U(1.W)))
    // Initialize all inputs of `twosComplement` to zero by default
    twosComplement.io.input    := VecInit(Seq.fill(4)(0.U(9.W)))
    twosComplement.io.widthSel := false.B
    // Initialise the inputs of Mux module
    simdMux.io.trueVal  := VecInit(Seq.fill(2)(0.U(16.W)))
    simdMux.io.falseVal := VecInit(Seq.fill(2)(0.U(16.W)))
    simdMux.io.cond     := VecInit(Seq.fill(2)(false.B))
    // Initialise the inputs of clipping module
    simdClip.io.in         := VecInit(Seq.fill(2)(0.S(16.W)))
    simdClip.io.upperLimit := 0.S(16.W)
    simdClip.io.lowerLimit := 0.S(16.W)
    // Initialize the internal wires and outputs to default values
    sumWires             := VecInit(Seq.fill(4)(0.U(9.W)))
    io.Rd        := 0.U
    io.vxsat_out := 0.U
    
    // ALU operation selection 

    //================================
    // 1. PADD.B -- SIMD 8bit Addition 
    //================================
    when(io.operation === ALUops.PADDB) {                      
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)        // Assign lower 8 bits of Rs1 to adder input a(0)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)        // Assign lower 8 bits of Rs2 to adder input b(0)
        fourByteAdder.io.carryIn(0) := 0.U                  // Carry-in is zero 
        sumWires(0)                 := fourByteAdder.io.sum(0)  // Store the result of the addition in sumWires(0)
        // Adder 1
        fourByteAdder.io.a(1)       := io.Rs1(15 , 8)       // Assign bits [15:8] of Rs1 to adder input a(1)
        fourByteAdder.io.b(1)       := io.Rs2(15 , 8)       // Assign bits [15:8] of Rs2 to adder input b(1)
        fourByteAdder.io.carryIn(1) := 0.U                  // Carry-in is zero
        sumWires(1)                 := fourByteAdder.io.sum(1)  // Store the result in sumWires(1)
        //Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)      // Assign bits [23:16] of Rs1 to adder input a(2)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)      // Assign bits [23:16] of Rs2 to adder input b(2)
        fourByteAdder.io.carryIn(2) := 0.U                  // Carry-in is zero for unsigned addition
        sumWires(2)                 := fourByteAdder.io.sum(2)  // Store the result in sumWires(2)
        // Adder 3
        fourByteAdder.io.a(3)       := io.Rs1(31 , 24)      // Assign bits [31:24] of Rs1 to adder input a(3)
        fourByteAdder.io.b(3)       := io.Rs2(31 , 24)      // Assign bits [31:24] of Rs2 to adder input b(3)
        fourByteAdder.io.carryIn(3) := 0.U                  // Carry-in is zero for unsigned addition
        sumWires(3)                 := fourByteAdder.io.sum(3)  // Store the result in sumWires(3)
        // Concatenate the 8-bit results from each adder to form the 32-bit Rd result                     
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := io.vxsat_in  // Leave the overflow register untouched, as there is no overflow handling for this operation.

    //===================================================
    // 2. PAADD.B -- SIMD 8-bit Signed Averaging Addition
    //===================================================    
    }.elsewhen(io.operation === ALUops.PAADDB) {            
        // Loop through each 8-bit segment (0 to 3) and assign Rs1 and Rs2 chunks to respective adders
        for (i <- 0 until 4) {
            // Concatenate MSB of the 8-bit segment to sign-extend the input for signed addition
            fourByteAdder.io.a(i)       := Cat(io.Rs1(i*8+7) , io.Rs1((i*8+7) , (i*8+0))) 
            fourByteAdder.io.b(i)       := Cat(io.Rs2(i*8+7) , io.Rs2((i*8+7) , (i*8+0)))      
            fourByteAdder.io.carryIn(i) := 0.U      // Carry-in is zero
            sumWires(i)                 := fourByteAdder.io.sum(i)      // Store the result from the adder
        }
        // Concatenate just the upper 8 bits which will take care of the shift right by 1 for averaging operation
        io.Rd        := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        // No overflow behavior; status register remains unchanged
        io.vxsat_out := io.vxsat_in  

    //======================================================
    // 3. PAADDU.B -- SIMD 8-bit Unsigned Averaging Addition
    //======================================================                
    }.elsewhen(io.operation === ALUops.PAADDUB) {    
        for (i <- 0 until 4) {
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0)) // 8bit Unsigned Input from Rs1 given to 9bit Unsigned adder input port a 
            fourByteAdder.io.b(i)       := io.Rs2((i*8+7) , (i*8+0)) // 8bit Unsigned Input from Rs2 given to 9bit Unsigned adder input port b
            fourByteAdder.io.carryIn(i) := 0.U     
            sumWires(i)                 := fourByteAdder.io.sum(i)      
        }
        // Concatenate just the upper 8 bits which will take care of the shift right by 1 for averaging operation
        io.Rd        := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat_out := io.vxsat_in     // Status register 
        //There is no overflow information. Status register left untouched
    
    //======================================================
    // 4. PSADDU.B -- SIMD 8Bit Unsigned Saturating Addition
    //======================================================
    }.elsewhen(io.operation === ALUops.PSADDUB) {
        for (i <- 0 until 4) {
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0)) // 8bit Unsigned Input from Rs1 given to 9bit Unsigned adder input port a 
            fourByteAdder.io.b(i)       := io.Rs2((i*8+7) , (i*8+0)) // 8bit Unsigned Input from Rs2 given to 9bit Unsigned adder input port b
            fourByteAdder.io.carryIn(i) := 0.U 

             // Check for overflow (when the 9th bit of the result is 1)
            when(fourByteAdder.io.sum(i)(8) === 1.U) {
                sumWires(i)         := 255.U         // Saturate to the maximum unsigned 8-bit value (255)
                overflowDetected(i) := true.B        // Set overflow flag for this segment 
            }.otherwise {
                sumWires(i)         := fourByteAdder.io.sum(i)      // Use the addition result if no overflow
                overflowDetected(i) := false.B                      // Unset overflow flag for this segment
            } 
        }
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        // Update the overflow status register: 
        //      Preserve the upper 31 bits of vxsat_in
        //      OR the incoming overflow information and put the ORed result in the LSB (i.e., Overflow Flag) of vxsat register
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3))

    //===================================
    // 5. PSUB.B -- SIMD 8Bit Subtraction
    //===================================
    }.elsewhen(io.operation === ALUops.PSUBB) {
        // Set the Two's Complement generator to 8-bit mode (widthSel = false)
        twosComplement.io.widthSel := false.B

        for(i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U      // Carry-in for subtraction chunks is always zero
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0))    // Pass the 8-bit unsigned input from Rs1 directly to the adder. Whether signed or unsigned, the user knows the values.
            twosComplement.io.input(i)  := Cat(io.Rs2(i*8+7) , io.Rs2((i*8+7) , (i*8+0)))        // Concatenate MSB (Sign Extension) to create a 9-bit input for the Two's Complement generator
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)      // Two's complement value as the second operand for the adder
            sumWires(i)                 := fourByteAdder.io.sum(i)          // Store the 9-bit result
        }
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched
    
    //=====================================================
    // 6. PASUB.B -- SIMD 8Bit Signed Averaging Subtraction
    //=====================================================
    }.elsewhen(io.operation === ALUops.PASUBB) {
        // Set the Two's Complement generator to 8-bit mode (widthSel = false)
        twosComplement.io.widthSel := false.B

        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := Cat(io.Rs1(i*8+7) , io.Rs1((i*8+7) , (i*8+0)))   // Concatenate MSB of Rs1 to preserve sign bits for signed right-shifting
            twosComplement.io.input(i)  := Cat(io.Rs2(i*8+7) , io.Rs2((i*8+7) , (i*8+0)))   // Concatenate MSB of Rs2 to create a 9-bit signed input for Two's Complement generator
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)
            sumWires(i)                 := fourByteAdder.io.sum(i)
        }
        // The upper 8 bits represent the result of shifting right by 1, achieving the signed averaging
        io.Rd        := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched

    //========================================================
    // 7. PASUBU.B -- SIMD 8Bit Unsigned Averaging Subtraction
    //========================================================
    }.elsewhen(io.operation === ALUops.PASUBUB) {
        // Set the Two's Complement generator to 8-bit mode (widthSel = false)
        twosComplement.io.widthSel := false.B

        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0))    // No concatenation; unsigned zero padding is implicit due to 9-bit adder input
            twosComplement.io.input(i)  := Cat(0.U , io.Rs2((i*8+7) , (i*8+0)))     // Concatenate a 0(since unisgned value) to create a 9-bit input for the Two's Complement generator
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)
            sumWires(i)                 := fourByteAdder.io.sum(i)
        }
        io.Rd        := Cat(sumWires(3)(8,1) , sumWires(2)(8,1) , sumWires(1)(8,1) , sumWires(0)(8,1))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched

    //======================================================
    // 8. PSSUB.B -- SIMD 8Bit Signed Saturating Subtraction
    //======================================================
    }.elsewhen(io.operation === ALUops.PSSUBB) {
        // Set the Two's Complement generator to 8-bit mode (widthSel = false)
        twosComplement.io.widthSel := false.B

        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := Cat(io.Rs1(i*8+7) , io.Rs1((i*8+7) , (i*8+0)))       // Concatenate MSB of Rs1 to preserve sign bits for signed subtraction
            twosComplement.io.input(i)  := Cat(io.Rs2(i*8+7) , io.Rs2((i*8+7) , (i*8+0)))       // Sign Extend and generate the two's complement of Rs2  
            fourByteAdder.io.b(i)       := twosComplement.io.output(i)

            // Check for signed saturation limits (-128 and 127)
            when((fourByteAdder.io.sum(i)).asSInt < -128.S) { 
                sumWires(i)         := (-128.S(9.W)).asUInt        // Saturate to the minimum signed 8-bit value (-128)
                overflowDetected(i) := true.B                      // Set overflow flag 
            }.elsewhen((fourByteAdder.io.sum(i)).asSInt > 127.S) {
                sumWires(i)         := (127.S(9.W)).asUInt      // Saturate to the maximum signed 8-bit value (127)
                overflowDetected(i) := true.B                   // Set overflow flag 
            }.otherwise {
                sumWires(i)         := fourByteAdder.io.sum(i)      // Use the subtraction result if within range
                overflowDetected(i) := false.B                      // Unset overflow flag
            }
        }
        // Concatenate the adder outputs to generate 32bit output and update the overflow status register
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3))  // XLEN concatenated with OV flag to form 32bit Status register, vxsat

    //=========================================================
    // 9. PSSUBU.B -- SIMD 8Bit Unsigned Saturating Subtraction
    //=========================================================
    }.elsewhen(io.operation === ALUops.PSSUBUB) {
        // Set the Two's Complement generator to 8-bit mode (widthSel = false)
        twosComplement.io.widthSel := false.B

        for (i <- 0 until 4) {
            fourByteAdder.io.carryIn(i) := 0.U 
            fourByteAdder.io.a(i)       := io.Rs1((i*8+7) , (i*8+0))        // Assign the 8-bit unsigned input from Rs1 directly to adder input a(i)
            twosComplement.io.input(i)  := Cat(0.U , io.Rs2((i*8+7) , (i*8+0)))      // Concatenate a 0 to create a 9-bit unsigned representation for two's complement generation
            fourByteAdder.io.b(i)       := twosComplement.io.output(i) 

            when((fourByteAdder.io.sum(i)).asSInt < 0.S) {    // Result is in 2'complement. Comparing in Signed form  
                sumWires(i)         := 0.U       // Saturate to 0 (minimum unsigned value)
                overflowDetected(i) := true.B    // Set overflow flag 
            }.otherwise {
                sumWires(i)         := fourByteAdder.io.sum(i)      // Use the subtraction result if no underflow 
                overflowDetected(i) := false.B                      // Unset overflow flag
            }
        }
        // Concatenate the lower 8 bits of the results
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        // Update the overflow status register
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(0) | overflowDetected(1) | overflowDetected(2) | overflowDetected(3))  // XLEN concatenated with OV flag to form 32bit Status register, vxsat
        
        //====================================================16 Bit Operations====================================================// 
    
    //===================================
    // 10. PADD.H -- SIMD 16-bit Addition
    //===================================
    }.elsewhen(io.operation === ALUops.PADDH) {
        // Adder 0: Lower 8 bits of the first 16-bit half-word
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)        // Lower byte of the first 16-bit half-word from Rs1
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)        // Lower byte of the first 16-bit half-word from Rs2
        fourByteAdder.io.carryIn(0) := 0.U                  // Carry-in is zero for the first adder
        sumWires(0)                 := fourByteAdder.io.sum(0)      // Store the result in sumWires(0)
        // Adder 1: Upper 8 bits of the first 16-bit half-word
        fourByteAdder.io.a(1)       := io.Rs1(15 , 8)       // Upper byte of the first 16-bit half-word from Rs1
        fourByteAdder.io.b(1)       := io.Rs2(15 , 8)       // Upper byte of the first 16-bit half-word from Rs2
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)     // Carry-out from adder 0 is carry-in for adder 1
        sumWires(1)                 := fourByteAdder.io.sum(1)      // Store the result in sumWires(1)
        // Adder 2: Lower 8 bits of the second 16-bit half-word
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)      // Lower byte of the second 16-bit half-word from Rs1
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)      // Lower byte of the second 16-bit half-word from Rs2
        fourByteAdder.io.carryIn(2) := 0.U                  // Carry-in is zero for the first part of the second half-word
        sumWires(2)                 := fourByteAdder.io.sum(2)      // Store the result in sumWires(2)
        // Adder 3: Upper 8 bits of the second 16-bit half-word
        fourByteAdder.io.a(3)       := io.Rs1(31 , 24)      // Upper byte of the second 16-bit half-word from Rs1
        fourByteAdder.io.b(3)       := io.Rs2(31 , 24)      // Upper byte of the second 16-bit half-word from Rs2
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)     // Carry-out from adder 2 is carry-in for adder 3
        sumWires(3)                 := fourByteAdder.io.sum(3)      // Store the result in sumWires(3)

        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched
    
    //=====================================================
    // 11. PAADD.H -- SIMD 16-bit Signed Averaging Addition
    //=====================================================    
    }.elsewhen(io.operation === ALUops.PAADDH) {
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(0)       
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))     // Sign-extend the upper byte of Rs1
        fourByteAdder.io.b(1)       := Cat(io.Rs2(15) , io.Rs2(15 , 8))     // Sign-extend the upper byte of Rs2
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        sumWires(1)                 := fourByteAdder.io.sum(1)
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(2)       
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))        // Sign-extend the upper byte of Rs1
        fourByteAdder.io.b(3)       := Cat(io.Rs2(31) , io.Rs2(31 , 24))        // Sign-extend the upper byte of Rs2
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(3)                 := fourByteAdder.io.sum(3)

        // Averaging (shift right by 1) implemented by extracting proper bits from four adder results
        io.Rd        := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched

    //========================================================
    // 12. PAADDU.H -- SIMD 16-bit Unsigned Averaging Addition
    //========================================================
    }.elsewhen(io.operation === ALUops.PAADDUH) {
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(0)
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(0.U , io.Rs1(15 , 8))        // Zero-extend the upper byte of Rs1
        fourByteAdder.io.b(1)       := Cat(0.U , io.Rs2(15 , 8))        // Zero-extend the upper byte of Rs2
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        sumWires(1)                 := fourByteAdder.io.sum(1)
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(2)
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(0.U , io.Rs1(31 , 24))       // Zero-extend the upper byte of Rs1
        fourByteAdder.io.b(3)       := Cat(0.U , io.Rs2(31 , 24))       // Zero-extend the upper byte of Rs2
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(3)                 := fourByteAdder.io.sum(3)

        // Averaging (shift right by 1) of upper byte and lower bytes obtained by extracting appropirate bits from sum output of adders
        io.Rd        := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))
        io.vxsat_out := io.vxsat_in     // Status register
        //There is no overflow information. Status register left untouched

    //======================================================
    // 13. PSADD.H -- SIMD 16-bit Signed Saturating Addition
    //======================================================    
    }.elsewhen(io.operation === ALUops.PSADDH) {
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))     // Sign-extend the upper byte of Rs1
        fourByteAdder.io.b(1)       := Cat(io.Rs2(15) , io.Rs2(15 , 8))     // Sign-extend the upper byte of Rs2
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))        // Sign-extend the upper byte of Rs1
        fourByteAdder.io.b(3)       := Cat(io.Rs2(31) , io.Rs2(31 , 24))        // Sign-extend the upper byte of Rs2
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)

        // Saturation logic for the lower 16-bit half-word (Adders 0 and 1)
        when((fourByteAdder.io.sum(1)).asSInt < -128.S) { 
            sumWires(0)         := 0.U                      // lower byte of 16bit signed min value 
            sumWires(1)         := (-128.S(9.W)).asUInt     // upper byte of 16bit signed min value
            overflowDetected(1) := true.B                   // Set overflow flag 
            // Combined 16-bit value: 0x8000 (-32768), the minimum value for a signed 16-bit register
        }.elsewhen((fourByteAdder.io.sum(1)).asSInt > 127.S) {
            sumWires(0)         := 255.U                            // lower byte of 16bit signed max value
            sumWires(1)         := (127.S(9.W)).asUInt              // upper byte of 16bit signed max value
            overflowDetected(1) := true.B           // Set overflow flag 
            // Combined 16-bit value: 0x7FFF (32767), the maximum value for a signed 16-bit register
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(0)      // Use normal addition result for lower byte
            sumWires(1)         := fourByteAdder.io.sum(1)      // Use normal addition result for upper byte
            overflowDetected(1) := false.B         // Unset overflow flag
            // Combined 16-bit value represents the actual computed result without saturation
        }
 
        // Saturation logic for the upper 16-bit half-word (Adders 2 and 3)
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
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(1) | overflowDetected(3)) 
 
    
    //=========================================================
    // 14. PSADDU.H -- SIMD 16-bit Unsigned Saturating Addition
    //=========================================================
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
            overflowDetected(1) := true.B           // Set overflow flag indicating the 16-bit result exceeded 65535
            // Combined 16-bit value: 0xFFFF (65535), the maximum value for an unsigned 16-bit register 
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(0)
            sumWires(1)         := fourByteAdder.io.sum(1)
            overflowDetected(1) := false.B         // Unset overflow flag
            // Combined 16-bit value represents the actual computed result within the range [0, 65535]
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
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(1) | overflowDetected(3))  
 
    
    //======================================
    // 15. PSUB.H -- SIMD 16-bit Subtraction
    //======================================
    }.elsewhen(io.operation === ALUops.PSUBH) {
        // 2x16bit Twos Complement generator selection
        twosComplement.io.widthSel  := true.B
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        twosComplement.io.input(0)  := io.Rs2(7 , 0)                  // Lower 8 bit sent as is to 16bit Twos Complement generator. Refer to Two's Complement generator class
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)    // Complemented lower 8bits sent to 9.W adder input
        fourByteAdder.io.carryIn(0) := 0.U                            // Carryin is 0 for Adder0, since 16bit additions
        sumWires(0)                 := fourByteAdder.io.sum(0)
        // Adder 1
        fourByteAdder.io.a(1)       := io.Rs1(15 , 8)
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))     // Upper 8bits of halfword sign extended in order to get 16bit complement value
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)          // Complemented 9bit value sent to 9.W Adder1 input. Refer to Complement Generator module
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)         // Carryout from previous byte result added to next byte. Carry out from lower byte has to be preserved.
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
    
    //========================================================
    // 16. PASUB.H -- SIMD 16-bit Signed Averaging Subtraction
    //========================================================
    }.elsewhen(io.operation === ALUops.PASUBH) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)                    // Does not need any concat. For first byte in 16bit, 9th bit carry info is of no concern. Adder-a input will be of 9bits though
        twosComplement.io.input(0)  := io.Rs2(7 , 0)                    // Lower 8 bit sent as is to 16bit Twos Complement generator. Refer to Two's Complement generator class
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)      // lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class).
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(0)          // Cat(carryout , lower 8bit 2's complement result) = 9bit output. But after s>>1, (7,1) bits are needed from this result. 
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))     // Sign Extension for first input in order to get SE17
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))     // Sign Extended second input sent to 16bit complement generator.  Refer to Two's Complement generator class
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)          // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class).
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)         // Carry from previous byte
        sumWires(1)                 := fourByteAdder.io.sum(1)              // 9bit sum value in 2's complement form. All 9bits, i.e. (8,0), needed after s>>1 operation. 
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        twosComplement.io.input(2)  := io.Rs2(23 , 16)
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
    
    //===========================================================
    // 17. PASUBU.H -- SIMD 16-bit Unsigned Averaging Subtraction
    //===========================================================    
    }.elsewhen(io.operation === ALUops.PASUBUH) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        twosComplement.io.input(0)  := io.Rs2(7 , 0)                    // Lower 8 bit sent as is to 16bit Twos Complement generator. Refer to Two's Complement generator class
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)      // lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(0)      // Cat(carryout , lower 8bit 2's complement result) = 9bit output
        // Adder 1
        fourByteAdder.io.a(1)       := io.Rs1(15 , 8)
        twosComplement.io.input(1)  := Cat(0.U , io.Rs2(15 , 8))        // Zero Extended second input (because Unsigned op) sent to 16bit complement generator.  Refer to Two's Complement generator class
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)     
        sumWires(1)                 := fourByteAdder.io.sum(1)
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        twosComplement.io.input(2)  := io.Rs2(23 , 16)
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

    //=========================================================
    // 18. PSSUB.H -- SIMD 16-bit Signed Saturating Subtraction
    //========================================================= 
    }.elsewhen(io.operation === ALUops.PSSUBH) {
       twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        twosComplement.io.input(0)  := io.Rs2(7 , 0)                    // Lower 8bits sent as is to 16bit Twos Complement generator. Refer to Two's Complement generator class
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)      // Lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class).
        fourByteAdder.io.carryIn(0) := 0.U
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(15) , io.Rs1(15 , 8)) // Sign Extension for first input in order to get SE17
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8)) // Sign Extended second input sent to 16bit complement generator.  Refer to Two's Complement generator class
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)     
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        twosComplement.io.input(2)  := io.Rs2(23 , 16)
        fourByteAdder.io.b(2)       := twosComplement.io.output(2)
        fourByteAdder.io.carryIn(2) := 0.U
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))
        twosComplement.io.input(3)  := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.b(3)       := twosComplement.io.output(3)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)

        // Saturation logic for the lower 16-bit half-word
        when((fourByteAdder.io.sum(1)).asSInt < -128.S) {                    
            sumWires(0)         := 0.U                               // Lower byte saturated to 0
            sumWires(1)         := (-128.S(9.W)).asUInt              // Upper byte saturated to -128
            overflowDetected(1) := true.B                            // Overflow flag set
            // Combined 16-bit value: 0x8000 (-32768), the minimum value for a signed 16-bit register
        }.elsewhen((fourByteAdder.io.sum(1)).asSInt > 127.S) {
            sumWires(0)         := 255.U                            // Lower byte saturated to 255
            sumWires(1)         := (127.S(9.W)).asUInt              // Upper byte saturated to 127
            overflowDetected(1) := true.B                           // Overflow flag set
            // Combined 16-bit value: 0x7FFF (32767), the maximum value for a signed 16-bit register
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(0)          // Use normal subtraction result for lower byte
            sumWires(1)         := fourByteAdder.io.sum(1)          // Use normal subtraction result for upper byte
            overflowDetected(1) := false.B           // Unset overflow flag 
        }
 
        // Saturation logic for the upper 16-bit half-word
        when((fourByteAdder.io.sum(3)).asSInt < -128.S) {                      
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
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(1) | overflowDetected(3)) 

    //============================================================
    // 19. PSSUBU.H -- SIMD 16-bit Unsigned Saturating Subtraction
    //============================================================    
    }.elsewhen(io.operation === ALUops.PSSUBUH) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0
        fourByteAdder.io.a(0)       := io.Rs1(7 , 0)
        twosComplement.io.input(0)  := io.Rs2(7 , 0)                    // Lower 8 bit sent as is to 16bit Twos Complement generator. Refer to Two's Complement generator class
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)      // lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class)
        fourByteAdder.io.carryIn(0) := 0.U
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(0.U , io.Rs1(15 , 8))        // *****Explicit concatenation is not required since port is (9.W)*****
        twosComplement.io.input(1)  := Cat(0.U , io.Rs2(15 , 8))
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)      // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)    
        // Adder 2
        fourByteAdder.io.a(2)       := io.Rs1(23 , 16)
        twosComplement.io.input(2)  := io.Rs2(23 , 16)
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
            overflowDetected(1) := true.B            // Set overflow flag 
            // Combined 16-bit value: 0x0000 (0), the minimum value for an unsigned 16-bit register
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(0)
            sumWires(1)         := fourByteAdder.io.sum(1)
            overflowDetected(1) := false.B           // Unset overflow flag
        }

        when((fourByteAdder.io.sum(3)).asSInt < 0.S) {      // Saturating condition for upper 16bit result
            sumWires(2)         := 0.U
            sumWires(3)         := 0.U
            overflowDetected(3) := true.B            // Set overflow flag 
        }.otherwise {
            sumWires(2)         := fourByteAdder.io.sum(2)
            sumWires(3)         := fourByteAdder.io.sum(3)
            overflowDetected(3) := false.B           // Unset overflow flag
        } 
        
        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(1) | overflowDetected(3))  
    
    //=======================================================
    // 20. PAS.HX -- SIMD 16-bit Cross Addition & Subtraction
    //======================================================= 
    }.elsewhen(io.operation === ALUops.PASHX) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        //               ===ADDITION===
        // Adder 0: Adds the lower byte of the upper half-word of Rs1 ([23:16]) 
        //          with the lower byte of the lower half-word of Rs2 ([7:0]).
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(0)                  // Result of addition stored in sumWires(2) (contributes to Rd[23:16])
        // Adder 1: Adds the upper byte of the upper half-word of Rs1 ([31:24]) 
        //          with the upper byte of the lower half-word of Rs2 ([15:8]).
        fourByteAdder.io.a(1)       := io.Rs1(31 , 24)
        fourByteAdder.io.b(1)       := io.Rs2(15 , 8)
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        sumWires(3)                 := fourByteAdder.io.sum(1)                  // Result of addition stored in sumWires(3) (contributes to Rd[31:24])
       //              ===SUBTRACTION===
       // Adder 2: Subtracts the lower byte of the upper half-word of Rs2 ([23:16]) 
       //          from the lower byte of the lower half-word of Rs1 ([7:0]).
       fourByteAdder.io.a(2)       := io.Rs1(7 , 0)
       twosComplement.io.input(2)  := io.Rs2(23 , 16)
       fourByteAdder.io.b(2)       := twosComplement.io.output(2)
       fourByteAdder.io.carryIn(2) := 0.U
       sumWires(0)                 := fourByteAdder.io.sum(2)                   // Result of subtraction stored in sumWires(0) (contributes to Rd[7:0])
       // Adder 3: Subtracts the upper byte of the upper half-word of Rs2 ([31:24]) 
       //          from the upper byte of the lower half-word of Rs1 ([15:8]).
       fourByteAdder.io.a(3)       := io.Rs1(15 , 8)
       twosComplement.io.input(3)  := Cat(io.Rs2(31) , io.Rs2(31 , 24))
       fourByteAdder.io.b(3)       := twosComplement.io.output(3)
       fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
       sumWires(1)                 := fourByteAdder.io.sum(3)                   // Result of addition stored in sumWires(1) (contributes to Rd[15:8])

       io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
       io.vxsat_out := io.vxsat_in     // Status register

    //=========================================================================
    // 21. PAAS.HX -- SIMD 16-bit Signed Averaging Cross Addition & Subtraction
    //=========================================================================
    }.elsewhen(io.operation === ALUops.PAASHX) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        //              ===ADDITION===        
        // Adder 0: Adds lower byte of the upper half-word of Rs1 ([23:16]) with the lower byte of the lower half-word of Rs2 ([7:0])    
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)                 
        fourByteAdder.io.b(0)       := io.Rs2(7 , 0)                   
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(0)              // Result of addition stored in sumWires(2) (contributes to Rd[23:16])
        // Adder 1: Adds upper byte of the upper half-word of Rs1 ([31:24]) with the upper byte of the lower half-word of Rs2 ([15:8])
        fourByteAdder.io.a(1)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))    // Upper byte of the upper half-word of Rs1, sign-extended
        fourByteAdder.io.b(1)       := Cat(io.Rs2(15) , io.Rs2(15 , 8))     // Upper byte of the lower half-word of Rs2, sign-extended
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)
        sumWires(3)                 := fourByteAdder.io.sum(1)              // Result of addition stored in sumWires(3) (contributes to Rd[31:24])
        //              ===SUBTRACTION===
        // Adder 2: Subtracts the lower byte of the upper half-word of Rs2 ([23:16]) from the lower byte of the lower half-word of Rs1 ([7:0])          
        fourByteAdder.io.a(2)       := io.Rs1(7 , 0)
        twosComplement.io.input(2)  := io.Rs2(23 , 16)                      // 8 bits sent as is to 16bit Twos Complement generator. Refer to Two's Complement generator class
        fourByteAdder.io.b(2)       := twosComplement.io.output(2)
        fourByteAdder.io.carryIn(2) := 0.U
        sumWires(0)                 := fourByteAdder.io.sum(2)              // Result of subtraction stored in sumWires(0) (contributes to Rd[7:0])
        // Adder 3: Subtracts the upper byte of the upper half-word of Rs2 ([31:24]) from the upper byte of the lower half-word of Rs1 ([15:8])
        fourByteAdder.io.a(3)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))     // sign-extended half word
        twosComplement.io.input(3)  := Cat(io.Rs2(31) , io.Rs2(31 , 24))    // Sign extended to 9bits sent to 16bit Twos Complement generator. Refer to Two's Complement generator class
        fourByteAdder.io.b(3)       := twosComplement.io.output(3)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)
        sumWires(1)                 := fourByteAdder.io.sum(3)              // Result of addition stored in sumWires(1) (contributes to Rd[15:8])

        io.Rd        := Cat(sumWires(3)(8,0) , sumWires(2)(7,1) , sumWires(1)(8,0) , sumWires(0)(7,1))
        io.vxsat_out := io.vxsat_in     // Status register. No extra overflow information coming out of this operation

    //==========================================================================
    // 22. PSAS.HX -- SIMD 16-bit Signed Saturating Cross Addition & Subtraction
    //==========================================================================
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
        twosComplement.io.input(2)  := io.Rs2(23 , 16)                    
        fourByteAdder.io.b(2)       := twosComplement.io.output(2)
        fourByteAdder.io.carryIn(2) := 0.U
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))
        twosComplement.io.input(3)  := Cat(io.Rs2(31) , io.Rs2(31 , 24))  
        fourByteAdder.io.b(3)       := twosComplement.io.output(3)
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)

        when((fourByteAdder.io.sum(1)).asSInt < -128.S) { 
            sumWires(2)         := 0.U       
            sumWires(3)         := (-128.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag indicating the lower 16-bit result is less than -32768 
        }.elsewhen((fourByteAdder.io.sum(1)).asSInt > 127.S) {
            sumWires(2)         := 255.U
            sumWires(3)         := (127.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag indicating the lower 16-bit result is greater than 32767 
        }.otherwise {
            sumWires(2)         := fourByteAdder.io.sum(0)
            sumWires(3)         := fourByteAdder.io.sum(1)
            overflowDetected(1) := false.B         // Unset overflow flag
        }

        when((fourByteAdder.io.sum(3)).asSInt < -128.S) {                      
            sumWires(0)         := 0.U
            sumWires(1)         := (-128.S(9.W)).asUInt
            overflowDetected(3) := true.B           // Set overflow flag due to upper half overflow
        }.elsewhen((fourByteAdder.io.sum(3)).asSInt > 127.S) {
            sumWires(0)         := 255.U
            sumWires(1)         := (127.S(9.W)).asUInt
            overflowDetected(3) := true.B           // Set overflow flag due to upper half overflow
        }.otherwise {
            sumWires(0)         := fourByteAdder.io.sum(2)
            sumWires(1)         := fourByteAdder.io.sum(3)
            overflowDetected(3) := false.B           // Unset overflow flag
        }

        io.Rd        := Cat(sumWires(3)(7,0) , sumWires(2)(7,0) , sumWires(1)(7,0) , sumWires(0)(7,0))
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(1) | overflowDetected(3)) 
    
    //=======================================================
    // 23. PSA.HX -- SIMD 16-bit Cross Subtraction & Addition
    //=======================================================
    }.elsewhen(io.operation === ALUops.PSAHX) {
        twosComplement.io.widthSel  := true.B
        // Adder 0          ===SUBTRACTION===
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)
        twosComplement.io.input(0)  := io.Rs2(7 , 0)    
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)  
        fourByteAdder.io.carryIn(0) := 0.U      
        sumWires(2)                 := fourByteAdder.io.sum(0)
        // Adder 1
        fourByteAdder.io.a(1)       := io.Rs1(31 , 24)
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))     
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)         
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)        
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
    
    //=========================================================================
    // 24. PASA.HX -- SIMD 16-bit Signed Averaging Cross Subtraction & Addition
    //=========================================================================
    }.elsewhen(io.operation === ALUops.PASAHX) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder0           ===SUBTRACTION===
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)                   // Does not need any concat. For first byte in 16bit, 9th is of no concern. Adder-a input will be of 9bits though
        twosComplement.io.input(0)  := io.Rs2(7 , 0)                     // 8 bits sent as is to 16bit Twos Complement generator. Refer to Two's Complement generator class
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)       // lower 8bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class). 
        fourByteAdder.io.carryIn(0) := 0.U
        sumWires(2)                 := fourByteAdder.io.sum(0)           // Cat(carryout , lower 8bit 2's complement result) = 9bit output. But after s>>1, (7,1) bits are needed off this result. 
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))     // Sign Extension for upper byte in order to get SE17
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))      // Sign Extension for upper byte, sent to 16bit complement generator
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)           // Upper 9bits from SE-17bit-complemented values goes to (9.W) b input of Adder(REFER to TwosComplementGenerator class).
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)          // Carry from previous byte
        sumWires(3)                 := fourByteAdder.io.sum(1)               // 9bit sum value in 2's complement form. All 9bits needed after s>>1
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
    
    //==========================================================================
    // 25. PSSA.HX -- SIMD 16-bit Signed Saturating Cross Subtraction & Addition
    //==========================================================================
    }.elsewhen(io.operation === ALUops.PSSAHX) {
        twosComplement.io.widthSel := true.B        // 16bit complement generator selected
        // Adder 0          ===SUBTRACTION===
        fourByteAdder.io.a(0)       := io.Rs1(23 , 16)
        twosComplement.io.input(0)  := io.Rs2(7 , 0)       
        fourByteAdder.io.b(0)       := twosComplement.io.output(0)     
        fourByteAdder.io.carryIn(0) := 0.U
        // Adder 1
        fourByteAdder.io.a(1)       := Cat(io.Rs1(31) , io.Rs1(31 , 24))
        twosComplement.io.input(1)  := Cat(io.Rs2(15) , io.Rs2(15 , 8))
        fourByteAdder.io.b(1)       := twosComplement.io.output(1)     
        fourByteAdder.io.carryIn(1) := fourByteAdder.io.carryOut(0)  
        // Adder 2            ===ADDITION===
        fourByteAdder.io.a(2)       := io.Rs1(7 , 0)
        fourByteAdder.io.b(2)       := io.Rs2(23 , 16)
        fourByteAdder.io.carryIn(2) := 0.U
        // Adder 3
        fourByteAdder.io.a(3)       := Cat(io.Rs1(15) , io.Rs1(15 , 8))
        fourByteAdder.io.b(3)       := Cat(io.Rs2(31) , io.Rs2(31 , 24))
        fourByteAdder.io.carryIn(3) := fourByteAdder.io.carryOut(2)

        when((fourByteAdder.io.sum(1)).asSInt < -128.S) {                     
            sumWires(2)         := 0.U   
            sumWires(3)         := (-128.S(9.W)).asUInt
            overflowDetected(1) := true.B           // Set overflow flag 
        }.elsewhen((fourByteAdder.io.sum(1)).asSInt > 127.S) {
            sumWires(2)         := 255.U     
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
        io.vxsat_out := Cat(io.vxsat_in(31, 1) , overflowDetected(1) | overflowDetected(3)) 
    
    //==================================COMPARE INSTRUCTIONS====================================//
    //=================================================
    // 26. PMSEQ.H -- SIMD 16-bit Integer Compare Equal
    //=================================================
    }.elsewhen(io.operation === ALUops.PMSEQH) {      
        
        //  =====CONDITION=====
        simdMux.io.cond := VecInit(Seq(
            io.Rs1(15, 0)  === io.Rs2(15, 0),  // Lower half condition
            io.Rs1(31, 16) === io.Rs2(31, 16)  // Upper half condition
          ))
        //  =====VALUES=====
          simdMux.io.trueVal  := VecInit(Seq.fill(2)("hFFFF".U(16.W))) // Same true value for both halves
          simdMux.io.falseVal := VecInit(Seq.fill(2)(0.U(16.W)))       // Same false value for both halves
        //  =====OUTPUT=====
          io.Rd        := Cat(simdMux.io.out(1), simdMux.io.out(0)) // Concatenate upper and lower halves
          io.vxsat_out := io.vxsat_in
   
    //====================================================
    // 27. PMSLT.H -- SIMD 16-bit Signed Compare Less Than
    //====================================================
    }.elsewhen(io.operation === ALUops.PMSLTH) {

        //  =====CONDITION=====
        simdMux.io.cond := VecInit(Seq(
            (io.Rs1(15 ,  0)).asSInt < (io.Rs2(15 ,  0)).asSInt,  // Lower half condition
            (io.Rs1(31 , 16)).asSInt < (io.Rs2(31 , 16)).asSInt   // Upper half condition
            ))
        //  =====VALUES=====
        simdMux.io.trueVal  := VecInit(Seq.fill(2)("hFFFF".U(16.W))) // Same true value for both halves
        simdMux.io.falseVal := VecInit(Seq.fill(2)(0.U(16.W)))       // Same false value for both halves
        //  =====OUTPUT=====
        io.Rd        := Cat(simdMux.io.out(1), simdMux.io.out(0)) // Concatenate upper and lower halves
        io.vxsat_out := io.vxsat_in
 
    //=======================================================
    // 28. PMSLTU.H -- SIMD 16-bit Unsigned Compare Less Than
    //=======================================================
    }.elsewhen(io.operation === ALUops.PMSLTUH) {

        //  =====CONDITION=====
        simdMux.io.cond := VecInit(Seq(
            io.Rs1(15, 0)  < io.Rs2(15, 0),  // Lower half condition
            io.Rs1(31, 16) < io.Rs2(31, 16)  // Upper half condition
            ))
        //  =====VALUES=====
        simdMux.io.trueVal  := VecInit(Seq.fill(2)("hFFFF".U(16.W))) // Same true value for both halves
        simdMux.io.falseVal := VecInit(Seq.fill(2)(0.U(16.W)))       // Same false value for both halves
        //  =====OUTPUT=====
        io.Rd        := Cat(simdMux.io.out(1), simdMux.io.out(0)) // Concatenate upper and lower halves
        io.vxsat_out := io.vxsat_in
   
    //=============================================================
    // 29. PMSLE.H -- SIMD 16-bit Signed Compare Less Than or Equal         Possible nomenclature error in the specification
    //=============================================================
    }.elsewhen(io.operation === ALUops.PMSLEH) {

        //  =====CONDITION=====
            simdMux.io.cond := VecInit(Seq(
                (io.Rs1(15 ,  0)).asSInt <= (io.Rs2(15 ,  0)).asSInt,  // Lower half condition
                (io.Rs1(31 , 16)).asSInt <= (io.Rs2(31 , 16)).asSInt   // Upper half condition
              ))
            //  =====VALUES=====
              simdMux.io.trueVal  := VecInit(Seq.fill(2)("hFFFF".U(16.W))) // Same true value for both halves
              simdMux.io.falseVal := VecInit(Seq.fill(2)(0.U(16.W)))       // Same false value for both halves
            //  =====OUTPUT=====
              io.Rd        := Cat(simdMux.io.out(1), simdMux.io.out(0)) // Concatenate upper and lower halves
              io.vxsat_out := io.vxsat_in
  
    //===============================================================
    // 30. PMSLEU.H -- SIMD 16-bit Unsigned Compare Less Than & Equal       Possible nomenclature error in the specification
    //===============================================================
    }.elsewhen(io.operation === ALUops.PMSLEUH) {

        //  =====CONDITION=====
        simdMux.io.cond := VecInit(Seq(
            io.Rs1(15, 0)  <= io.Rs2(15, 0),  // Lower half condition
            io.Rs1(31, 16) <= io.Rs2(31, 16)  // Upper half condition
            ))
        //  =====VALUES=====
        simdMux.io.trueVal  := VecInit(Seq.fill(2)("hFFFF".U(16.W))) // Same true value for both halves
        simdMux.io.falseVal := VecInit(Seq.fill(2)(0.U(16.W)))       // Same false value for both halves
        //  =====OUTPUT=====
        io.Rd        := Cat(simdMux.io.out(1), simdMux.io.out(0)) // Concatenate upper and lower halves
        io.vxsat_out := io.vxsat_in
 
    //=========================================
    // 31. PMIN.H -- SIMD 16-bit Signed Minimum
    //=========================================
    }.elsewhen(io.operation === ALUops.PMINH) {
        //  =====CONDITION=====
        simdMux.io.cond := VecInit(Seq(
            (io.Rs1(15, 0)).asSInt  < (io.Rs2(15, 0)).asSInt,  // Lower half condition
            (io.Rs1(31, 16)).asSInt < (io.Rs2(31, 16)).asSInt  // Upper half condition
        ))
        //  =====VALUE=====
        simdMux.io.trueVal := VecInit(Seq(
            io.Rs1(15, 0),                                  // True value for lower half
            io.Rs1(31, 16)                                  // True value for upper half
        ))     
        simdMux.io.falseVal := VecInit(Seq(
            io.Rs2(15, 0),                                  // False value for lower half
            io.Rs2(31, 16)                                  // False value for upper half
        ))
        //  =====OUTPUT=====
        io.Rd        := Cat(simdMux.io.out(1), simdMux.io.out(0)) // Concatenate upper and lower halves
        io.vxsat_out := io.vxsat_in

    //============================================
    // 32. PMINU.H -- SIMD 16-bit Unsigned Minimum
    //============================================
    }.elsewhen(io.operation === ALUops.PMINUH) {

        //  =====CONDITION=====
        simdMux.io.cond := VecInit(Seq(
            io.Rs1(15, 0)  < io.Rs2(15, 0),   // Lower half condition
            io.Rs1(31, 16) < io.Rs2(31, 16)  // Upper half condition
        ))
        //  =====VALUE=====
        simdMux.io.trueVal := VecInit(Seq(
            io.Rs1(15, 0),                                  // True value for lower half
            io.Rs1(31, 16)                                  // True value for upper half
        ))     
        simdMux.io.falseVal := VecInit(Seq(
            io.Rs2(15, 0),                                  // False value for lower half
            io.Rs2(31, 16)                                  // False value for upper half
        ))
        //  =====OUTPUT=====
        io.Rd        := Cat(simdMux.io.out(1), simdMux.io.out(0)) // Concatenate upper and lower halves
        io.vxsat_out := io.vxsat_in

    //=========================================
    // 33. PMAX.H -- SIMD 16-bit Signed Maximum
    //=========================================
    }.elsewhen(io.operation === ALUops.PMAXH) {

        //  =====CONDITION=====
        simdMux.io.cond := VecInit(Seq(
            (io.Rs1(15, 0)).asSInt  > (io.Rs2(15, 0)).asSInt,  // Lower half condition
            (io.Rs1(31, 16)).asSInt > (io.Rs2(31, 16)).asSInt  // Upper half condition
        ))
        //  =====VALUE=====
        simdMux.io.trueVal := VecInit(Seq(
            io.Rs1(15, 0),                                  // True value for lower half
            io.Rs1(31, 16)                                  // True value for upper half
        ))     
        simdMux.io.falseVal := VecInit(Seq(
            io.Rs2(15, 0),                                  // False value for lower half
            io.Rs2(31, 16)                                  // False value for upper half
        ))
        //  =====OUTPUT=====
        io.Rd        := Cat(simdMux.io.out(1), simdMux.io.out(0)) // Concatenate upper and lower halves
        io.vxsat_out := io.vxsat_in

    //============================================
    // 34. PMAXU.H -- SIMD 16-bit Unsigned Maximum
    //============================================
    }.elsewhen(io.operation === ALUops.PMAXUH) {

        //  =====CONDITION=====
        simdMux.io.cond := VecInit(Seq(
            io.Rs1(15, 0)  > io.Rs2(15, 0),   // Lower half condition
            io.Rs1(31, 16) > io.Rs2(31, 16)  // Upper half condition
        ))
        //  =====VALUE=====
        simdMux.io.trueVal := VecInit(Seq(
            io.Rs1(15, 0),                                  // True value for lower half
            io.Rs1(31, 16)                                  // True value for upper half
        ))     
        simdMux.io.falseVal := VecInit(Seq(
            io.Rs2(15, 0),                                  // False value for lower half
            io.Rs2(31, 16)                                  // False value for upper half
        ))
        //  =====OUTPUT=====
        io.Rd        := Cat(simdMux.io.out(1), simdMux.io.out(0)) // Concatenate upper and lower halves
        io.vxsat_out := io.vxsat_in
    
    //===================================
    // 35. PABS.H -- SIMD 16-bit Absolute       // Possible correction in specification in both nomenclature and operation
    //===================================
        // * Rs1 contains signed 16bit elements
        // * If 16bit element is >= zero, result is as it is
        // * For negative values, take 2's complement
    }.elsewhen(io.operation === ALUops.PABSH) {

        //  =====CONDITION=====
        simdMux.io.cond := VecInit(Seq(
            io.Rs1(15,0).asSInt  < 0.S,   // Lower half condition
            io.Rs1(31,16).asSInt < 0.S    // Upper half condition
        ))
        //    =====VALUE=====
        simdMux.io.trueVal := VecInit(Seq(
            (-(io.Rs1(15,0).asSInt)).asUInt,           // True value for lower half. -(-ve) gives +ve value
            (-(io.Rs1(31,16).asSInt)).asUInt           // True value for upper half. -(-ve) gives +ve value
        ))     
        simdMux.io.falseVal := VecInit(Seq(
            io.Rs1(15,0),                             // False value for lower half. As it is value sent to output
            io.Rs1(31,16)                             // False value for upper half. As it is value sent to output
        ))
        //  =====OUTPUT=====
        io.Rd        := Cat(simdMux.io.out(1), simdMux.io.out(0)) // Concatenate upper and lower halves
        io.vxsat_out := io.vxsat_in
    
    //=============================================
    // 36. PCLIP.H -- SIMD 16-bit Signed Clip Value      Greyed out on the specification. Operation's enum value is chosen based on the naming logic derived from previous non-greyed instruction.
    //=============================================
    }.elsewhen(io.operation === ALUops.PCLIPH) {
        val imm4u           = (io.Rs2(3,0)).asUInt      // Decoder extracts and sends in the immediate value, padded to 32 bits, at Rs2 input port.

        simdClip.io.upperLimit := (1.S << imm4u) - 1.S  // Unsigned immediate value is used to form the signed upper limit for clip op.
        simdClip.io.lowerLimit := -(1.S << imm4u)       // Unsigned immediate value is used to form the signed lower limit for clip op.
        simdClip.io.in         := VecInit(Seq(
          io.Rs1(15, 0).asSInt,                         // Lower half input
          io.Rs1(31, 16).asSInt                         // Upper half input
        ))
    
        io.Rd        := Cat(simdClip.io.out(1), simdClip.io.out(0)) // Concatenate upper and lower halves
        io.vxsat_out := Cat(io.vxsat_in(31, 1), simdClip.io.overflow(0) | simdClip.io.overflow(1)) 

    //================================================
    // 37. PCLIPU.H -- SIMD 16-bit Unsigned Clip Value      Greyed out on the specification. Operation's enum value is chosen based on the naming logic derived from previous non-greyed instruction.
    //================================================
    }.elsewhen(io.operation === ALUops.PCLIPUH) {
        val imm4u          = (io.Rs2(3,0)).asUInt       // Decoder extracts and sends in the immediate value, padded to 32 bits, at Rs2 input port.

        simdClip.io.upperLimit := (1.S << imm4u) - 1.S
        simdClip.io.lowerLimit := 0.S
        simdClip.io.in := VecInit(Seq(
          io.Rs1(15, 0).asSInt,                         // Lower half input
          io.Rs1(31, 16).asSInt                         // Upper half input
        ))
    
        io.Rd := Cat(simdClip.io.out(1), simdClip.io.out(0)) // Concatenate upper and lower halves
        io.vxsat_out := Cat(io.vxsat_in(31, 1), simdClip.io.overflow(0) | simdClip.io.overflow(1))
    }   
}
 
object ALUMain extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new PextALU(), Array("--target-dir", "generated"))
}

# RISC-V P Extension ALU Implementation in Chisel

## About RISC-V P Extension

The RISC-V P Extension is a specialised extension designed to enhance performance in DSP and multimedia applications. It introduces SIMD capabilities in a RISC-V processor, allowing parallel operations on packed data, significantly improving performance for tasks like image processing, audio encoding, and more.

## Overview

This project implements an **ALU (Arithmetic Logic Unit)** for the **RISC-V P Extension** using the **Chisel hardware construction language**. The design focuses on **hardware reuse** and **modularity**, aligning with RISC-V's philosophy of efficiency and extensibility. The ALU supports various **SIMD (Single Instruction Multiple Data)** operations, enabling **parallel processing** of smaller data elements within 32-bit registers, making it suitable for **DSP (Digital Signal Processing)** and other performance-critical applications.

---

## Key Features

- **Modular Design**: Each functional block (adder, two's complement generator, MUX, and clip modules) is implemented as a reusable module, enhancing scalability and readability.
- **Hardware Reuse**: 
  - A **4x8-bit adder** is shared for both 8-bit and 16-bit operations.
  - A **2x1 multiplexer** handles all comparison, min-max, and absolute value operations.
  - A unified module is used for both signed and unsigned clip operations, optimizing area, readability, and efficiency.
- **Support for RISC-V P Extension**: Implements SIMD arithmetic, averaging, saturating, clipping, and comparison instructions for 8-bit and 16-bit data types.
- **Chisel Implementation**: Leverages the Chisel language to describe hardware at a high abstraction level while maintaining synthesisability.

---

## Repository Structure

The repository follows a standard Chisel project structure:

```plaintext
.
├── src
│   ├── main
│   │   └── scala
│   │       ├── PextALU.scala        # Main P Extension ALU implementation
│   │                                # Contains:
│   │                                #   - ALU operations enumerator
│   │                                #   - 4x8-bit adder module
│   │                                #   - Configurable two's complement generator module
│   │                                #   - SIMD MUX module for comparison, max-min, and abs operations
│   │                                #   - SIMD clip operations module
│   └── test
│       └── scala
│           └── TesterPextALU.scala  # Testbench for the ALU
├── generated                        # Directory for generated Verilog files
└── README.md                        # Project documentation

```
---

## Supported Instructions

The ALU so far supports **37 instructions** (continuosly under development) from the **RISC-V P Extension Specification**, categorised as follows:

### 8-bit Operations

- **Simple Addition**: `PADDB`
- **Simple Subtraction**: `PSUBB`
- **Averaging**: `PAADDB`, `PAADDUB`, `PASUBB`, `PASUBUB`
- **Saturating Operations**: `PSADDUB`, `PSSUBB`, `PSSUBUB`

### 16-bit Operations

- **Simple Addition**: `PADDH`
- **Simple Subtraction**: `PSUBH`
- **Averaging**: `PAADDH`, `PAADDUH`, `PASUBH`, `PASUBUH`
- **Saturating Operations**: `PSADDH`, `PSADDUH`, `PSSUBH`, `PSSUBUH`
- **Cross Operations**: `PASHX`, `PAASHX`, `PSASHX`, `PSAHX`, `PASAHX`, `PSSAHX`

### Comparison and Other Operations

- **Comparison**: `PMSEQH`, `PMSLTH`, `PMSLTUH`, `PMSLEH`, `PMSLEUH`
- **Min/Max**: `PMINH`, `PMINUH`, `PMAXH`, `PMAXUH`
- **Clipping**: `PCLIPH`, `PCLIPUH`
- **Absolute Value**: `PABSH`

---

## Design Details

- **AdderALU**: A 4x8-bit adder used for both 8-bit and 16-bit operations.
- **TwosComplementGenerator**: Generates two's complement for signed subtraction, configurable for 8-bit or 16-bit inputs.
- **SimdMuxHalf**: Implements SIMD comparison, max-min, and absolute value operations.
- **SimdClipHalf**: Handles clipping operations for signed and unsigned values.

---

## Getting Started

### Prerequisites

- **Chisel**: Ensure you have Chisel installed. Refer to the [Chisel Getting Started Guide](https://www.chisel-lang.org/) for installation instructions.
- **sbt**: Scala Build Tool for compiling and testing the design.

### Running the Project

1. Clone the repository
   

2. Generating Verilog for the ALU

- sbt "runMain ALUMain"

- To run the tests for the PextALU module, use:
    sbt "testOnly PextALUWrapperTester"

The generated Verilog file will be located in the generated directory.





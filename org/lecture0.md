# Computer Organization Lecture 0

## Term paper

- Group of 2
- Format
  - 5 pages without references
  - 2 column
  - 1/2” margin
  - single space
  - 10pt times new roman
- Timeline
  - 1/6 Form team
  - 1/20 topic approval
  - 2 ideas - few lines per idea
  - 2/3 supplementary topic approval
  - 4/7 paper due
  - 4/7 and 4/14 presentation or turn in hard copy of powerpoint presentation

## Chapter 2

- ARM Processor
  - RISC (reduced instruction set computer)
  - 5400 page manual
- LEGv8 used in this textbook
  - Subset of ARMv8
  - 32 x 64-bit registers
    - X0-X30, XZR
    - XZR is always 0
- Instructions
  - Add 2 numbers
  - Multiply 2 numbers
  - Copy a number
- Register
  - Memory element that can hold some data
  - Measured in bits
- ALU
  - Arithmetic Logic Unit
- Control Unit
- Memory
  - Byte = 8 bits
  - Word = 4 bytes
  - Double word = 2 words

## Assembly Language

```
ADD X1, X2, X3 // X1 <= X2 + X3
```

A[12] = h + A[8]; // Using double words

```
// Value of h => X21
// Address of A => X22

LDUR X9,[X22,#64] // load 64 bits of data starting at A[8] into X9
ADD X9,X21,X9 // X9 += X21
STUR X9,[X22,#96] // Store X9 to 96/8=12th element of A
```

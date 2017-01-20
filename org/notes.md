# Constant of immediate operands

In high level language

```
x = x + 20;
```

In assembly

```
ADDI X22, X22, #20
```

<aside>
Skip section 2.4
</aside>

# Representing Instructions in the Computer

Any instruction in the LEG8 processor will have 32 bits of code divided into 5
fields.

## Assembly

    ADD X9, X20, X21

## R Format instruction

    rm = register m
    rn = register n
    rd = register d
    shamt = shift amount

    11          5     6      5     5
    opcode      rm    shamt  rn    rd
    00000000000 00000 000000 00000 00000

    ADD         X9           X20   X21
    xxxxxxxxxxx 01001 000000 10100 10101

## Data-Format Instruction

    LDUR X9, [X22, #64]

    11          9         2  5     5
    opcode      addr      op2 rn   rt
    00000000000 000000000 00 00000 00000

    LDUR        #64          X22   X9
    xxxxxxxxxxx 001000000 00 10110 01001

## I-Format Instruction

    ADDI X22, X22, #20

    10          12           5     5
    opcode      immediate    rn    rd
    00000000000 000000000000 00000 00000

    ADDI       #20          X22   X22
    xxxxxxxxxx 000000010100 10110 10110

## Examples

    A[30] = h + A[30] + 1;

    LDUR X9, [X10, #240]
    ADD X9, X21, X9
    ADDI X9, X9, #1
    STUR X9, [X10, #240]

## How does the computer know which format to use?

The computer looks at the first 10 bits of the instruction, which then gives
the computer enough information to make a decision as to how to treat the rest
of the instruction.

# Logical Operations

- Shift operations
  - `x = x << 1;`
    - `1011 => 0110`
- Bitwise operations
  - `x = x | y;`
  - `x = x & y;`


    // X11 = X19 << 4
    LSL X11, X19, #4
    // X11 = X19 >> 4
    LSR X11, X19, #4

    // X9 = X10 & X11
    AND X9, X10, X11

# Instructions for Making Decisions

    if, else, switch, case, while, for

## Compare and branch if zero

    // if X10 == 0 GOTO label5
    ...
    CBZ X10, Label5
    ...
    ...
    Label5: // <= Program will jump here if X10 is 0
    ...
    ...




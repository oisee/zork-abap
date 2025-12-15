# Z80 CPU Emulator for SAP HANA

A Z80 CPU emulator implemented in ABAP, designed for potential AMDP/SQLScript execution on SAP HANA.

## Overview

This emulator provides a foundation for running Z80-based software (CP/M, games, etc.) on SAP systems. The architecture uses lookup tables for flag calculations and a clean bus abstraction for memory/IO.

## Performance Benchmark

```
Z80 Emulator Benchmark (1000 iterations)
=========================================
Program: Hello World (13 opcodes, 112 T-states)

Timing Results:
  Total time: 85,782 µs (0.086 sec)
  Time per run: 85.78 µs
  Time per opcode: 6.60 µs

Performance Metrics:
  Opcodes/second: 151,546
  T-states/second: 1,305,635

Equivalent Clock Speed: ~1.3 MHz

Comparison to real Z80 @ 4MHz:
  Real Z80: 28 µs for Hello World
  This emulator: 85.78 µs
  Ratio: 3.06x slower than 4MHz Z80
```

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    ZCL_Z80_00_CPU                       │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │ Registers   │  │ Fetch/      │  │ Execute     │     │
│  │ AF,BC,DE,HL │  │ Decode      │  │ Opcode      │     │
│  │ IX,IY,SP,PC │  │             │  │             │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
│                          │                              │
│                          ▼                              │
│              ┌─────────────────────┐                    │
│              │   ZIF_Z80_00_BUS    │                    │
│              │  (Memory + I/O)     │                    │
│              └─────────────────────┘                    │
└─────────────────────────────────────────────────────────┘
                           │
                           ▼
              ┌─────────────────────┐
              │   ZCL_Z80_00_BUS    │
              │  (Internal Tables)  │
              │  - 64KB Memory      │
              │  - 256 I/O Ports    │
              │  - Console I/O      │
              └─────────────────────┘
```

## Components (Package: $Z80_00)

| Object | Type | Lines | Purpose |
|--------|------|-------|---------|
| `ZIF_Z80_00_TYPES` | Interface | 90 | Type definitions |
| `ZIF_Z80_00_BUS` | Interface | 45 | Memory/IO abstraction |
| `ZCL_Z80_00_BUS` | Class | 206 | Internal table bus implementation |
| `ZCL_Z80_00_FLAGS` | Class | 191 | Precomputed flag lookup tables |
| `ZCL_Z80_00_CPU` | Class | 218 | Core emulator with opcodes |
| `Z_Z80_TEST_HELLO` | Program | 55 | Hello World test |
| `Z_Z80_BENCHMARK` | Program | 91 | Performance benchmark |

## Implemented Opcodes

| Opcode | Hex | Instruction | Cycles |
|--------|-----|-------------|--------|
| 0 | 00 | NOP | 4 |
| 1 | 01 | LD BC,nn | 10 |
| 17 | 11 | LD DE,nn | 10 |
| 33 | 21 | LD HL,nn | 10 |
| 49 | 31 | LD SP,nn | 10 |
| 50 | 32 | LD (nn),A | 13 |
| 58 | 3A | LD A,(nn) | 13 |
| 62 | 3E | LD A,n | 7 |
| 118 | 76 | HALT | 4 |
| 195 | C3 | JP nn | 10 |
| 201 | C9 | RET | 10 |
| 205 | CD | CALL nn | 17 |
| 211 | D3 | OUT (n),A | 11 |
| 219 | DB | IN A,(n) | 11 |
| 243 | F3 | DI | 4 |
| 251 | FB | EI | 4 |

## Usage Example

```abap
" Create bus and CPU
DATA(lo_bus) = NEW zcl_z80_00_bus( ).
DATA(lo_cpu) = NEW zcl_z80_00_cpu( lo_bus ).

" Load Z80 machine code (Hello World)
DATA(lv_program) = CONV xstring( '3E48D3003E65D3003E6CD3003E6CD3003E6FD3003E21D30076' ).
lo_bus->zif_z80_00_bus~load( iv_addr = 0 iv_data = lv_program ).

" Run until halt or max cycles
lo_cpu->run( iv_max_cycles = 1000 ).

" Get console output
DATA(lv_output) = lo_bus->zif_z80_00_bus~get_output( ).
" lv_output = "Hello!"
```

## Hello World Machine Code

```
Address  Hex      Assembly
-------  -------  ------------------
0x0000   3E 48    LD A, 'H' (0x48)
0x0002   D3 00    OUT (0), A
0x0004   3E 65    LD A, 'e' (0x65)
0x0006   D3 00    OUT (0), A
0x0008   3E 6C    LD A, 'l' (0x6C)
0x000A   D3 00    OUT (0), A
0x000C   3E 6C    LD A, 'l' (0x6C)
0x000E   D3 00    OUT (0), A
0x0010   3E 6F    LD A, 'o' (0x6F)
0x0012   D3 00    OUT (0), A
0x0014   3E 21    LD A, '!' (0x21)
0x0016   D3 00    OUT (0), A
0x0018   76       HALT
```

## Console I/O Ports

| Port | Direction | Function |
|------|-----------|----------|
| 0 | IN | Read character from input buffer |
| 0 | OUT | Write character to output buffer |
| 1 | IN | Check input status (0=empty, 255=ready) |

## Future Enhancements

1. **Complete Opcode Set** - Port remaining ~200 opcodes from cpm-abap
2. **Prefix Handlers** - CB (bit ops), DD (IX), ED (extended), FD (IY)
3. **AMDP Version** - Run execution loop in HANA SQLScript
4. **APC Integration** - WebSocket terminal for interactive use
5. **CP/M BDOS** - Operating system calls for running .COM files

## References

- Z80 instruction set from cpm-abap project
- Original Z80 ran at 2.5-4 MHz (this emulator: ~1.3 MHz equivalent)

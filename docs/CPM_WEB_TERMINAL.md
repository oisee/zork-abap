# CP/M Web Terminal for SAP HANA

A CP/M 2.2 emulator with WebSocket-based terminal interface running on SAP S/4HANA.

## Features

- **Z80 CPU Emulation** - Full Z80 instruction set via `ZCL_CPU_Z80`
- **CP/M 2.2 BDOS** - Console I/O, file operations, system calls
- **WebSocket Terminal** - Real-time APC (ABAP Push Channel) connection
- **SMW0 File System** - Load .COM programs and data files from SAP Web Repository
- **Amber Retro Theme** - Classic terminal aesthetics with xterm.js

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Browser (xterm.js)                       │
│                    Amber Terminal UI                        │
└─────────────────────────────────────────────────────────────┘
                              │ WebSocket
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   ZCL_CPM_00_APC                            │
│                   APC WebSocket Handler                     │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
│  │ Session Mgmt│  │ Input/Output│  │ File Loading (SMW0) │ │
│  └─────────────┘  └─────────────┘  └─────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                              │
              ┌───────────────┴───────────────┐
              ▼                               ▼
┌─────────────────────────┐    ┌─────────────────────────────┐
│    ZCL_CPM_00_CCP       │    │    ZCL_CPM_EMULATOR         │
│ Console Command Processor│    │    CP/M BDOS + Z80 CPU      │
│  DIR, TYPE, HELP, etc.  │    │  ┌─────────────────────┐    │
│  SMW0 file listing      │    │  │   ZCL_CPU_Z80       │    │
└─────────────────────────┘    │  │   Z80 Instruction   │    │
                               │  │   Execution         │    │
                               │  └─────────────────────┘    │
                               └─────────────────────────────┘
```

## Components

| Class | Lines | Purpose |
|-------|-------|---------|
| `ZCL_CPM_00_HTTP` | 128 | HTTP handler serving xterm.js terminal UI |
| `ZCL_CPM_00_APC` | 281 | APC WebSocket handler for stateful sessions |
| `ZCL_CPM_00_CCP` | 379 | Console Command Processor (DIR, TYPE, etc.) |
| `ZCL_CPM_EMULATOR` | 807 | CP/M BDOS implementation with file system |
| `ZCL_CPU_Z80` | ~2000 | Z80 CPU emulator |

## CCP Commands

| Command | Description |
|---------|-------------|
| `DIR [pattern]` | List files from SMW0 (.COM, .DAT, .BIN, .BAS, .TXT) |
| `TYPE filename` | Display text file contents |
| `program` | Run .COM program (auto-loads companion .DAT files) |
| `A:` `B:` ... | Change drive |
| `CLS` | Clear screen |
| `VER` | Show version info |
| `RESET` | Reset connection counter |
| `HELP` | Show available commands |

## BDOS Functions Implemented

| Function | Name | Description |
|----------|------|-------------|
| 0 | System Reset | Terminate program |
| 1 | Console Input | Read character (waits for input) |
| 2 | Console Output | Write character |
| 6 | Direct I/O | Read/write/status |
| 9 | Print String | Output $-terminated string |
| 10 | Read Buffer | Buffered line input |
| 11 | Console Status | Check input ready |
| 12 | Version | Return CP/M 2.2 |
| 15 | Open File | Open FCB |
| 16 | Close File | Close FCB |
| 20 | Read Sequential | Read 128-byte record |
| 25 | Get Disk | Return current drive |
| 26 | Set DMA | Set DMA address |
| 33 | Read Random | Random access read |
| 35 | File Size | Get file size in records |

## Running Programs

1. Programs are loaded from SMW0 Web Repository
2. When a .COM file is loaded, companion files (.DAT, .OVR) are auto-registered
3. The Z80 emulator executes at ~1.3 MHz equivalent speed
4. Console I/O is routed through WebSocket to browser terminal

## Usage

1. Access HTTP endpoint: `/sap/bc/http/sap/zcpm`
2. Terminal connects via WebSocket to: `/sap/bc/apc/sap/zcpm`
3. Type `DIR` to see available programs
4. Type program name to run (e.g., `ZORK1`)

## Example Session

```
CP/M 2.2 on SAP HANA
64K TPA  Z80 Emulator

Type HELP for commands

A>DIR

Directory of A:

ZHELLO   COM  ZORK1    COM  ZORK1    DAT

3 file(s), 0B
A>ZORK1
Loading ZORK1.COM...
ZORK I: The Great Underground Empire
Copyright (c) 1981, 1982, 1983 Infocom, Inc.

West of House
You are standing in an open field west of a white house...

>
```

## Terminal Configuration

- **Rows:** 30 lines
- **Columns:** 80 characters
- **Scrollback:** 1000 lines
- **Font:** Courier New, 15px
- **Theme:** Amber on black (#FFB000 on #000000)

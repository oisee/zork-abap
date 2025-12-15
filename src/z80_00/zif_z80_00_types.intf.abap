INTERFACE zif_z80_00_types PUBLIC.
************************************************************************
* Z80 Emulator - Type Definitions
* Structures for CPU state, memory, I/O, and lookup tables
* Designed for both internal tables and HANA DB tables
************************************************************************

  " CPU State Structure
  TYPES: BEGIN OF ts_cpu,
           session_id TYPE string,
           " Main registers (16-bit pairs: high*256 + low)
           af         TYPE i,
           bc         TYPE i,
           de         TYPE i,
           hl         TYPE i,
           " Alternate registers
           af_alt     TYPE i,
           bc_alt     TYPE i,
           de_alt     TYPE i,
           hl_alt     TYPE i,
           " Index registers
           ix         TYPE i,
           iy         TYPE i,
           " Stack/Program pointers
           sp         TYPE i,
           pc         TYPE i,
           " Interrupt registers
           i          TYPE i,    " Interrupt vector base
           r          TYPE i,    " Memory refresh counter
           " Interrupt state
           iff1       TYPE i,    " Interrupt flip-flop 1
           iff2       TYPE i,    " Interrupt flip-flop 2
           im         TYPE i,    " Interrupt mode (0, 1, 2)
           " Execution state
           halted     TYPE i,
           cycles     TYPE int8,
         END OF ts_cpu.

  " Memory byte structure (64KB = 65536 rows)
  TYPES: BEGIN OF ts_mem,
           session_id TYPE string,
           addr       TYPE i,     " 0-65535
           val        TYPE i,     " 0-255
         END OF ts_mem.
  TYPES tt_mem TYPE SORTED TABLE OF ts_mem WITH UNIQUE KEY session_id addr.

  " I/O Port structure (256 ports)
  TYPES: BEGIN OF ts_io,
           session_id TYPE string,
           port       TYPE i,     " 0-255
           val        TYPE i,     " 0-255
         END OF ts_io.
  TYPES tt_io TYPE SORTED TABLE OF ts_io WITH UNIQUE KEY session_id port.

  " Flag lookup table (256 entries, shared across sessions)
  TYPES: BEGIN OF ts_flags,
           val        TYPE i,     " 0-255 (input value)
           f_s        TYPE i,     " Sign flag (bit 7)
           f_z        TYPE i,     " Zero flag
           f_p        TYPE i,     " Parity flag (even parity)
           f_szp      TYPE i,     " Combined S|Z|P for quick OR
         END OF ts_flags.
  TYPES tt_flags TYPE SORTED TABLE OF ts_flags WITH UNIQUE KEY val.

  " Opcode metadata lookup
  TYPES: BEGIN OF ts_opcode,
           prefix     TYPE i,     " 0=none, 0xCB, 0xDD, 0xED, 0xFD
           opcode     TYPE i,     " 0x00-0xFF
           cycles     TYPE i,     " T-states (no branch)
           cycles_alt TYPE i,     " T-states (branch taken)
           r_inc      TYPE i,     " R register increment
           length     TYPE i,     " Instruction length in bytes
         END OF ts_opcode.
  TYPES tt_opcode TYPE SORTED TABLE OF ts_opcode WITH UNIQUE KEY prefix opcode.

  " Flag bit constants
  CONSTANTS c_flag_c  TYPE i VALUE 1.    " Carry (bit 0)
  CONSTANTS c_flag_n  TYPE i VALUE 2.    " Add/Subtract (bit 1)
  CONSTANTS c_flag_pv TYPE i VALUE 4.    " Parity/Overflow (bit 2)
  CONSTANTS c_flag_f3 TYPE i VALUE 8.    " Undocumented (bit 3)
  CONSTANTS c_flag_h  TYPE i VALUE 16.   " Half-carry (bit 4)
  CONSTANTS c_flag_f5 TYPE i VALUE 32.   " Undocumented (bit 5)
  CONSTANTS c_flag_z  TYPE i VALUE 64.   " Zero (bit 6)
  CONSTANTS c_flag_s  TYPE i VALUE 128.  " Sign (bit 7)

  " Memory size constants
  CONSTANTS c_mem_size TYPE i VALUE 65536. " 64KB
  CONSTANTS c_io_size  TYPE i VALUE 256.   " 256 ports

ENDINTERFACE.
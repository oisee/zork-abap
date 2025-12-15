CLASS zcl_z80_00_flags DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* Z80 Flag Lookup Tables
* Precomputed flag values for fast lookup during execution
* Sign, Zero, Parity flags for all 256 byte values
************************************************************************

  PUBLIC SECTION.
    " Flag constants
    CONSTANTS c_flag_c  TYPE i VALUE 1.    " Carry
    CONSTANTS c_flag_n  TYPE i VALUE 2.    " Add/Subtract
    CONSTANTS c_flag_pv TYPE i VALUE 4.    " Parity/Overflow
    CONSTANTS c_flag_f3 TYPE i VALUE 8.    " Undocumented bit 3
    CONSTANTS c_flag_h  TYPE i VALUE 16.   " Half-carry
    CONSTANTS c_flag_f5 TYPE i VALUE 32.   " Undocumented bit 5
    CONSTANTS c_flag_z  TYPE i VALUE 64.   " Zero
    CONSTANTS c_flag_s  TYPE i VALUE 128.  " Sign

    CLASS-METHODS class_constructor.

    " Get Sign + Zero flags for a value
    CLASS-METHODS get_sz
      IMPORTING iv_val        TYPE i
      RETURNING VALUE(rv_flags) TYPE i.

    " Get Sign + Zero + Parity flags for a value
    CLASS-METHODS get_szp
      IMPORTING iv_val        TYPE i
      RETURNING VALUE(rv_flags) TYPE i.

    " Get parity flag only (0 = odd, 4 = even)
    CLASS-METHODS get_parity
      IMPORTING iv_val        TYPE i
      RETURNING VALUE(rv_p)   TYPE i.

    " Calculate INC flags (preserves C, sets S,Z,H,PV,N=0)
    CLASS-METHODS get_inc_flags
      IMPORTING iv_val        TYPE i   " value AFTER increment
      RETURNING VALUE(rv_flags) TYPE i.

    " Calculate DEC flags (preserves C, sets S,Z,H,PV,N=1)
    CLASS-METHODS get_dec_flags
      IMPORTING iv_val        TYPE i   " value AFTER decrement
      RETURNING VALUE(rv_flags) TYPE i.

  PRIVATE SECTION.
    CLASS-DATA mt_sz  TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    CLASS-DATA mt_szp TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    CLASS-DATA mt_p   TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    CLASS-METHODS calc_parity
      IMPORTING iv_val      TYPE i
      RETURNING VALUE(rv_p) TYPE i.

ENDCLASS.


CLASS zcl_z80_00_flags IMPLEMENTATION.

  METHOD class_constructor.
    DATA lv_i TYPE i.
    DATA lv_flags TYPE i.
    DATA lv_p TYPE i.

    " Initialize lookup tables for values 0-255
    DO 256 TIMES.
      lv_i = sy-index - 1.

      " Sign flag (bit 7)
      DATA(lv_s) = COND i( WHEN lv_i >= 128 THEN c_flag_s ELSE 0 ).

      " Zero flag
      DATA(lv_z) = COND i( WHEN lv_i = 0 THEN c_flag_z ELSE 0 ).

      " Parity (even = 1)
      lv_p = calc_parity( lv_i ).

      " Store SZ flags
      lv_flags = lv_s + lv_z.
      APPEND lv_flags TO mt_sz.

      " Store SZP flags
      lv_flags = lv_s + lv_z + lv_p.
      APPEND lv_flags TO mt_szp.

      " Store parity only
      APPEND lv_p TO mt_p.
    ENDDO.
  ENDMETHOD.

  METHOD calc_parity.
    " Count 1-bits in value, return c_flag_pv if even
    DATA lv_val TYPE i.
    DATA lv_count TYPE i.
    DATA lv_bit TYPE i.

    lv_val = iv_val MOD 256.
    lv_count = 0.

    DO 8 TIMES.
      lv_bit = lv_val MOD 2.
      lv_count = lv_count + lv_bit.
      lv_val = lv_val DIV 2.
    ENDDO.

    " Even parity = flag set
    rv_p = COND #( WHEN lv_count MOD 2 = 0 THEN c_flag_pv ELSE 0 ).
  ENDMETHOD.

  METHOD get_sz.
    DATA lv_idx TYPE i.
    lv_idx = ( iv_val MOD 256 ) + 1.
    READ TABLE mt_sz INTO rv_flags INDEX lv_idx.
    IF sy-subrc <> 0.
      rv_flags = 0.
    ENDIF.
  ENDMETHOD.

  METHOD get_szp.
    DATA lv_idx TYPE i.
    lv_idx = ( iv_val MOD 256 ) + 1.
    READ TABLE mt_szp INTO rv_flags INDEX lv_idx.
    IF sy-subrc <> 0.
      rv_flags = 0.
    ENDIF.
  ENDMETHOD.

  METHOD get_parity.
    DATA lv_idx TYPE i.
    lv_idx = ( iv_val MOD 256 ) + 1.
    READ TABLE mt_p INTO rv_p INDEX lv_idx.
    IF sy-subrc <> 0.
      rv_p = 0.
    ENDIF.
  ENDMETHOD.

  METHOD get_inc_flags.
    " INC sets: S, Z, H (if low nibble was 0xF), PV (if was 0x7F), N=0
    DATA lv_val TYPE i.
    DATA lv_flags TYPE i.

    lv_val = iv_val MOD 256.

    " Start with S and Z
    lv_flags = get_sz( lv_val ).

    " Half-carry: set if low nibble is now 0 (was 0xF before inc)
    IF lv_val MOD 16 = 0.
      lv_flags = lv_flags + c_flag_h.
    ENDIF.

    " Overflow: set if result is 0x80 (was 0x7F, positive became negative)
    IF lv_val = 128.
      lv_flags = lv_flags + c_flag_pv.
    ENDIF.

    rv_flags = lv_flags.
  ENDMETHOD.

  METHOD get_dec_flags.
    " DEC sets: S, Z, H (if low nibble is 0xF), PV (if was 0x80), N=1
    DATA lv_val TYPE i.
    DATA lv_flags TYPE i.

    lv_val = iv_val MOD 256.

    " Start with S and Z
    lv_flags = get_sz( lv_val ).

    " N flag is always set for DEC
    lv_flags = lv_flags + c_flag_n.

    " Half-carry: set if low nibble is 0xF (borrowed from high nibble)
    IF lv_val MOD 16 = 15.
      lv_flags = lv_flags + c_flag_h.
    ENDIF.

    " Overflow: set if result is 0x7F (was 0x80, negative became positive)
    IF lv_val = 127.
      lv_flags = lv_flags + c_flag_pv.
    ENDIF.

    rv_flags = lv_flags.
  ENDMETHOD.

ENDCLASS.

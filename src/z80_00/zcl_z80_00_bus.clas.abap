CLASS zcl_z80_00_bus DEFINITION
  PUBLIC
  CREATE PUBLIC.

************************************************************************
* Z80 Bus Implementation using Internal Tables
* Fast in-memory implementation for immediate use
************************************************************************

  PUBLIC SECTION.
    INTERFACES zif_z80_00_bus.

    METHODS constructor.

    METHODS get_memory_dump
      IMPORTING iv_start        TYPE i DEFAULT 0
                iv_length       TYPE i DEFAULT 256
      RETURNING VALUE(rv_dump)  TYPE string.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_byte,
             addr TYPE i,
             val  TYPE i,
           END OF ts_byte.
    TYPES tt_memory TYPE SORTED TABLE OF ts_byte WITH UNIQUE KEY addr.

    DATA mt_memory TYPE tt_memory.
    DATA mt_io     TYPE tt_memory.

    DATA mv_output    TYPE string.
    DATA mv_input     TYPE string.
    DATA mv_input_pos TYPE i.

    METHODS init_memory.

ENDCLASS.


CLASS zcl_z80_00_bus IMPLEMENTATION.

  METHOD constructor.
    init_memory( ).
  ENDMETHOD.

  METHOD init_memory.
    CLEAR mt_memory.
    CLEAR mt_io.
    mv_output = ``.
    mv_input = ``.
    mv_input_pos = 0.
  ENDMETHOD.

  METHOD zif_z80_00_bus~read_mem.
    DATA lv_addr TYPE i.
    DATA ls_byte TYPE ts_byte.

    lv_addr = iv_addr MOD 65536.
    IF lv_addr < 0.
      lv_addr = lv_addr + 65536.
    ENDIF.

    READ TABLE mt_memory INTO ls_byte WITH KEY addr = lv_addr.
    IF sy-subrc = 0.
      rv_val = ls_byte-val.
    ELSE.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.

  METHOD zif_z80_00_bus~write_mem.
    DATA lv_addr TYPE i.
    DATA ls_byte TYPE ts_byte.

    lv_addr = iv_addr MOD 65536.
    IF lv_addr < 0.
      lv_addr = lv_addr + 65536.
    ENDIF.

    ls_byte-addr = lv_addr.
    ls_byte-val = iv_val MOD 256.

    " Upsert: try insert, if exists then modify
    INSERT ls_byte INTO TABLE mt_memory.
    IF sy-subrc = 4.
      MODIFY TABLE mt_memory FROM ls_byte.
    ENDIF.
  ENDMETHOD.

  METHOD zif_z80_00_bus~read_io.
    DATA lv_port TYPE i.
    DATA ls_byte TYPE ts_byte.
    DATA lv_xstr TYPE xstring.
    DATA lv_x TYPE x LENGTH 1.

    lv_port = iv_port MOD 256.

    " Console input on port 0
    IF lv_port = 0.
      IF mv_input_pos < strlen( mv_input ).
        DATA(lv_char) = mv_input+mv_input_pos(1).
        mv_input_pos = mv_input_pos + 1.
        lv_xstr = cl_abap_conv_codepage=>create_out( )->convert( lv_char ).
        lv_x = lv_xstr+0(1).
        rv_val = lv_x.
        RETURN.
      ELSE.
        rv_val = 0.
        RETURN.
      ENDIF.
    ENDIF.

    " Console status on port 1
    IF lv_port = 1.
      IF mv_input_pos < strlen( mv_input ).
        rv_val = 255.
      ELSE.
        rv_val = 0.
      ENDIF.
      RETURN.
    ENDIF.

    READ TABLE mt_io INTO ls_byte WITH KEY addr = lv_port.
    IF sy-subrc = 0.
      rv_val = ls_byte-val.
    ELSE.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.

  METHOD zif_z80_00_bus~write_io.
    DATA lv_port TYPE i.
    DATA ls_byte TYPE ts_byte.
    DATA lv_char TYPE string.
    DATA lv_xval TYPE x LENGTH 1.
    DATA lv_xstr TYPE xstring.

    lv_port = iv_port MOD 256.

    " Console output on port 0
    IF lv_port = 0.
      lv_xval = iv_val MOD 256.
      lv_xstr = lv_xval.
      lv_char = cl_abap_conv_codepage=>create_in( )->convert( lv_xstr ).
      mv_output = mv_output && lv_char.
      RETURN.
    ENDIF.

    ls_byte-addr = lv_port.
    ls_byte-val = iv_val MOD 256.
    MODIFY TABLE mt_io FROM ls_byte.
  ENDMETHOD.

  METHOD zif_z80_00_bus~load.
    DATA lv_i TYPE i.
    DATA lv_len TYPE i.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_addr TYPE i.
    DATA lv_val TYPE i.

    lv_len = xstrlen( iv_data ).
    lv_addr = iv_addr.

    DO lv_len TIMES.
      lv_i = sy-index - 1.
      lv_byte = iv_data+lv_i(1).
      lv_val = lv_byte.
      zif_z80_00_bus~write_mem( iv_addr = lv_addr iv_val = lv_val ).
      lv_addr = lv_addr + 1.
    ENDDO.
  ENDMETHOD.

  METHOD zif_z80_00_bus~is_input_ready.
    rv_ready = xsdbool( mv_input_pos < strlen( mv_input ) ).
  ENDMETHOD.

  METHOD zif_z80_00_bus~get_output.
    rv_output = mv_output.
  ENDMETHOD.

  METHOD zif_z80_00_bus~clear_output.
    CLEAR mv_output.
  ENDMETHOD.

  METHOD zif_z80_00_bus~provide_input.
    mv_input = mv_input && iv_text.
  ENDMETHOD.

  METHOD get_memory_dump.
    DATA lv_i TYPE i.
    DATA lv_val TYPE i.
    DATA lv_line TYPE string.

    rv_dump = ``.
    DO iv_length TIMES.
      lv_i = iv_start + sy-index - 1.
      lv_val = zif_z80_00_bus~read_mem( lv_i ).
      lv_line = COND #( WHEN lv_val < 16 THEN |0{ lv_val }| ELSE |{ lv_val }| ).
      rv_dump = rv_dump && lv_line && ` `.
      IF sy-index MOD 16 = 0.
        rv_dump = rv_dump && cl_abap_char_utilities=>newline.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.

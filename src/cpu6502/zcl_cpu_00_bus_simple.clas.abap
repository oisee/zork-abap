CLASS zcl_cpu_00_bus_simple DEFINITION
  PUBLIC
  CREATE PUBLIC.
************************************************************************
* Simple 6502 Bus Implementation
* - 64KB RAM
* - Memory-mapped I/O at $F001 (input) and $F002 (output)
* - Suitable for running MS BASIC and similar programs
************************************************************************

  PUBLIC SECTION.
    INTERFACES zif_cpu_00_bus.

    " I/O addresses
    CONSTANTS: c_io_input  TYPE i VALUE 61441,  " $F001
               c_io_output TYPE i VALUE 61442.  " $F002

    " Memory table type for debugging
    TYPES tt_memory TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    METHODS constructor.

    " Direct memory access for debugging
    METHODS get_memory
      RETURNING VALUE(rt_mem) TYPE tt_memory.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: mt_mem         TYPE tt_memory,  " 64KB memory
          mv_input_buf   TYPE string,     " Input queue
          mv_input_pos   TYPE i,          " Position in input
          mv_output_buf  TYPE string,     " Output accumulator
          mv_waiting     TYPE abap_bool.  " Waiting for input

ENDCLASS.



CLASS zcl_cpu_00_bus_simple IMPLEMENTATION.

  METHOD constructor.
    " Initialize 64KB of zero-filled RAM
    DO 65536 TIMES.
      APPEND 0 TO mt_mem.
    ENDDO.
    mv_input_pos = 0.
    mv_waiting = abap_false.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~read.
    " Bounds check
    IF iv_addr < 0 OR iv_addr > 65535.
      rv_val = 0.
      RETURN.
    ENDIF.

    " I/O: Input character
    IF iv_addr = c_io_input.
      IF mv_input_pos < strlen( mv_input_buf ).
        " Return next character from input buffer
        DATA lv_char TYPE c LENGTH 1.
        lv_char = mv_input_buf+mv_input_pos(1).
        mv_input_pos = mv_input_pos + 1.
        " Convert to ASCII code
        DATA lv_hex TYPE x LENGTH 2.
        lv_hex = cl_abap_conv_out_ce=>uccp( lv_char ).
        rv_val = lv_hex.
        mv_waiting = abap_false.
      ELSE.
        " No input available - return 0 and set waiting flag
        rv_val = 0.
        mv_waiting = abap_true.
      ENDIF.
      RETURN.
    ENDIF.

    " Regular memory read
    DATA lv_idx TYPE i.
    lv_idx = iv_addr + 1.  " ABAP tables are 1-indexed
    READ TABLE mt_mem INDEX lv_idx INTO rv_val.
    IF sy-subrc <> 0.
      rv_val = 0.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~write.
    " Bounds check
    IF iv_addr < 0 OR iv_addr > 65535.
      RETURN.
    ENDIF.

    " Ensure value is 8-bit
    DATA lv_val TYPE i.
    lv_val = iv_val MOD 256.
    IF lv_val < 0.
      lv_val = lv_val + 256.
    ENDIF.

    " I/O: Output character
    IF iv_addr = c_io_output.
      " Convert ASCII code to character
      DATA lv_char TYPE c LENGTH 1.
      DATA lv_hex2 TYPE x LENGTH 2.
      lv_hex2 = lv_val.
      lv_char = cl_abap_conv_in_ce=>uccp( lv_hex2 ).
      mv_output_buf = mv_output_buf && lv_char.
      RETURN.
    ENDIF.

    " Regular memory write
    DATA lv_idx TYPE i.
    lv_idx = iv_addr + 1.  " ABAP tables are 1-indexed
    MODIFY mt_mem INDEX lv_idx FROM lv_val.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~load.
    " Load binary data into memory starting at iv_addr
    DATA: lv_len  TYPE i,
          lv_i    TYPE i,
          lv_byte TYPE x LENGTH 1,
          lv_val  TYPE i,
          lv_addr TYPE i.

    lv_len = xstrlen( iv_data ).
    DO lv_len TIMES.
      lv_i = sy-index - 1.
      lv_byte = iv_data+lv_i(1).
      lv_val = lv_byte.
      lv_addr = iv_addr + lv_i.
      zif_cpu_00_bus~write( iv_addr = lv_addr iv_val = lv_val ).
    ENDDO.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~is_input_ready.
    rv_ready = xsdbool( mv_input_pos < strlen( mv_input_buf ) ).
  ENDMETHOD.

  METHOD zif_cpu_00_bus~get_output.
    rv_output = mv_output_buf.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~clear_output.
    CLEAR mv_output_buf.
  ENDMETHOD.

  METHOD zif_cpu_00_bus~provide_input.
    " Append to input buffer (with newline for line-based input)
    mv_input_buf = mv_input_buf && iv_text && cl_abap_char_utilities=>cr_lf.
    mv_waiting = abap_false.
  ENDMETHOD.

  METHOD get_memory.
    rt_mem = mt_mem.
  ENDMETHOD.

ENDCLASS.

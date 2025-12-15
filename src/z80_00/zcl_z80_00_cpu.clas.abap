CLASS zcl_z80_00_cpu DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_status,
             af      TYPE i,
             bc      TYPE i,
             de      TYPE i,
             hl      TYPE i,
             af_alt  TYPE i,
             bc_alt  TYPE i,
             de_alt  TYPE i,
             hl_alt  TYPE i,
             ix      TYPE i,
             iy      TYPE i,
             sp      TYPE i,
             pc      TYPE i,
             i       TYPE i,
             r       TYPE i,
             iff1    TYPE abap_bool,
             iff2    TYPE abap_bool,
             im      TYPE i,
             cycles  TYPE int8,
             halted  TYPE abap_bool,
           END OF ts_status.

    METHODS constructor IMPORTING io_bus TYPE REF TO zif_z80_00_bus.
    METHODS reset.
    METHODS step RETURNING VALUE(rv_cycles) TYPE i.
    METHODS run IMPORTING iv_max_cycles TYPE i DEFAULT 100000.
    METHODS get_status RETURNING VALUE(rs_status) TYPE ts_status.
    METHODS is_halted RETURNING VALUE(rv_halted) TYPE abap_bool.
    METHODS provide_input IMPORTING iv_text TYPE string.
    METHODS get_pc RETURNING VALUE(rv_pc) TYPE i.
    METHODS get_sp RETURNING VALUE(rv_sp) TYPE i.
    METHODS get_af RETURNING VALUE(rv_af) TYPE i.
    METHODS get_bc RETURNING VALUE(rv_bc) TYPE i.
    METHODS get_de RETURNING VALUE(rv_de) TYPE i.
    METHODS get_hl RETURNING VALUE(rv_hl) TYPE i.

  PRIVATE SECTION.
    DATA mo_bus TYPE REF TO zif_z80_00_bus.
    DATA mv_af TYPE i.
    DATA mv_bc TYPE i.
    DATA mv_de TYPE i.
    DATA mv_hl TYPE i.
    DATA mv_af_alt TYPE i.
    DATA mv_bc_alt TYPE i.
    DATA mv_de_alt TYPE i.
    DATA mv_hl_alt TYPE i.
    DATA mv_ix TYPE i.
    DATA mv_iy TYPE i.
    DATA mv_sp TYPE i.
    DATA mv_pc TYPE i.
    DATA mv_i  TYPE i.
    DATA mv_r  TYPE i.
    DATA mv_iff1 TYPE abap_bool.
    DATA mv_iff2 TYPE abap_bool.
    DATA mv_im   TYPE i.
    DATA mv_cycles TYPE int8.
    DATA mv_halted TYPE abap_bool.

    METHODS read8 IMPORTING iv_addr TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS write8 IMPORTING iv_addr TYPE i iv_val TYPE i.
    METHODS fetch8 RETURNING VALUE(rv_val) TYPE i.
    METHODS fetch16 RETURNING VALUE(rv_val) TYPE i.
    METHODS push16 IMPORTING iv_val TYPE i.
    METHODS pop16 RETURNING VALUE(rv_val) TYPE i.
    METHODS get_a RETURNING VALUE(rv_val) TYPE i.
    METHODS set_a IMPORTING iv_val TYPE i.
    METHODS get_f RETURNING VALUE(rv_val) TYPE i.
    METHODS set_f IMPORTING iv_val TYPE i.
    METHODS set_flag IMPORTING iv_flag TYPE i iv_val TYPE abap_bool.
    METHODS alu_inc8 IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
    METHODS alu_dec8 IMPORTING iv_val TYPE i RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.


CLASS zcl_z80_00_cpu IMPLEMENTATION.
  METHOD constructor.
    mo_bus = io_bus.
    reset( ).
  ENDMETHOD.

  METHOD reset.
    mv_af = 0. mv_bc = 0. mv_de = 0. mv_hl = 0.
    mv_af_alt = 0. mv_bc_alt = 0. mv_de_alt = 0. mv_hl_alt = 0.
    mv_ix = 0. mv_iy = 0. mv_sp = 65535. mv_pc = 0.
    mv_i = 0. mv_r = 0.
    mv_iff1 = abap_false. mv_iff2 = abap_false. mv_im = 0.
    mv_cycles = 0. mv_halted = abap_false.
  ENDMETHOD.

  METHOD step.
    DATA lv_op TYPE i.
    DATA lv_val TYPE i.
    DATA lv_addr TYPE i.

    IF mv_halted = abap_true.
      rv_cycles = 4.
      RETURN.
    ENDIF.

    lv_op = fetch8( ).
    mv_r = ( mv_r + 1 ) MOD 128.

    CASE lv_op.
      WHEN 0.    rv_cycles = 4.
      WHEN 1.    mv_bc = fetch16( ). rv_cycles = 10.
      WHEN 17.   mv_de = fetch16( ). rv_cycles = 10.
      WHEN 33.   mv_hl = fetch16( ). rv_cycles = 10.
      WHEN 49.   mv_sp = fetch16( ). rv_cycles = 10.
      WHEN 50.   lv_addr = fetch16( ). write8( iv_addr = lv_addr iv_val = get_a( ) ). rv_cycles = 13.
      WHEN 58.   lv_addr = fetch16( ). set_a( read8( lv_addr ) ). rv_cycles = 13.
      WHEN 62.   set_a( fetch8( ) ). rv_cycles = 7.
      WHEN 118.  mv_halted = abap_true. rv_cycles = 4.
      WHEN 195.  mv_pc = fetch16( ). rv_cycles = 10.
      WHEN 201.  mv_pc = pop16( ). rv_cycles = 10.
      WHEN 205.  lv_addr = fetch16( ). push16( mv_pc ). mv_pc = lv_addr. rv_cycles = 17.
      WHEN 211.  lv_val = fetch8( ). mo_bus->write_io( iv_port = lv_val iv_val = get_a( ) ). rv_cycles = 11.
      WHEN 219.  lv_val = fetch8( ). set_a( mo_bus->read_io( lv_val ) ). rv_cycles = 11.
      WHEN 243.  mv_iff1 = abap_false. mv_iff2 = abap_false. rv_cycles = 4.
      WHEN 251.  mv_iff1 = abap_true. mv_iff2 = abap_true. rv_cycles = 4.
      WHEN OTHERS. rv_cycles = 4.
    ENDCASE.
    mv_cycles = mv_cycles + rv_cycles.
  ENDMETHOD.

  METHOD run.
    WHILE mv_cycles < iv_max_cycles AND mv_halted = abap_false.
      step( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD get_status.
    rs_status-af = mv_af. rs_status-bc = mv_bc.
    rs_status-de = mv_de. rs_status-hl = mv_hl.
    rs_status-af_alt = mv_af_alt. rs_status-bc_alt = mv_bc_alt.
    rs_status-de_alt = mv_de_alt. rs_status-hl_alt = mv_hl_alt.
    rs_status-ix = mv_ix. rs_status-iy = mv_iy.
    rs_status-sp = mv_sp. rs_status-pc = mv_pc.
    rs_status-i = mv_i. rs_status-r = mv_r.
    rs_status-iff1 = mv_iff1. rs_status-iff2 = mv_iff2.
    rs_status-im = mv_im. rs_status-cycles = mv_cycles.
    rs_status-halted = mv_halted.
  ENDMETHOD.

  METHOD is_halted. rv_halted = mv_halted. ENDMETHOD.
  METHOD provide_input. mo_bus->provide_input( iv_text ). ENDMETHOD.
  METHOD get_pc. rv_pc = mv_pc. ENDMETHOD.
  METHOD get_sp. rv_sp = mv_sp. ENDMETHOD.
  METHOD get_af. rv_af = mv_af. ENDMETHOD.
  METHOD get_bc. rv_bc = mv_bc. ENDMETHOD.
  METHOD get_de. rv_de = mv_de. ENDMETHOD.
  METHOD get_hl. rv_hl = mv_hl. ENDMETHOD.

  METHOD read8. rv_val = mo_bus->read_mem( iv_addr ). ENDMETHOD.
  METHOD write8. mo_bus->write_mem( iv_addr = iv_addr iv_val = iv_val ). ENDMETHOD.

  METHOD fetch8.
    rv_val = read8( mv_pc ).
    mv_pc = ( mv_pc + 1 ) MOD 65536.
  ENDMETHOD.

  METHOD fetch16.
    DATA(lv_lo) = fetch8( ).
    DATA(lv_hi) = fetch8( ).
    rv_val = lv_hi * 256 + lv_lo.
  ENDMETHOD.

  METHOD push16.
    mv_sp = ( mv_sp - 1 ) MOD 65536.
    write8( iv_addr = mv_sp iv_val = iv_val DIV 256 ).
    mv_sp = ( mv_sp - 1 ) MOD 65536.
    write8( iv_addr = mv_sp iv_val = iv_val MOD 256 ).
  ENDMETHOD.

  METHOD pop16.
    DATA(lv_lo) = read8( mv_sp ).
    mv_sp = ( mv_sp + 1 ) MOD 65536.
    DATA(lv_hi) = read8( mv_sp ).
    mv_sp = ( mv_sp + 1 ) MOD 65536.
    rv_val = lv_hi * 256 + lv_lo.
  ENDMETHOD.

  METHOD get_a. rv_val = mv_af DIV 256. ENDMETHOD.
  METHOD set_a. mv_af = iv_val * 256 + ( mv_af MOD 256 ). ENDMETHOD.
  METHOD get_f. rv_val = mv_af MOD 256. ENDMETHOD.
  METHOD set_f. mv_af = ( mv_af DIV 256 ) * 256 + ( iv_val MOD 256 ). ENDMETHOD.

  METHOD set_flag.
    DATA(lv_f) = get_f( ).
    IF iv_val = abap_true.
      IF lv_f MOD ( iv_flag * 2 ) DIV iv_flag = 0.
        lv_f = lv_f + iv_flag.
      ENDIF.
    ELSE.
      IF lv_f MOD ( iv_flag * 2 ) DIV iv_flag = 1.
        lv_f = lv_f - iv_flag.
      ENDIF.
    ENDIF.
    set_f( lv_f ).
  ENDMETHOD.

  METHOD alu_inc8.
    rv_result = ( iv_val + 1 ) MOD 256.
    DATA(lv_carry) = get_f( ) MOD 2.
    set_f( zcl_z80_00_flags=>get_inc_flags( rv_result ) + lv_carry ).
  ENDMETHOD.

  METHOD alu_dec8.
    rv_result = iv_val - 1.
    IF rv_result < 0. rv_result = 255. ENDIF.
    DATA(lv_carry) = get_f( ) MOD 2.
    set_f( zcl_z80_00_flags=>get_dec_flags( rv_result ) + lv_carry ).
  ENDMETHOD.
ENDCLASS.
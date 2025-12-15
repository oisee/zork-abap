*&---------------------------------------------------------------------*
*& Report Z_Z80_TEST_HELLO
*& Z80 Emulator - Hello World Test
*&---------------------------------------------------------------------*
REPORT z_z80_test_hello.

START-OF-SELECTION.
  DATA(lo_bus) = NEW zcl_z80_00_bus( ).
  DATA(lo_cpu) = NEW zcl_z80_00_cpu( lo_bus ).

  DATA lv_program TYPE xstring.
  lv_program = '3E48D3003E65D3003E6CD3003E6CD3003E6FD3003E21D30076'.

  cl_demo_output=>write( |Program length: { xstrlen( lv_program ) } bytes| ).
  cl_demo_output=>write( |Program hex: { lv_program }| ).

  lo_bus->zif_z80_00_bus~load( iv_addr = 0 iv_data = lv_program ).

  " Verify memory loaded
  DATA lv_mem TYPE string.
  DATA lv_i TYPE i.
  DO 25 TIMES.
    lv_i = sy-index - 1.
    DATA(lv_byte) = lo_bus->zif_z80_00_bus~read_mem( lv_i ).
    lv_mem = lv_mem && | { lv_byte }|.
  ENDDO.
  cl_demo_output=>write( |Memory at 0x0000: { lv_mem }| ).

  " Step through instructions
  cl_demo_output=>write( |--- Stepping through instructions ---| ).

  DATA lt_trace TYPE string_table.
  DO 20 TIMES.
    DATA(lv_pc_before) = lo_cpu->get_pc( ).
    DATA(lv_opcode) = lo_bus->zif_z80_00_bus~read_mem( lv_pc_before ).
    DATA(lv_cycles) = lo_cpu->step( ).
    DATA(lv_pc_after) = lo_cpu->get_pc( ).
    APPEND |PC:{ lv_pc_before WIDTH = 4 } Op:{ lv_opcode WIDTH = 3 } -> PC:{ lv_pc_after WIDTH = 4 } Cyc:{ lv_cycles }| TO lt_trace.
    IF lo_cpu->is_halted( ) = abap_true.
      APPEND |*** HALTED ***| TO lt_trace.
      EXIT.
    ENDIF.
  ENDDO.
  cl_demo_output=>write( lt_trace ).

  cl_demo_output=>write( |--- Results ---| ).
  cl_demo_output=>write( |Output: "{ lo_bus->zif_z80_00_bus~get_output( ) }"| ).

  DATA(ls_status) = lo_cpu->get_status( ).
  cl_demo_output=>write( |AF: { ls_status-af }| ).
  cl_demo_output=>write( |PC: { ls_status-pc }| ).
  cl_demo_output=>write( |Cycles: { ls_status-cycles }| ).
  cl_demo_output=>write( |Halted: { ls_status-halted }| ).

  cl_demo_output=>display( ).
CLASS zcl_cpu_00_test DEFINITION
  PUBLIC
  CREATE PUBLIC
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.

  PRIVATE SECTION.
    DATA mo_bus TYPE REF TO zcl_cpu_00_bus_simple.
    DATA mo_cpu TYPE REF TO zcl_cpu_00_cpu.

    METHODS setup.
    METHODS load_program
      IMPORTING iv_program TYPE xstring
                iv_addr    TYPE i DEFAULT 32768.

    METHODS test_lda_imm FOR TESTING.
    METHODS test_ldx_imm FOR TESTING.
    METHODS test_ldy_imm FOR TESTING.
    METHODS test_sta_zp FOR TESTING.
    METHODS test_sta_abs FOR TESTING.
    METHODS test_adc_no_carry FOR TESTING.
    METHODS test_adc_with_carry FOR TESTING.
    METHODS test_adc_overflow FOR TESTING.
    METHODS test_sbc FOR TESTING.
    METHODS test_and FOR TESTING.
    METHODS test_ora FOR TESTING.
    METHODS test_eor FOR TESTING.
    METHODS test_cmp_equal FOR TESTING.
    METHODS test_cmp_greater FOR TESTING.
    METHODS test_cmp_less FOR TESTING.
    METHODS test_asl_a FOR TESTING.
    METHODS test_lsr_a FOR TESTING.
    METHODS test_rol_a FOR TESTING.
    METHODS test_ror_a FOR TESTING.
    METHODS test_inc_zp FOR TESTING.
    METHODS test_dec_zp FOR TESTING.
    METHODS test_inx_dex FOR TESTING.
    METHODS test_iny_dey FOR TESTING.
    METHODS test_bne_taken FOR TESTING.
    METHODS test_bne_not_taken FOR TESTING.
    METHODS test_beq FOR TESTING.
    METHODS test_bcs_bcc FOR TESTING.
    METHODS test_jmp_abs FOR TESTING.
    METHODS test_jmp_ind FOR TESTING.
    METHODS test_jsr_rts FOR TESTING.
    METHODS test_pha_pla FOR TESTING.
    METHODS test_php_plp FOR TESTING.
    METHODS test_tax_txa FOR TESTING.
    METHODS test_tay_tya FOR TESTING.
    METHODS test_tsx_txs FOR TESTING.
    METHODS test_clc_sec FOR TESTING.
    METHODS test_flag_z FOR TESTING.
    METHODS test_flag_n FOR TESTING.
    METHODS test_loop_countdown FOR TESTING.
    METHODS test_output_hello FOR TESTING.
    METHODS test_zp_indexed FOR TESTING.
    METHODS test_abs_indexed FOR TESTING.
    METHODS test_indirect_x FOR TESTING.
    METHODS test_indirect_y FOR TESTING.

ENDCLASS.



CLASS zcl_cpu_00_test IMPLEMENTATION.

  METHOD setup.
    mo_bus = NEW zcl_cpu_00_bus_simple( ).
  ENDMETHOD.

  METHOD load_program.
    DATA lv_addr_lo TYPE i.
    DATA lv_addr_hi TYPE i.
    mo_bus->zif_cpu_00_bus~load( iv_addr = iv_addr iv_data = iv_program ).
    lv_addr_lo = iv_addr MOD 256.
    lv_addr_hi = iv_addr / 256.
    mo_bus->zif_cpu_00_bus~write( iv_addr = 65532 iv_val = lv_addr_lo ).
    mo_bus->zif_cpu_00_bus~write( iv_addr = 65533 iv_val = lv_addr_hi ).
    mo_cpu = NEW zcl_cpu_00_cpu( mo_bus ).
  ENDMETHOD.

  METHOD test_lda_imm.
    load_program( iv_program = 'A942' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 66 msg = 'LDA #$42' ).
  ENDMETHOD.

  METHOD test_ldx_imm.
    load_program( iv_program = 'A255' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-x exp = 85 msg = 'LDX #$55' ).
  ENDMETHOD.

  METHOD test_ldy_imm.
    load_program( iv_program = 'A0AA' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-y exp = 170 msg = 'LDY #$AA' ).
  ENDMETHOD.

  METHOD test_sta_zp.
    load_program( iv_program = 'A9558510' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(lv_val) = mo_bus->zif_cpu_00_bus~read( 16 ).
    cl_abap_unit_assert=>assert_equals( act = lv_val exp = 85 msg = 'STA $10' ).
  ENDMETHOD.

  METHOD test_sta_abs.
    load_program( iv_program = 'A9778D0020' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(lv_val) = mo_bus->zif_cpu_00_bus~read( 8192 ).
    cl_abap_unit_assert=>assert_equals( act = lv_val exp = 119 msg = 'STA $2000' ).
  ENDMETHOD.

  METHOD test_adc_no_carry.
    load_program( iv_program = 'A910186905' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 21 msg = '$10 + $05 = $15' ).
  ENDMETHOD.

  METHOD test_adc_with_carry.
    load_program( iv_program = 'A9FF386901' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 1 msg = '$FF + $01 + C = $01' ).
    DATA(lv_carry) = ls_status-p MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_carry exp = 1 msg = 'Carry set' ).
  ENDMETHOD.

  METHOD test_adc_overflow.
    load_program( iv_program = 'A97F186901' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 128 msg = '$7F + $01 = $80' ).
    DATA(lv_overflow) = ( ls_status-p / 64 ) MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_overflow exp = 1 msg = 'Overflow set' ).
  ENDMETHOD.

  METHOD test_sbc.
    load_program( iv_program = 'A91038E905' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 11 msg = '$10 - $05 = $0B' ).
  ENDMETHOD.

  METHOD test_and.
    " Test: LDA #$F0 ($F0=240), AND #$0F ($0F=15) -> result should be 0
    DATA lv_prog TYPE xstring.
    lv_prog = 'A9F0290F'.
    load_program( iv_program = lv_prog iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_after_lda) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_after_lda-a exp = 240 msg = 'LDA #$F0 = 240' ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 0 msg = '$F0 AND $0F = $00' ).
  ENDMETHOD.

  METHOD test_ora.
    load_program( iv_program = 'A9F0090F' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 255 msg = '$F0 OR $0F = $FF' ).
  ENDMETHOD.

  METHOD test_eor.
    load_program( iv_program = 'A9FF49AA' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 85 msg = '$FF XOR $AA = $55' ).
  ENDMETHOD.

  METHOD test_cmp_equal.
    DATA lv_prog TYPE xstring.
    lv_prog = 'A942C942'.
    load_program( iv_program = lv_prog iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_after_lda) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_after_lda-a exp = 66 msg = 'LDA #$42 = 66' ).
    cl_abap_unit_assert=>assert_equals( act = ls_after_lda-pc exp = 32770 msg = 'PC after LDA = $8002' ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-pc exp = 32772 msg = 'PC after CMP = $8004' ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 66 msg = 'A still = 66 after CMP' ).
    DATA lv_p_str TYPE string.
    lv_p_str = |P={ ls_status-p }, Z calc: { ( ls_status-p / 2 ) MOD 2 }|.
    DATA(lv_zero) = ( ls_status-p DIV 2 ) MOD 2.
    DATA(lv_carry) = ls_status-p MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_zero exp = 1 msg = lv_p_str ).
    cl_abap_unit_assert=>assert_equals( act = lv_carry exp = 1 msg = 'Carry flag set' ).
  ENDMETHOD.

  METHOD test_cmp_greater.
    load_program( iv_program = 'A950C940' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    DATA(lv_zero) = ( ls_status-p DIV 2 ) MOD 2.
    DATA(lv_carry) = ls_status-p MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_zero exp = 0 msg = 'Zero flag clear' ).
    cl_abap_unit_assert=>assert_equals( act = lv_carry exp = 1 msg = 'Carry flag set' ).
  ENDMETHOD.

  METHOD test_cmp_less.
    load_program( iv_program = 'A940C950' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    DATA(lv_carry) = ls_status-p MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_carry exp = 0 msg = 'Carry flag clear' ).
  ENDMETHOD.

  METHOD test_asl_a.
    load_program( iv_program = 'A9550A' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 170 msg = '$55 << 1 = $AA' ).
  ENDMETHOD.

  METHOD test_lsr_a.
    load_program( iv_program = 'A9AA4A' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 85 msg = '$AA >> 1 = $55' ).
  ENDMETHOD.

  METHOD test_rol_a.
    load_program( iv_program = 'A980382A' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 1 msg = '$80 ROL with C=1 = $01' ).
    DATA(lv_carry) = ls_status-p MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_carry exp = 1 msg = 'Carry set from bit 7' ).
  ENDMETHOD.

  METHOD test_ror_a.
    load_program( iv_program = 'A901386A' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 128 msg = '$01 ROR with C=1 = $80' ).
    DATA(lv_carry) = ls_status-p MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_carry exp = 1 msg = 'Carry set from bit 0' ).
  ENDMETHOD.

  METHOD test_inc_zp.
    mo_bus->zif_cpu_00_bus~write( iv_addr = 16 iv_val = 255 ).
    load_program( iv_program = 'E610' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(lv_val) = mo_bus->zif_cpu_00_bus~read( 16 ).
    cl_abap_unit_assert=>assert_equals( act = lv_val exp = 0 msg = 'INC $10: $FF + 1 = $00' ).
  ENDMETHOD.

  METHOD test_dec_zp.
    mo_bus->zif_cpu_00_bus~write( iv_addr = 16 iv_val = 0 ).
    load_program( iv_program = 'C610' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(lv_val) = mo_bus->zif_cpu_00_bus~read( 16 ).
    cl_abap_unit_assert=>assert_equals( act = lv_val exp = 255 msg = 'DEC $10: $00 - 1 = $FF' ).
  ENDMETHOD.

  METHOD test_inx_dex.
    load_program( iv_program = 'A2FFE8E8CA' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-x exp = 0 msg = 'INX: $FF + 1 = $00' ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-x exp = 1 msg = 'INX: $00 + 1 = $01' ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-x exp = 0 msg = 'DEX: $01 - 1 = $00' ).
  ENDMETHOD.

  METHOD test_iny_dey.
    load_program( iv_program = 'A000C8C888' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-y exp = 1 msg = 'INY: $00 + 1 = $01' ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-y exp = 2 msg = 'INY: $01 + 1 = $02' ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-y exp = 1 msg = 'DEY: $02 - 1 = $01' ).
  ENDMETHOD.

  METHOD test_bne_taken.
    load_program( iv_program = 'A201D002A9FF' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-pc exp = 32774 msg = 'BNE taken, skip 2 bytes' ).
  ENDMETHOD.

  METHOD test_bne_not_taken.
    load_program( iv_program = 'A200D002A9FF' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-pc exp = 32772 msg = 'BNE not taken, continue' ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 255 msg = 'LDA #$FF executed' ).
  ENDMETHOD.

  METHOD test_beq.
    load_program( iv_program = 'A900F002A9FF' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-pc exp = 32774 msg = 'BEQ taken when Z=1' ).
  ENDMETHOD.

  METHOD test_bcs_bcc.
    load_program( iv_program = '38B00290028A' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-pc exp = 32773 msg = 'BCS taken when C=1' ).
  ENDMETHOD.

  METHOD test_jmp_abs.
    load_program( iv_program = '4C1080A9FF' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-pc exp = 32784 msg = 'JMP $8010' ).
  ENDMETHOD.

  METHOD test_jmp_ind.
    mo_bus->zif_cpu_00_bus~write( iv_addr = 512 iv_val = 16 ).
    mo_bus->zif_cpu_00_bus~write( iv_addr = 513 iv_val = 128 ).
    load_program( iv_program = '6C0002' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-pc exp = 32784 msg = 'JMP ($0200)' ).
  ENDMETHOD.

  METHOD test_jsr_rts.
    " Program layout:
    " $8000: JSR $8004 (20 04 80)
    " $8003: 00 (BRK - not reached during normal flow)
    " $8004: LDA #$42 (A9 42)
    " $8006: RTS (60)
    DATA lv_prog TYPE xstring.
    lv_prog = '20048000A94260'.
    load_program( iv_program = lv_prog iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-pc exp = 32772 msg = 'JSR $8004' ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 66 msg = 'LDA #$42' ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-pc exp = 32771 msg = 'RTS returns to $8003' ).
  ENDMETHOD.

  METHOD test_pha_pla.
    load_program( iv_program = 'A94248A90068' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 0 msg = 'A = $00 after LDA' ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 66 msg = 'A = $42 after PLA' ).
  ENDMETHOD.

  METHOD test_php_plp.
    load_program( iv_program = 'A9FF0828' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_before) = mo_cpu->get_status( ).
    load_program( iv_program = 'A90028' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_after) = mo_cpu->get_status( ).
    DATA(lv_n_before) = ( ls_before-p / 128 ) MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_n_before exp = 1 msg = 'N flag was set' ).
  ENDMETHOD.

  METHOD test_tax_txa.
    load_program( iv_program = 'A942AAA9008A' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-x exp = 66 msg = 'TAX: X = $42' ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 66 msg = 'TXA: A = $42' ).
  ENDMETHOD.

  METHOD test_tay_tya.
    load_program( iv_program = 'A955A8A90098' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-y exp = 85 msg = 'TAY: Y = $55' ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 85 msg = 'TYA: A = $55' ).
  ENDMETHOD.

  METHOD test_tsx_txs.
    load_program( iv_program = 'BAA2809A' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-x exp = 253 msg = 'TSX: X = SP' ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-sp exp = 128 msg = 'TXS: SP = $80' ).
  ENDMETHOD.

  METHOD test_clc_sec.
    load_program( iv_program = '3818' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    DATA(lv_carry) = ls_status-p MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_carry exp = 1 msg = 'SEC sets carry' ).
    mo_cpu->step( ).
    ls_status = mo_cpu->get_status( ).
    lv_carry = ls_status-p MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_carry exp = 0 msg = 'CLC clears carry' ).
  ENDMETHOD.

  METHOD test_flag_z.
    load_program( iv_program = 'A900' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    DATA(lv_zero) = ( ls_status-p DIV 2 ) MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_zero exp = 1 msg = 'Z flag set for $00' ).
  ENDMETHOD.

  METHOD test_flag_n.
    load_program( iv_program = 'A980' iv_addr = 32768 ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    DATA(lv_neg) = ( ls_status-p / 128 ) MOD 2.
    cl_abap_unit_assert=>assert_equals( act = lv_neg exp = 1 msg = 'N flag set for $80' ).
  ENDMETHOD.

  METHOD test_loop_countdown.
    load_program( iv_program = 'A205CAD0FD' iv_addr = 32768 ).
    mo_cpu->run( iv_max_cycles = 100 ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-x exp = 0 msg = 'Loop counts down to 0' ).
  ENDMETHOD.

  METHOD test_output_hello.
    load_program( iv_program = 'A9488D02F0A9698D02F000' iv_addr = 32768 ).
    mo_cpu->run( iv_max_cycles = 100 ).
    DATA(lv_output) = mo_bus->zif_cpu_00_bus~get_output( ).
    cl_abap_unit_assert=>assert_equals( act = lv_output exp = 'Hi' msg = 'Output is Hi' ).
  ENDMETHOD.

  METHOD test_zp_indexed.
    mo_bus->zif_cpu_00_bus~write( iv_addr = 18 iv_val = 99 ).
    load_program( iv_program = 'A202B510' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 99 msg = 'LDA $10,X with X=2' ).
  ENDMETHOD.

  METHOD test_abs_indexed.
    mo_bus->zif_cpu_00_bus~write( iv_addr = 8195 iv_val = 77 ).
    load_program( iv_program = 'A203BD0020' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 77 msg = 'LDA $2000,X with X=3' ).
  ENDMETHOD.

  METHOD test_indirect_x.
    mo_bus->zif_cpu_00_bus~write( iv_addr = 18 iv_val = 0 ).
    mo_bus->zif_cpu_00_bus~write( iv_addr = 19 iv_val = 32 ).
    mo_bus->zif_cpu_00_bus~write( iv_addr = 8192 iv_val = 123 ).
    load_program( iv_program = 'A202A110' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 123 msg = 'LDA ($10,X)' ).
  ENDMETHOD.

  METHOD test_indirect_y.
    mo_bus->zif_cpu_00_bus~write( iv_addr = 16 iv_val = 0 ).
    mo_bus->zif_cpu_00_bus~write( iv_addr = 17 iv_val = 32 ).
    mo_bus->zif_cpu_00_bus~write( iv_addr = 8197 iv_val = 88 ).
    load_program( iv_program = 'A005B110' iv_addr = 32768 ).
    mo_cpu->step( ).
    mo_cpu->step( ).
    DATA(ls_status) = mo_cpu->get_status( ).
    cl_abap_unit_assert=>assert_equals( act = ls_status-a exp = 88 msg = 'LDA ($10),Y' ).
  ENDMETHOD.

ENDCLASS.

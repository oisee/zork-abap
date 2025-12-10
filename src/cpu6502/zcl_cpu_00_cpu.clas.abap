CLASS zcl_cpu_00_cpu DEFINITION
  PUBLIC
  CREATE PUBLIC.
************************************************************************
* 6502 CPU Emulator
* Implements the MOS 6502 processor with all official opcodes
* Uses bus interface for memory-mapped I/O
************************************************************************

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_status,
             a       TYPE i,
             x       TYPE i,
             y       TYPE i,
             sp      TYPE i,
             pc      TYPE i,
             p       TYPE i,
             cycles  TYPE i,
             running TYPE abap_bool,
             waiting TYPE abap_bool,
           END OF ts_status.

    CONSTANTS: c_flag_c TYPE i VALUE 1,
               c_flag_z TYPE i VALUE 2,
               c_flag_i TYPE i VALUE 4,
               c_flag_d TYPE i VALUE 8,
               c_flag_b TYPE i VALUE 16,
               c_flag_u TYPE i VALUE 32,
               c_flag_v TYPE i VALUE 64,
               c_flag_n TYPE i VALUE 128.

    METHODS constructor
      IMPORTING io_bus TYPE REF TO zif_cpu_00_bus.

    METHODS reset.

    METHODS step
      RETURNING VALUE(rv_cycles) TYPE i.

    METHODS run
      IMPORTING iv_max_cycles TYPE i DEFAULT 1000000.

    METHODS nmi.

    METHODS irq.

    METHODS get_status
      RETURNING VALUE(rs_status) TYPE ts_status.

    METHODS is_waiting
      RETURNING VALUE(rv_waiting) TYPE abap_bool.

    METHODS provide_input
      IMPORTING iv_text TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_bus TYPE REF TO zif_cpu_00_bus.

    DATA: mv_a      TYPE i,
          mv_x      TYPE i,
          mv_y      TYPE i,
          mv_sp     TYPE i,
          mv_pc     TYPE i,
          mv_p      TYPE i.

    DATA: mv_cycles  TYPE i,
          mv_running TYPE abap_bool,
          mv_waiting TYPE abap_bool.

    METHODS read
      IMPORTING iv_addr       TYPE i
      RETURNING VALUE(rv_val) TYPE i.

    METHODS write
      IMPORTING iv_addr TYPE i
                iv_val  TYPE i.

    METHODS read16
      IMPORTING iv_addr       TYPE i
      RETURNING VALUE(rv_val) TYPE i.

    METHODS push
      IMPORTING iv_val TYPE i.

    METHODS pull
      RETURNING VALUE(rv_val) TYPE i.

    METHODS push16
      IMPORTING iv_val TYPE i.

    METHODS pull16
      RETURNING VALUE(rv_val) TYPE i.

    METHODS get_flag
      IMPORTING iv_flag       TYPE i
      RETURNING VALUE(rv_set) TYPE abap_bool.

    METHODS set_flag
      IMPORTING iv_flag TYPE i
                iv_set  TYPE abap_bool.

    METHODS update_nz
      IMPORTING iv_val TYPE i.

    METHODS addr_imm RETURNING VALUE(rv_addr) TYPE i.
    METHODS addr_zp RETURNING VALUE(rv_addr) TYPE i.
    METHODS addr_zpx RETURNING VALUE(rv_addr) TYPE i.
    METHODS addr_zpy RETURNING VALUE(rv_addr) TYPE i.
    METHODS addr_abs RETURNING VALUE(rv_addr) TYPE i.
    METHODS addr_abx RETURNING VALUE(rv_addr) TYPE i.
    METHODS addr_aby RETURNING VALUE(rv_addr) TYPE i.
    METHODS addr_izx RETURNING VALUE(rv_addr) TYPE i.
    METHODS addr_izy RETURNING VALUE(rv_addr) TYPE i.
    METHODS addr_ind RETURNING VALUE(rv_addr) TYPE i.

    METHODS op_adc IMPORTING iv_val TYPE i.
    METHODS op_sbc IMPORTING iv_val TYPE i.
    METHODS op_and IMPORTING iv_val TYPE i.
    METHODS op_ora IMPORTING iv_val TYPE i.
    METHODS op_eor IMPORTING iv_val TYPE i.
    METHODS op_cmp IMPORTING iv_val TYPE i.
    METHODS op_cpx IMPORTING iv_val TYPE i.
    METHODS op_cpy IMPORTING iv_val TYPE i.
    METHODS op_bit IMPORTING iv_val TYPE i.
    METHODS op_asl_a.
    METHODS op_asl_m IMPORTING iv_addr TYPE i.
    METHODS op_lsr_a.
    METHODS op_lsr_m IMPORTING iv_addr TYPE i.
    METHODS op_rol_a.
    METHODS op_rol_m IMPORTING iv_addr TYPE i.
    METHODS op_ror_a.
    METHODS op_ror_m IMPORTING iv_addr TYPE i.
    METHODS op_inc IMPORTING iv_addr TYPE i.
    METHODS op_dec IMPORTING iv_addr TYPE i.
    METHODS op_branch IMPORTING iv_cond TYPE abap_bool.

ENDCLASS.



CLASS zcl_cpu_00_cpu IMPLEMENTATION.

  METHOD constructor.
    mo_bus = io_bus.
    reset( ).
  ENDMETHOD.

  METHOD reset.
    mv_a = 0.
    mv_x = 0.
    mv_y = 0.
    mv_sp = 253.
    mv_p = c_flag_u + c_flag_i.
    mv_pc = read16( 65532 ).
    mv_cycles = 0.
    mv_running = abap_true.
    mv_waiting = abap_false.
  ENDMETHOD.

  METHOD read.
    rv_val = mo_bus->read( iv_addr ).
    IF mo_bus->is_input_ready( ) = abap_false AND iv_addr = 61441.
      mv_waiting = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD write.
    mo_bus->write( iv_addr = iv_addr iv_val = iv_val ).
  ENDMETHOD.

  METHOD read16.
    DATA: lv_lo TYPE i,
          lv_hi TYPE i.
    lv_lo = read( iv_addr ).
    lv_hi = read( iv_addr + 1 ).
    rv_val = lv_lo + lv_hi * 256.
  ENDMETHOD.

  METHOD push.
    write( iv_addr = 256 + mv_sp iv_val = iv_val ).
    mv_sp = ( mv_sp - 1 ) MOD 256.
    IF mv_sp < 0.
      mv_sp = mv_sp + 256.
    ENDIF.
  ENDMETHOD.

  METHOD pull.
    mv_sp = ( mv_sp + 1 ) MOD 256.
    rv_val = read( 256 + mv_sp ).
  ENDMETHOD.

  METHOD push16.
    push( iv_val / 256 ).
    push( iv_val MOD 256 ).
  ENDMETHOD.

  METHOD pull16.
    DATA: lv_lo TYPE i,
          lv_hi TYPE i.
    lv_lo = pull( ).
    lv_hi = pull( ).
    rv_val = lv_lo + lv_hi * 256.
  ENDMETHOD.

  METHOD get_flag.
    rv_set = xsdbool( mv_p MOD ( iv_flag * 2 ) >= iv_flag ).
  ENDMETHOD.

  METHOD set_flag.
    IF iv_set = abap_true.
      IF get_flag( iv_flag ) = abap_false.
        mv_p = mv_p + iv_flag.
      ENDIF.
    ELSE.
      IF get_flag( iv_flag ) = abap_true.
        mv_p = mv_p - iv_flag.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD update_nz.
    DATA lv_val TYPE i.
    lv_val = iv_val MOD 256.
    set_flag( iv_flag = c_flag_z iv_set = xsdbool( lv_val = 0 ) ).
    set_flag( iv_flag = c_flag_n iv_set = xsdbool( lv_val >= 128 ) ).
  ENDMETHOD.

  METHOD addr_imm.
    rv_addr = mv_pc.
    mv_pc = mv_pc + 1.
  ENDMETHOD.

  METHOD addr_zp.
    rv_addr = read( mv_pc ).
    mv_pc = mv_pc + 1.
  ENDMETHOD.

  METHOD addr_zpx.
    rv_addr = ( read( mv_pc ) + mv_x ) MOD 256.
    mv_pc = mv_pc + 1.
  ENDMETHOD.

  METHOD addr_zpy.
    rv_addr = ( read( mv_pc ) + mv_y ) MOD 256.
    mv_pc = mv_pc + 1.
  ENDMETHOD.

  METHOD addr_abs.
    rv_addr = read16( mv_pc ).
    mv_pc = mv_pc + 2.
  ENDMETHOD.

  METHOD addr_abx.
    rv_addr = ( read16( mv_pc ) + mv_x ) MOD 65536.
    mv_pc = mv_pc + 2.
  ENDMETHOD.

  METHOD addr_aby.
    rv_addr = ( read16( mv_pc ) + mv_y ) MOD 65536.
    mv_pc = mv_pc + 2.
  ENDMETHOD.

  METHOD addr_izx.
    DATA lv_ptr TYPE i.
    lv_ptr = ( read( mv_pc ) + mv_x ) MOD 256.
    mv_pc = mv_pc + 1.
    rv_addr = read( lv_ptr ) + read( ( lv_ptr + 1 ) MOD 256 ) * 256.
  ENDMETHOD.

  METHOD addr_izy.
    DATA: lv_ptr  TYPE i,
          lv_base TYPE i.
    lv_ptr = read( mv_pc ).
    mv_pc = mv_pc + 1.
    lv_base = read( lv_ptr ) + read( ( lv_ptr + 1 ) MOD 256 ) * 256.
    rv_addr = ( lv_base + mv_y ) MOD 65536.
  ENDMETHOD.

  METHOD addr_ind.
    DATA: lv_ptr TYPE i,
          lv_lo  TYPE i,
          lv_hi  TYPE i.
    lv_ptr = read16( mv_pc ).
    mv_pc = mv_pc + 2.
    lv_lo = read( lv_ptr ).
    lv_hi = read( ( lv_ptr MOD 256 + 1 ) MOD 256 + ( lv_ptr / 256 ) * 256 ).
    rv_addr = lv_lo + lv_hi * 256.
  ENDMETHOD.

  METHOD op_adc.
    DATA: lv_carry  TYPE i,
          lv_result TYPE i,
          lv_a_sign TYPE i,
          lv_v_sign TYPE i,
          lv_r_sign TYPE i.

    IF get_flag( c_flag_c ).
      lv_carry = 1.
    ELSE.
      lv_carry = 0.
    ENDIF.

    lv_result = mv_a + iv_val + lv_carry.

    lv_a_sign = mv_a / 128 MOD 2.
    lv_v_sign = iv_val / 128 MOD 2.
    lv_r_sign = lv_result / 128 MOD 2.
    set_flag( iv_flag = c_flag_v
              iv_set = xsdbool( lv_a_sign = lv_v_sign AND lv_a_sign <> lv_r_sign ) ).

    set_flag( iv_flag = c_flag_c iv_set = xsdbool( lv_result > 255 ) ).
    mv_a = lv_result MOD 256.
    update_nz( mv_a ).
  ENDMETHOD.

  METHOD op_sbc.
    DATA: lv_carry  TYPE i,
          lv_result TYPE i,
          lv_a_sign TYPE i,
          lv_v_sign TYPE i,
          lv_r_sign TYPE i,
          lv_val    TYPE i.

    IF get_flag( c_flag_c ).
      lv_carry = 1.
    ELSE.
      lv_carry = 0.
    ENDIF.

    lv_val = 255 - iv_val.
    lv_result = mv_a + lv_val + lv_carry.

    lv_a_sign = mv_a / 128 MOD 2.
    lv_v_sign = lv_val / 128 MOD 2.
    lv_r_sign = lv_result / 128 MOD 2.
    set_flag( iv_flag = c_flag_v
              iv_set = xsdbool( lv_a_sign = lv_v_sign AND lv_a_sign <> lv_r_sign ) ).

    set_flag( iv_flag = c_flag_c iv_set = xsdbool( lv_result > 255 ) ).
    mv_a = lv_result MOD 256.
    update_nz( mv_a ).
  ENDMETHOD.

  METHOD op_and.
    DATA: lv_result TYPE i,
          lv_mask   TYPE i.
    lv_result = 0.
    lv_mask = 1.
    DO 8 TIMES.
      IF ( mv_a / lv_mask ) MOD 2 = 1 AND ( iv_val / lv_mask ) MOD 2 = 1.
        lv_result = lv_result + lv_mask.
      ENDIF.
      lv_mask = lv_mask * 2.
    ENDDO.
    mv_a = lv_result.
    update_nz( mv_a ).
  ENDMETHOD.

  METHOD op_ora.
    DATA: lv_result TYPE i,
          lv_mask   TYPE i.
    lv_result = 0.
    lv_mask = 1.
    DO 8 TIMES.
      IF ( mv_a / lv_mask ) MOD 2 = 1 OR ( iv_val / lv_mask ) MOD 2 = 1.
        lv_result = lv_result + lv_mask.
      ENDIF.
      lv_mask = lv_mask * 2.
    ENDDO.
    mv_a = lv_result.
    update_nz( mv_a ).
  ENDMETHOD.

  METHOD op_eor.
    DATA: lv_result TYPE i,
          lv_mask   TYPE i.
    lv_result = 0.
    lv_mask = 1.
    DO 8 TIMES.
      IF ( ( mv_a / lv_mask ) MOD 2 + ( iv_val / lv_mask ) MOD 2 ) = 1.
        lv_result = lv_result + lv_mask.
      ENDIF.
      lv_mask = lv_mask * 2.
    ENDDO.
    mv_a = lv_result.
    update_nz( mv_a ).
  ENDMETHOD.

  METHOD op_cmp.
    DATA lv_result TYPE i.
    lv_result = mv_a - iv_val.
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( mv_a >= iv_val ) ).
    IF lv_result < 0.
      lv_result = lv_result + 256.
    ENDIF.
    update_nz( lv_result ).
  ENDMETHOD.

  METHOD op_cpx.
    DATA lv_result TYPE i.
    lv_result = mv_x - iv_val.
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( mv_x >= iv_val ) ).
    IF lv_result < 0.
      lv_result = lv_result + 256.
    ENDIF.
    update_nz( lv_result ).
  ENDMETHOD.

  METHOD op_cpy.
    DATA lv_result TYPE i.
    lv_result = mv_y - iv_val.
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( mv_y >= iv_val ) ).
    IF lv_result < 0.
      lv_result = lv_result + 256.
    ENDIF.
    update_nz( lv_result ).
  ENDMETHOD.

  METHOD op_bit.
    DATA lv_result TYPE i.
    DATA lv_mask TYPE i.
    lv_result = 0.
    lv_mask = 1.
    DO 8 TIMES.
      IF ( mv_a / lv_mask ) MOD 2 = 1 AND ( iv_val / lv_mask ) MOD 2 = 1.
        lv_result = lv_result + lv_mask.
      ENDIF.
      lv_mask = lv_mask * 2.
    ENDDO.
    set_flag( iv_flag = c_flag_z iv_set = xsdbool( lv_result = 0 ) ).
    set_flag( iv_flag = c_flag_n iv_set = xsdbool( iv_val >= 128 ) ).
    set_flag( iv_flag = c_flag_v iv_set = xsdbool( ( iv_val / 64 ) MOD 2 = 1 ) ).
  ENDMETHOD.

  METHOD op_asl_a.
    DATA lv_result TYPE i.
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( mv_a >= 128 ) ).
    lv_result = ( mv_a * 2 ) MOD 256.
    mv_a = lv_result.
    update_nz( mv_a ).
  ENDMETHOD.

  METHOD op_asl_m.
    DATA: lv_val    TYPE i,
          lv_result TYPE i.
    lv_val = read( iv_addr ).
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( lv_val >= 128 ) ).
    lv_result = ( lv_val * 2 ) MOD 256.
    write( iv_addr = iv_addr iv_val = lv_result ).
    update_nz( lv_result ).
  ENDMETHOD.

  METHOD op_lsr_a.
    DATA lv_result TYPE i.
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( mv_a MOD 2 = 1 ) ).
    lv_result = mv_a / 2.
    mv_a = lv_result.
    update_nz( mv_a ).
  ENDMETHOD.

  METHOD op_lsr_m.
    DATA: lv_val    TYPE i,
          lv_result TYPE i.
    lv_val = read( iv_addr ).
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( lv_val MOD 2 = 1 ) ).
    lv_result = lv_val / 2.
    write( iv_addr = iv_addr iv_val = lv_result ).
    update_nz( lv_result ).
  ENDMETHOD.

  METHOD op_rol_a.
    DATA: lv_result TYPE i,
          lv_carry  TYPE i.
    IF get_flag( c_flag_c ).
      lv_carry = 1.
    ELSE.
      lv_carry = 0.
    ENDIF.
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( mv_a >= 128 ) ).
    lv_result = ( mv_a * 2 + lv_carry ) MOD 256.
    mv_a = lv_result.
    update_nz( mv_a ).
  ENDMETHOD.

  METHOD op_rol_m.
    DATA: lv_val    TYPE i,
          lv_result TYPE i,
          lv_carry  TYPE i.
    lv_val = read( iv_addr ).
    IF get_flag( c_flag_c ).
      lv_carry = 1.
    ELSE.
      lv_carry = 0.
    ENDIF.
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( lv_val >= 128 ) ).
    lv_result = ( lv_val * 2 + lv_carry ) MOD 256.
    write( iv_addr = iv_addr iv_val = lv_result ).
    update_nz( lv_result ).
  ENDMETHOD.

  METHOD op_ror_a.
    DATA: lv_result TYPE i,
          lv_carry  TYPE i.
    IF get_flag( c_flag_c ).
      lv_carry = 128.
    ELSE.
      lv_carry = 0.
    ENDIF.
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( mv_a MOD 2 = 1 ) ).
    lv_result = mv_a / 2 + lv_carry.
    mv_a = lv_result.
    update_nz( mv_a ).
  ENDMETHOD.

  METHOD op_ror_m.
    DATA: lv_val    TYPE i,
          lv_result TYPE i,
          lv_carry  TYPE i.
    lv_val = read( iv_addr ).
    IF get_flag( c_flag_c ).
      lv_carry = 128.
    ELSE.
      lv_carry = 0.
    ENDIF.
    set_flag( iv_flag = c_flag_c iv_set = xsdbool( lv_val MOD 2 = 1 ) ).
    lv_result = lv_val / 2 + lv_carry.
    write( iv_addr = iv_addr iv_val = lv_result ).
    update_nz( lv_result ).
  ENDMETHOD.

  METHOD op_inc.
    DATA lv_val TYPE i.
    lv_val = ( read( iv_addr ) + 1 ) MOD 256.
    write( iv_addr = iv_addr iv_val = lv_val ).
    update_nz( lv_val ).
  ENDMETHOD.

  METHOD op_dec.
    DATA lv_val TYPE i.
    lv_val = read( iv_addr ) - 1.
    IF lv_val < 0.
      lv_val = lv_val + 256.
    ENDIF.
    write( iv_addr = iv_addr iv_val = lv_val ).
    update_nz( lv_val ).
  ENDMETHOD.

  METHOD op_branch.
    DATA: lv_offset TYPE i,
          lv_target TYPE i.
    lv_offset = read( mv_pc ).
    mv_pc = mv_pc + 1.
    IF iv_cond = abap_true.
      IF lv_offset >= 128.
        lv_offset = lv_offset - 256.
      ENDIF.
      lv_target = mv_pc + lv_offset.
      IF lv_target < 0.
        lv_target = lv_target + 65536.
      ENDIF.
      mv_pc = lv_target MOD 65536.
    ENDIF.
  ENDMETHOD.

  METHOD step.
    DATA: lv_opcode TYPE i,
          lv_addr   TYPE i.

    IF mv_running = abap_false.
      rv_cycles = 0.
      RETURN.
    ENDIF.

    IF mv_waiting = abap_true.
      IF mo_bus->is_input_ready( ) = abap_false.
        rv_cycles = 0.
        RETURN.
      ENDIF.
      mv_waiting = abap_false.
    ENDIF.

    lv_opcode = read( mv_pc ).
    mv_pc = ( mv_pc + 1 ) MOD 65536.

    CASE lv_opcode.
      WHEN 169. mv_a = read( addr_imm( ) ). update_nz( mv_a ). rv_cycles = 2.
      WHEN 165. mv_a = read( addr_zp( ) ). update_nz( mv_a ). rv_cycles = 3.
      WHEN 181. mv_a = read( addr_zpx( ) ). update_nz( mv_a ). rv_cycles = 4.
      WHEN 173. mv_a = read( addr_abs( ) ). update_nz( mv_a ). rv_cycles = 4.
      WHEN 189. mv_a = read( addr_abx( ) ). update_nz( mv_a ). rv_cycles = 4.
      WHEN 185. mv_a = read( addr_aby( ) ). update_nz( mv_a ). rv_cycles = 4.
      WHEN 161. mv_a = read( addr_izx( ) ). update_nz( mv_a ). rv_cycles = 6.
      WHEN 177. mv_a = read( addr_izy( ) ). update_nz( mv_a ). rv_cycles = 5.

      WHEN 162. mv_x = read( addr_imm( ) ). update_nz( mv_x ). rv_cycles = 2.
      WHEN 166. mv_x = read( addr_zp( ) ). update_nz( mv_x ). rv_cycles = 3.
      WHEN 182. mv_x = read( addr_zpy( ) ). update_nz( mv_x ). rv_cycles = 4.
      WHEN 174. mv_x = read( addr_abs( ) ). update_nz( mv_x ). rv_cycles = 4.
      WHEN 190. mv_x = read( addr_aby( ) ). update_nz( mv_x ). rv_cycles = 4.

      WHEN 160. mv_y = read( addr_imm( ) ). update_nz( mv_y ). rv_cycles = 2.
      WHEN 164. mv_y = read( addr_zp( ) ). update_nz( mv_y ). rv_cycles = 3.
      WHEN 180. mv_y = read( addr_zpx( ) ). update_nz( mv_y ). rv_cycles = 4.
      WHEN 172. mv_y = read( addr_abs( ) ). update_nz( mv_y ). rv_cycles = 4.
      WHEN 188. mv_y = read( addr_abx( ) ). update_nz( mv_y ). rv_cycles = 4.

      WHEN 133. write( iv_addr = addr_zp( ) iv_val = mv_a ). rv_cycles = 3.
      WHEN 149. write( iv_addr = addr_zpx( ) iv_val = mv_a ). rv_cycles = 4.
      WHEN 141. write( iv_addr = addr_abs( ) iv_val = mv_a ). rv_cycles = 4.
      WHEN 157. write( iv_addr = addr_abx( ) iv_val = mv_a ). rv_cycles = 5.
      WHEN 153. write( iv_addr = addr_aby( ) iv_val = mv_a ). rv_cycles = 5.
      WHEN 129. write( iv_addr = addr_izx( ) iv_val = mv_a ). rv_cycles = 6.
      WHEN 145. write( iv_addr = addr_izy( ) iv_val = mv_a ). rv_cycles = 6.

      WHEN 134. write( iv_addr = addr_zp( ) iv_val = mv_x ). rv_cycles = 3.
      WHEN 150. write( iv_addr = addr_zpy( ) iv_val = mv_x ). rv_cycles = 4.
      WHEN 142. write( iv_addr = addr_abs( ) iv_val = mv_x ). rv_cycles = 4.

      WHEN 132. write( iv_addr = addr_zp( ) iv_val = mv_y ). rv_cycles = 3.
      WHEN 148. write( iv_addr = addr_zpx( ) iv_val = mv_y ). rv_cycles = 4.
      WHEN 140. write( iv_addr = addr_abs( ) iv_val = mv_y ). rv_cycles = 4.

      WHEN 105. op_adc( read( addr_imm( ) ) ). rv_cycles = 2.
      WHEN 101. op_adc( read( addr_zp( ) ) ). rv_cycles = 3.
      WHEN 117. op_adc( read( addr_zpx( ) ) ). rv_cycles = 4.
      WHEN 109. op_adc( read( addr_abs( ) ) ). rv_cycles = 4.
      WHEN 125. op_adc( read( addr_abx( ) ) ). rv_cycles = 4.
      WHEN 121. op_adc( read( addr_aby( ) ) ). rv_cycles = 4.
      WHEN 97.  op_adc( read( addr_izx( ) ) ). rv_cycles = 6.
      WHEN 113. op_adc( read( addr_izy( ) ) ). rv_cycles = 5.

      WHEN 233. op_sbc( read( addr_imm( ) ) ). rv_cycles = 2.
      WHEN 229. op_sbc( read( addr_zp( ) ) ). rv_cycles = 3.
      WHEN 245. op_sbc( read( addr_zpx( ) ) ). rv_cycles = 4.
      WHEN 237. op_sbc( read( addr_abs( ) ) ). rv_cycles = 4.
      WHEN 253. op_sbc( read( addr_abx( ) ) ). rv_cycles = 4.
      WHEN 249. op_sbc( read( addr_aby( ) ) ). rv_cycles = 4.
      WHEN 225. op_sbc( read( addr_izx( ) ) ). rv_cycles = 6.
      WHEN 241. op_sbc( read( addr_izy( ) ) ). rv_cycles = 5.

      WHEN 41.  op_and( read( addr_imm( ) ) ). rv_cycles = 2.
      WHEN 37.  op_and( read( addr_zp( ) ) ). rv_cycles = 3.
      WHEN 53.  op_and( read( addr_zpx( ) ) ). rv_cycles = 4.
      WHEN 45.  op_and( read( addr_abs( ) ) ). rv_cycles = 4.
      WHEN 61.  op_and( read( addr_abx( ) ) ). rv_cycles = 4.
      WHEN 57.  op_and( read( addr_aby( ) ) ). rv_cycles = 4.
      WHEN 33.  op_and( read( addr_izx( ) ) ). rv_cycles = 6.
      WHEN 49.  op_and( read( addr_izy( ) ) ). rv_cycles = 5.

      WHEN 9.   op_ora( read( addr_imm( ) ) ). rv_cycles = 2.
      WHEN 5.   op_ora( read( addr_zp( ) ) ). rv_cycles = 3.
      WHEN 21.  op_ora( read( addr_zpx( ) ) ). rv_cycles = 4.
      WHEN 13.  op_ora( read( addr_abs( ) ) ). rv_cycles = 4.
      WHEN 29.  op_ora( read( addr_abx( ) ) ). rv_cycles = 4.
      WHEN 25.  op_ora( read( addr_aby( ) ) ). rv_cycles = 4.
      WHEN 1.   op_ora( read( addr_izx( ) ) ). rv_cycles = 6.
      WHEN 17.  op_ora( read( addr_izy( ) ) ). rv_cycles = 5.

      WHEN 73.  op_eor( read( addr_imm( ) ) ). rv_cycles = 2.
      WHEN 69.  op_eor( read( addr_zp( ) ) ). rv_cycles = 3.
      WHEN 85.  op_eor( read( addr_zpx( ) ) ). rv_cycles = 4.
      WHEN 77.  op_eor( read( addr_abs( ) ) ). rv_cycles = 4.
      WHEN 93.  op_eor( read( addr_abx( ) ) ). rv_cycles = 4.
      WHEN 89.  op_eor( read( addr_aby( ) ) ). rv_cycles = 4.
      WHEN 65.  op_eor( read( addr_izx( ) ) ). rv_cycles = 6.
      WHEN 81.  op_eor( read( addr_izy( ) ) ). rv_cycles = 5.

      WHEN 201. op_cmp( read( addr_imm( ) ) ). rv_cycles = 2.
      WHEN 197. op_cmp( read( addr_zp( ) ) ). rv_cycles = 3.
      WHEN 213. op_cmp( read( addr_zpx( ) ) ). rv_cycles = 4.
      WHEN 205. op_cmp( read( addr_abs( ) ) ). rv_cycles = 4.
      WHEN 221. op_cmp( read( addr_abx( ) ) ). rv_cycles = 4.
      WHEN 217. op_cmp( read( addr_aby( ) ) ). rv_cycles = 4.
      WHEN 193. op_cmp( read( addr_izx( ) ) ). rv_cycles = 6.
      WHEN 209. op_cmp( read( addr_izy( ) ) ). rv_cycles = 5.

      WHEN 224. op_cpx( read( addr_imm( ) ) ). rv_cycles = 2.
      WHEN 228. op_cpx( read( addr_zp( ) ) ). rv_cycles = 3.
      WHEN 236. op_cpx( read( addr_abs( ) ) ). rv_cycles = 4.

      WHEN 192. op_cpy( read( addr_imm( ) ) ). rv_cycles = 2.
      WHEN 196. op_cpy( read( addr_zp( ) ) ). rv_cycles = 3.
      WHEN 204. op_cpy( read( addr_abs( ) ) ). rv_cycles = 4.

      WHEN 36.  op_bit( read( addr_zp( ) ) ). rv_cycles = 3.
      WHEN 44.  op_bit( read( addr_abs( ) ) ). rv_cycles = 4.

      WHEN 10.  op_asl_a( ). rv_cycles = 2.
      WHEN 6.   op_asl_m( addr_zp( ) ). rv_cycles = 5.
      WHEN 22.  op_asl_m( addr_zpx( ) ). rv_cycles = 6.
      WHEN 14.  op_asl_m( addr_abs( ) ). rv_cycles = 6.
      WHEN 30.  op_asl_m( addr_abx( ) ). rv_cycles = 7.

      WHEN 74.  op_lsr_a( ). rv_cycles = 2.
      WHEN 70.  op_lsr_m( addr_zp( ) ). rv_cycles = 5.
      WHEN 86.  op_lsr_m( addr_zpx( ) ). rv_cycles = 6.
      WHEN 78.  op_lsr_m( addr_abs( ) ). rv_cycles = 6.
      WHEN 94.  op_lsr_m( addr_abx( ) ). rv_cycles = 7.

      WHEN 42.  op_rol_a( ). rv_cycles = 2.
      WHEN 38.  op_rol_m( addr_zp( ) ). rv_cycles = 5.
      WHEN 54.  op_rol_m( addr_zpx( ) ). rv_cycles = 6.
      WHEN 46.  op_rol_m( addr_abs( ) ). rv_cycles = 6.
      WHEN 62.  op_rol_m( addr_abx( ) ). rv_cycles = 7.

      WHEN 106. op_ror_a( ). rv_cycles = 2.
      WHEN 102. op_ror_m( addr_zp( ) ). rv_cycles = 5.
      WHEN 118. op_ror_m( addr_zpx( ) ). rv_cycles = 6.
      WHEN 110. op_ror_m( addr_abs( ) ). rv_cycles = 6.
      WHEN 126. op_ror_m( addr_abx( ) ). rv_cycles = 7.

      WHEN 230. op_inc( addr_zp( ) ). rv_cycles = 5.
      WHEN 246. op_inc( addr_zpx( ) ). rv_cycles = 6.
      WHEN 238. op_inc( addr_abs( ) ). rv_cycles = 6.
      WHEN 254. op_inc( addr_abx( ) ). rv_cycles = 7.

      WHEN 198. op_dec( addr_zp( ) ). rv_cycles = 5.
      WHEN 214. op_dec( addr_zpx( ) ). rv_cycles = 6.
      WHEN 206. op_dec( addr_abs( ) ). rv_cycles = 6.
      WHEN 222. op_dec( addr_abx( ) ). rv_cycles = 7.

      WHEN 232. mv_x = ( mv_x + 1 ) MOD 256. update_nz( mv_x ). rv_cycles = 2.
      WHEN 200. mv_y = ( mv_y + 1 ) MOD 256. update_nz( mv_y ). rv_cycles = 2.
      WHEN 202. mv_x = mv_x - 1. IF mv_x < 0. mv_x = 255. ENDIF. update_nz( mv_x ). rv_cycles = 2.
      WHEN 136. mv_y = mv_y - 1. IF mv_y < 0. mv_y = 255. ENDIF. update_nz( mv_y ). rv_cycles = 2.

      WHEN 16.  op_branch( xsdbool( NOT get_flag( c_flag_n ) ) ). rv_cycles = 2.
      WHEN 48.  op_branch( get_flag( c_flag_n ) ). rv_cycles = 2.
      WHEN 80.  op_branch( xsdbool( NOT get_flag( c_flag_v ) ) ). rv_cycles = 2.
      WHEN 112. op_branch( get_flag( c_flag_v ) ). rv_cycles = 2.
      WHEN 144. op_branch( xsdbool( NOT get_flag( c_flag_c ) ) ). rv_cycles = 2.
      WHEN 176. op_branch( get_flag( c_flag_c ) ). rv_cycles = 2.
      WHEN 208. op_branch( xsdbool( NOT get_flag( c_flag_z ) ) ). rv_cycles = 2.
      WHEN 240. op_branch( get_flag( c_flag_z ) ). rv_cycles = 2.

      WHEN 76.  mv_pc = addr_abs( ). rv_cycles = 3.
      WHEN 108. mv_pc = addr_ind( ). rv_cycles = 5.

      WHEN 32.
        lv_addr = addr_abs( ).
        push16( mv_pc - 1 ).
        mv_pc = lv_addr.
        rv_cycles = 6.

      WHEN 96.
        mv_pc = pull16( ) + 1.
        rv_cycles = 6.

      WHEN 64.
        mv_p = pull( ).
        set_flag( iv_flag = c_flag_u iv_set = abap_true ).
        mv_pc = pull16( ).
        rv_cycles = 6.

      WHEN 72.  push( mv_a ). rv_cycles = 3.
      WHEN 8.   push( mv_p ). rv_cycles = 3.
      WHEN 104. mv_a = pull( ). update_nz( mv_a ). rv_cycles = 4.
      WHEN 40.  mv_p = pull( ). set_flag( iv_flag = c_flag_u iv_set = abap_true ). rv_cycles = 4.

      WHEN 170. mv_x = mv_a. update_nz( mv_x ). rv_cycles = 2.
      WHEN 168. mv_y = mv_a. update_nz( mv_y ). rv_cycles = 2.
      WHEN 138. mv_a = mv_x. update_nz( mv_a ). rv_cycles = 2.
      WHEN 152. mv_a = mv_y. update_nz( mv_a ). rv_cycles = 2.
      WHEN 154. mv_sp = mv_x. rv_cycles = 2.
      WHEN 186. mv_x = mv_sp. update_nz( mv_x ). rv_cycles = 2.

      WHEN 24.  set_flag( iv_flag = c_flag_c iv_set = abap_false ). rv_cycles = 2.
      WHEN 56.  set_flag( iv_flag = c_flag_c iv_set = abap_true ). rv_cycles = 2.
      WHEN 88.  set_flag( iv_flag = c_flag_i iv_set = abap_false ). rv_cycles = 2.
      WHEN 120. set_flag( iv_flag = c_flag_i iv_set = abap_true ). rv_cycles = 2.
      WHEN 184. set_flag( iv_flag = c_flag_v iv_set = abap_false ). rv_cycles = 2.
      WHEN 216. set_flag( iv_flag = c_flag_d iv_set = abap_false ). rv_cycles = 2.
      WHEN 248. set_flag( iv_flag = c_flag_d iv_set = abap_true ). rv_cycles = 2.

      WHEN 0.
        mv_pc = mv_pc + 1.
        push16( mv_pc ).
        push( mv_p + c_flag_b ).
        set_flag( iv_flag = c_flag_i iv_set = abap_true ).
        mv_pc = read16( 65534 ).
        rv_cycles = 7.

      WHEN 234. rv_cycles = 2.

      WHEN OTHERS.
        rv_cycles = 2.
    ENDCASE.

    mv_cycles = mv_cycles + rv_cycles.
  ENDMETHOD.

  METHOD run.
    DATA lv_cycles TYPE i.
    lv_cycles = 0.
    WHILE mv_running = abap_true AND mv_waiting = abap_false AND lv_cycles < iv_max_cycles.
      lv_cycles = lv_cycles + step( ).
    ENDWHILE.
  ENDMETHOD.

  METHOD nmi.
    push16( mv_pc ).
    push( mv_p ).
    set_flag( iv_flag = c_flag_i iv_set = abap_true ).
    mv_pc = read16( 65530 ).
  ENDMETHOD.

  METHOD irq.
    IF get_flag( c_flag_i ) = abap_false.
      push16( mv_pc ).
      push( mv_p ).
      set_flag( iv_flag = c_flag_i iv_set = abap_true ).
      mv_pc = read16( 65534 ).
    ENDIF.
  ENDMETHOD.

  METHOD get_status.
    rs_status-a = mv_a.
    rs_status-x = mv_x.
    rs_status-y = mv_y.
    rs_status-sp = mv_sp.
    rs_status-pc = mv_pc.
    rs_status-p = mv_p.
    rs_status-cycles = mv_cycles.
    rs_status-running = mv_running.
    rs_status-waiting = mv_waiting.
  ENDMETHOD.

  METHOD is_waiting.
    rv_waiting = mv_waiting.
  ENDMETHOD.

  METHOD provide_input.
    mo_bus->provide_input( iv_text ).
    mv_waiting = abap_false.
  ENDMETHOD.

ENDCLASS.

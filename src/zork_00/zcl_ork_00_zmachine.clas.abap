CLASS zcl_ork_00_zmachine DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_status,
             running TYPE abap_bool,
             waiting TYPE abap_bool,
             output  TYPE string,
           END OF ty_status.

    METHODS constructor IMPORTING iv_story TYPE xstring.
    METHODS step.
    METHODS run.
    METHODS get_status RETURNING VALUE(rs_status) TYPE ty_status.
    METHODS provide_input IMPORTING iv_input TYPE string.

  PRIVATE SECTION.
    DATA mo_memory    TYPE REF TO zcl_ork_00_memory.
    DATA mo_stack     TYPE REF TO zcl_ork_00_stack.
    DATA mo_objects   TYPE REF TO zcl_ork_00_objects.
    DATA mo_text      TYPE REF TO zcl_ork_00_text.
    DATA mo_dict      TYPE REF TO zcl_ork_00_dict.
    DATA mv_pc        TYPE i.
    DATA mv_running   TYPE abap_bool.
    DATA mv_waiting   TYPE abap_bool.
    DATA mv_output    TYPE string.
    DATA mv_newline   TYPE string.
    DATA mv_read_text_buf  TYPE i.
    DATA mv_read_parse_buf TYPE i.

    METHODS get_var IMPORTING iv_var TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS set_var IMPORTING iv_var TYPE i iv_val TYPE i.
    METHODS peek_var IMPORTING iv_var TYPE i RETURNING VALUE(rv_val) TYPE i.
    METHODS read_byte RETURNING VALUE(rv_val) TYPE i.
    METHODS read_word RETURNING VALUE(rv_val) TYPE i.
    TYPES ty_int4_table TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    METHODS decode_operands
      IMPORTING iv_opcode TYPE i iv_form TYPE string
      RETURNING VALUE(rt_ops) TYPE ty_int4_table.
    METHODS read_branch EXPORTING ev_on TYPE abap_bool ev_offset TYPE i.
    METHODS do_branch IMPORTING iv_cond TYPE abap_bool iv_on TYPE abap_bool iv_offset TYPE i.
    METHODS do_return IMPORTING iv_val TYPE i.
    METHODS print_text IMPORTING iv_text TYPE string.
    METHODS to_signed IMPORTING iv_val TYPE i RETURNING VALUE(rv_signed) TYPE i.
    METHODS exec_0op IMPORTING iv_op TYPE i.
    METHODS exec_1op IMPORTING iv_op TYPE i it_ops TYPE ty_int4_table.
    METHODS exec_2op IMPORTING iv_op TYPE i it_ops TYPE ty_int4_table.
    METHODS exec_var IMPORTING iv_op TYPE i it_ops TYPE ty_int4_table.
ENDCLASS.

CLASS zcl_ork_00_zmachine IMPLEMENTATION.
  METHOD constructor.
    mv_newline = cl_abap_char_utilities=>newline.
    mo_memory = NEW zcl_ork_00_memory( iv_story ).
    mo_stack = NEW zcl_ork_00_stack( ).
    mo_objects = NEW zcl_ork_00_objects( mo_memory ).
    mo_text = NEW zcl_ork_00_text( mo_memory ).
    mo_dict = NEW zcl_ork_00_dict( io_mem = mo_memory io_text = mo_text ).
    mv_pc = mo_memory->initial_pc.
    mv_running = abap_true.
    mv_waiting = abap_false.
  ENDMETHOD.

  METHOD to_signed.
    IF iv_val >= 32768.
      rv_signed = iv_val - 65536.
    ELSE.
      rv_signed = iv_val.
    ENDIF.
  ENDMETHOD.

  METHOD get_var.
    IF iv_var = 0.
      rv_val = mo_stack->pop( ).
    ELSEIF iv_var < 16.
      rv_val = mo_stack->get_local( iv_var ).
    ELSE.
      rv_val = mo_memory->get_global( iv_var ).
    ENDIF.
  ENDMETHOD.

  METHOD set_var.
    DATA(lv_val) = iv_val MOD 65536.
    IF lv_val < 0.
      lv_val = lv_val + 65536.
    ENDIF.
    IF iv_var = 0.
      mo_stack->push( lv_val ).
    ELSEIF iv_var < 16.
      mo_stack->set_local( iv_var = iv_var iv_val = lv_val ).
    ELSE.
      mo_memory->set_global( iv_var = iv_var iv_val = lv_val ).
    ENDIF.
  ENDMETHOD.

  METHOD peek_var.
    IF iv_var = 0.
      rv_val = mo_stack->peek( ).
    ELSEIF iv_var < 16.
      rv_val = mo_stack->get_local( iv_var ).
    ELSE.
      rv_val = mo_memory->get_global( iv_var ).
    ENDIF.
  ENDMETHOD.

  METHOD read_byte.
    rv_val = mo_memory->u8( mv_pc ).
    mv_pc = mv_pc + 1.
  ENDMETHOD.

  METHOD read_word.
    rv_val = mo_memory->u16( mv_pc ).
    mv_pc = mv_pc + 2.
  ENDMETHOD.

  METHOD decode_operands.
    DATA lv_type TYPE i.
    DATA lv_val TYPE i.
    DATA lv_type_byte TYPE i.

    IF iv_form = 'short'.
      lv_type = ( iv_opcode DIV 16 ) MOD 4.
      CASE lv_type.
        WHEN 0. lv_val = read_word( ). APPEND lv_val TO rt_ops.
        WHEN 1. lv_val = read_byte( ). APPEND lv_val TO rt_ops.
        WHEN 2. lv_val = get_var( read_byte( ) ). APPEND lv_val TO rt_ops.
      ENDCASE.
    ELSEIF iv_form = 'long'.
      IF iv_opcode MOD 128 >= 64.
        lv_val = get_var( read_byte( ) ).
      ELSE.
        lv_val = read_byte( ).
      ENDIF.
      APPEND lv_val TO rt_ops.
      IF iv_opcode MOD 64 >= 32.
        lv_val = get_var( read_byte( ) ).
      ELSE.
        lv_val = read_byte( ).
      ENDIF.
      APPEND lv_val TO rt_ops.
    ELSEIF iv_form = 'variable'.
      lv_type_byte = read_byte( ).
      DO 4 TIMES.
        DATA(lv_shift) = ( 4 - sy-index ) * 2.
        lv_type = ( lv_type_byte DIV ( 2 ** lv_shift ) ) MOD 4.
        CASE lv_type.
          WHEN 0. lv_val = read_word( ). APPEND lv_val TO rt_ops.
          WHEN 1. lv_val = read_byte( ). APPEND lv_val TO rt_ops.
          WHEN 2. lv_val = get_var( read_byte( ) ). APPEND lv_val TO rt_ops.
          WHEN 3. EXIT.
        ENDCASE.
      ENDDO.
    ENDIF.
  ENDMETHOD.

  METHOD read_branch.
    DATA(lv_byte1) = read_byte( ).
    ev_on = xsdbool( lv_byte1 >= 128 ).
    IF lv_byte1 MOD 128 >= 64.
      ev_offset = lv_byte1 MOD 64.
    ELSE.
      DATA(lv_byte2) = read_byte( ).
      DATA(lv_off) = ( ( lv_byte1 MOD 64 ) * 256 ) + lv_byte2.
      IF lv_off >= 8192.
        lv_off = lv_off - 16384.
      ENDIF.
      ev_offset = lv_off.
    ENDIF.
  ENDMETHOD.

  METHOD do_branch.
    IF iv_cond = iv_on.
      IF iv_offset = 0.
        do_return( 0 ).
      ELSEIF iv_offset = 1.
        do_return( 1 ).
      ELSE.
        mv_pc = mv_pc + iv_offset - 2.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD do_return.
    DATA lv_return_pc TYPE i.
    DATA lv_return_var TYPE i.
    mo_stack->ret( IMPORTING ev_return_pc = lv_return_pc ev_return_var = lv_return_var ).
    mv_pc = lv_return_pc.
    set_var( iv_var = lv_return_var iv_val = iv_val ).
  ENDMETHOD.

  METHOD print_text.
    mv_output = mv_output && iv_text.
  ENDMETHOD.

  METHOD step.
    DATA lv_opcode TYPE i.
    DATA lv_op_type TYPE i.
    DATA lv_op_num TYPE i.
    DATA lt_operands TYPE int4_table.
    DATA lv_form TYPE string.

    IF mv_waiting = abap_true OR mv_running = abap_false.
      RETURN.
    ENDIF.

    lv_opcode = read_byte( ).

    IF lv_opcode = 190.
      RETURN.
    ELSEIF lv_opcode < 128.
      lv_form = 'long'.
      lv_op_num = lv_opcode MOD 32.
      lt_operands = decode_operands( iv_opcode = lv_opcode iv_form = lv_form ).
      exec_2op( iv_op = lv_op_num it_ops = lt_operands ).
    ELSEIF lv_opcode < 192.
      lv_form = 'short'.
      lv_op_type = ( lv_opcode DIV 16 ) MOD 4.
      lv_op_num = lv_opcode MOD 16.
      IF lv_op_type = 3.
        exec_0op( lv_op_num ).
      ELSE.
        lt_operands = decode_operands( iv_opcode = lv_opcode iv_form = lv_form ).
        exec_1op( iv_op = lv_op_num it_ops = lt_operands ).
      ENDIF.
    ELSE.
      lv_form = 'variable'.
      IF lv_opcode MOD 64 >= 32.
        lv_op_num = lv_opcode MOD 32.
        lt_operands = decode_operands( iv_opcode = lv_opcode iv_form = lv_form ).
        exec_var( iv_op = lv_op_num it_ops = lt_operands ).
      ELSE.
        lv_op_num = lv_opcode MOD 32.
        lt_operands = decode_operands( iv_opcode = lv_opcode iv_form = lv_form ).
        exec_2op( iv_op = lv_op_num it_ops = lt_operands ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    DATA lv_steps TYPE i VALUE 0.
    WHILE mv_running = abap_true AND mv_waiting = abap_false.
      step( ).
      lv_steps = lv_steps + 1.
      IF lv_steps > 100000.
        DATA(lv_msg) = |[LIMIT: { lv_steps } PC: { mv_pc }]|.
        print_text( lv_msg ).
        mv_running = abap_false.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_status.
    rs_status-running = mv_running.
    rs_status-waiting = mv_waiting.
    rs_status-output = mv_output.
    CLEAR mv_output.
  ENDMETHOD.

  METHOD provide_input.
    DATA lv_line TYPE string.
    DATA lv_char TYPE c LENGTH 1.

    IF mv_waiting = abap_false.
      RETURN.
    ENDIF.

    lv_line = to_lower( iv_input ).
    DATA(lv_max) = mo_memory->u8( mv_read_text_buf ).
    DATA(lv_len) = strlen( lv_line ).
    IF lv_len > lv_max.
      lv_len = lv_max.
    ENDIF.

    DO lv_len TIMES.
      DATA(lv_i) = sy-index - 1.
      lv_char = lv_line+lv_i(1).
      mo_memory->w8( iv_addr = mv_read_text_buf + 1 + lv_i
                     iv_val = cl_abap_conv_out_ce=>uccpi( lv_char ) ).
    ENDDO.
    mo_memory->w8( iv_addr = mv_read_text_buf + 1 + lv_len iv_val = 0 ).

    mo_dict->tokenize( iv_text = lv_line iv_text_buf = mv_read_text_buf iv_parse_buf = mv_read_parse_buf ).
    mv_waiting = abap_false.
  ENDMETHOD.

  METHOD exec_0op.
    DATA lv_text TYPE string.
    DATA lv_len TYPE i.
    DATA lv_on TYPE abap_bool.
    DATA lv_offset TYPE i.

    CASE iv_op.
      WHEN 0. do_return( 1 ).
      WHEN 1. do_return( 0 ).
      WHEN 2.
        mo_text->decode( EXPORTING iv_addr = mv_pc IMPORTING ev_text = lv_text ev_len = lv_len ).
        mv_pc = mv_pc + lv_len.
        print_text( lv_text ).
      WHEN 3.
        mo_text->decode( EXPORTING iv_addr = mv_pc IMPORTING ev_text = lv_text ev_len = lv_len ).
        mv_pc = mv_pc + lv_len.
        print_text( lv_text && mv_newline ).
        do_return( 1 ).
      WHEN 4. " nop
      WHEN 5.
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = abap_false iv_on = lv_on iv_offset = lv_offset ).
      WHEN 6.
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = abap_false iv_on = lv_on iv_offset = lv_offset ).
      WHEN 8. do_return( mo_stack->pop( ) ).
      WHEN 9. mo_stack->pop( ).
      WHEN 10. mv_running = abap_false.
      WHEN 11. print_text( mv_newline ).
      WHEN 13.
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = abap_true iv_on = lv_on iv_offset = lv_offset ).
    ENDCASE.
  ENDMETHOD.

  METHOD exec_1op.
    DATA lv_a TYPE i.
    DATA lv_store TYPE i.
    DATA lv_val TYPE i.
    DATA lv_on TYPE abap_bool.
    DATA lv_offset TYPE i.
    DATA lv_text TYPE string.

    READ TABLE it_ops INDEX 1 INTO lv_a.

    CASE iv_op.
      WHEN 0.
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = xsdbool( lv_a = 0 ) iv_on = lv_on iv_offset = lv_offset ).
      WHEN 1.
        lv_store = read_byte( ).
        lv_val = mo_objects->get_sibling( lv_a ).
        set_var( iv_var = lv_store iv_val = lv_val ).
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = xsdbool( lv_val <> 0 ) iv_on = lv_on iv_offset = lv_offset ).
      WHEN 2.
        lv_store = read_byte( ).
        lv_val = mo_objects->get_child( lv_a ).
        set_var( iv_var = lv_store iv_val = lv_val ).
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = xsdbool( lv_val <> 0 ) iv_on = lv_on iv_offset = lv_offset ).
      WHEN 3.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = mo_objects->get_parent( lv_a ) ).
      WHEN 4.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = mo_objects->get_prop_len( lv_a ) ).
      WHEN 5.
        lv_val = to_signed( peek_var( lv_a ) ) + 1.
        set_var( iv_var = lv_a iv_val = lv_val ).
      WHEN 6.
        lv_val = to_signed( peek_var( lv_a ) ) - 1.
        set_var( iv_var = lv_a iv_val = lv_val ).
      WHEN 7.
        mo_text->decode( EXPORTING iv_addr = lv_a IMPORTING ev_text = lv_text ).
        print_text( lv_text ).
      WHEN 9. mo_objects->remove_obj( lv_a ).
      WHEN 10.
        DATA(lv_name) = mo_objects->get_name_addr( lv_a ).
        IF lv_name > 0.
          mo_text->decode( EXPORTING iv_addr = lv_name IMPORTING ev_text = lv_text ).
          print_text( lv_text ).
        ENDIF.
      WHEN 11. do_return( lv_a ).
      WHEN 12. mv_pc = mv_pc + to_signed( lv_a ) - 2.
      WHEN 13.
        mo_text->decode( EXPORTING iv_addr = lv_a * 2 IMPORTING ev_text = lv_text ).
        print_text( lv_text ).
      WHEN 14.
        lv_store = read_byte( ).
        IF lv_a = 0.
          set_var( iv_var = lv_store iv_val = mo_stack->peek( ) ).
        ELSE.
          set_var( iv_var = lv_store iv_val = peek_var( lv_a ) ).
        ENDIF.
      WHEN 15.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = 65535 - lv_a ).
    ENDCASE.
  ENDMETHOD.

  METHOD exec_2op.
    DATA lv_a TYPE i.
    DATA lv_b TYPE i.
    DATA lv_c TYPE i.
    DATA lv_d TYPE i.
    DATA lv_store TYPE i.
    DATA lv_val TYPE i.
    DATA lv_on TYPE abap_bool.
    DATA lv_offset TYPE i.
    DATA lv_result TYPE abap_bool.

    READ TABLE it_ops INDEX 1 INTO lv_a.
    READ TABLE it_ops INDEX 2 INTO lv_b.
    READ TABLE it_ops INDEX 3 INTO lv_c.
    READ TABLE it_ops INDEX 4 INTO lv_d.

    CASE iv_op.
      WHEN 1.
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        lv_result = xsdbool( lv_a = lv_b ).
        IF lines( it_ops ) > 2 AND lv_result = abap_false.
          lv_result = xsdbool( lv_a = lv_c ).
        ENDIF.
        IF lines( it_ops ) > 3 AND lv_result = abap_false.
          lv_result = xsdbool( lv_a = lv_d ).
        ENDIF.
        do_branch( iv_cond = lv_result iv_on = lv_on iv_offset = lv_offset ).
      WHEN 2.
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = xsdbool( to_signed( lv_a ) < to_signed( lv_b ) ) iv_on = lv_on iv_offset = lv_offset ).
      WHEN 3.
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = xsdbool( to_signed( lv_a ) > to_signed( lv_b ) ) iv_on = lv_on iv_offset = lv_offset ).
      WHEN 4.
        lv_val = to_signed( peek_var( lv_a ) ) - 1.
        set_var( iv_var = lv_a iv_val = lv_val ).
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = xsdbool( lv_val < to_signed( lv_b ) ) iv_on = lv_on iv_offset = lv_offset ).
      WHEN 5.
        lv_val = to_signed( peek_var( lv_a ) ) + 1.
        set_var( iv_var = lv_a iv_val = lv_val ).
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = xsdbool( lv_val > to_signed( lv_b ) ) iv_on = lv_on iv_offset = lv_offset ).
      WHEN 6.
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = xsdbool( mo_objects->get_parent( lv_a ) = lv_b ) iv_on = lv_on iv_offset = lv_offset ).
      WHEN 7.
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        DATA(lv_test_and) = 0.
        DATA(lv_ta) = lv_a. DATA(lv_tb) = lv_b. DATA(lv_tbit) = 1.
        DO 16 TIMES.
          IF lv_ta MOD 2 = 1 AND lv_tb MOD 2 = 1. lv_test_and = lv_test_and + lv_tbit. ENDIF.
          lv_ta = lv_ta DIV 2. lv_tb = lv_tb DIV 2. lv_tbit = lv_tbit * 2.
        ENDDO.
        do_branch( iv_cond = xsdbool( lv_test_and = lv_b ) iv_on = lv_on iv_offset = lv_offset ).
      WHEN 8.
        lv_store = read_byte( ).
        DATA(lv_or) = 0. DATA(lv_oa) = lv_a. DATA(lv_ob) = lv_b. DATA(lv_obit) = 1.
        DO 16 TIMES.
          IF lv_oa MOD 2 = 1 OR lv_ob MOD 2 = 1. lv_or = lv_or + lv_obit. ENDIF.
          lv_oa = lv_oa DIV 2. lv_ob = lv_ob DIV 2. lv_obit = lv_obit * 2.
        ENDDO.
        set_var( iv_var = lv_store iv_val = lv_or ).
      WHEN 9.
        lv_store = read_byte( ).
        DATA(lv_and) = 0. DATA(lv_aa) = lv_a. DATA(lv_ab) = lv_b. DATA(lv_abit) = 1.
        DO 16 TIMES.
          IF lv_aa MOD 2 = 1 AND lv_ab MOD 2 = 1. lv_and = lv_and + lv_abit. ENDIF.
          lv_aa = lv_aa DIV 2. lv_ab = lv_ab DIV 2. lv_abit = lv_abit * 2.
        ENDDO.
        set_var( iv_var = lv_store iv_val = lv_and ).
      WHEN 10.
        read_branch( IMPORTING ev_on = lv_on ev_offset = lv_offset ).
        do_branch( iv_cond = mo_objects->get_attr( iv_obj = lv_a iv_attr = lv_b ) iv_on = lv_on iv_offset = lv_offset ).
      WHEN 11. mo_objects->set_attr( iv_obj = lv_a iv_attr = lv_b ).
      WHEN 12. mo_objects->clear_attr( iv_obj = lv_a iv_attr = lv_b ).
      WHEN 13. set_var( iv_var = lv_a iv_val = lv_b ).
      WHEN 14. mo_objects->insert_obj( iv_obj = lv_a iv_dest = lv_b ).
      WHEN 15.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = mo_memory->u16( ( lv_a + lv_b * 2 ) MOD 65536 ) ).
      WHEN 16.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = mo_memory->u8( ( lv_a + lv_b ) MOD 65536 ) ).
      WHEN 17.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = mo_objects->get_prop( iv_obj = lv_a iv_prop = lv_b ) ).
      WHEN 18.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = mo_objects->get_prop_data_addr( iv_obj = lv_a iv_prop = lv_b ) ).
      WHEN 19.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = mo_objects->get_next_prop( iv_obj = lv_a iv_prop = lv_b ) ).
      WHEN 20.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = to_signed( lv_a ) + to_signed( lv_b ) ).
      WHEN 21.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = to_signed( lv_a ) - to_signed( lv_b ) ).
      WHEN 22.
        lv_store = read_byte( ).
        set_var( iv_var = lv_store iv_val = to_signed( lv_a ) * to_signed( lv_b ) ).
      WHEN 23.
        lv_store = read_byte( ).
        IF to_signed( lv_b ) = 0.
          lv_val = 0.
        ELSE.
          lv_val = to_signed( lv_a ) DIV to_signed( lv_b ).
        ENDIF.
        set_var( iv_var = lv_store iv_val = lv_val ).
      WHEN 24.
        lv_store = read_byte( ).
        DATA(lv_sa) = to_signed( lv_a ). DATA(lv_sb) = to_signed( lv_b ).
        IF lv_sb = 0. lv_val = 0. ELSE. lv_val = lv_sa - ( lv_sa DIV lv_sb ) * lv_sb. ENDIF.
      set_var( iv_var = lv_store iv_val = lv_val ).
    ENDCASE.
  ENDMETHOD.

  METHOD exec_var.
    DATA lv_routine TYPE i.
    DATA lv_store TYPE i.
    DATA lv_num_locals TYPE i.
    DATA lv_val TYPE i.

    CASE iv_op.
      WHEN 0.
        READ TABLE it_ops INDEX 1 INTO lv_routine.
        IF lv_routine = 0.
          lv_store = read_byte( ).
          set_var( iv_var = lv_store iv_val = 0 ).
          RETURN.
        ENDIF.
        lv_routine = lv_routine * 2.
        lv_store = read_byte( ).
        lv_num_locals = mo_memory->u8( lv_routine ).
        lv_routine = lv_routine + 1.
        mo_stack->call( iv_return_pc = mv_pc iv_return_var = lv_store iv_num_locals = lv_num_locals ).
        DO lv_num_locals TIMES.
          DATA(lv_i) = sy-index.
          lv_val = mo_memory->u16( lv_routine ).
          mo_stack->set_frame_local( iv_idx = lv_i iv_val = lv_val ).
          lv_routine = lv_routine + 2.
        ENDDO.
        LOOP AT it_ops INTO lv_val FROM 2.
          DATA(lv_j) = sy-tabix - 1.
          IF lv_j <= lv_num_locals.
            mo_stack->set_frame_local( iv_idx = lv_j iv_val = lv_val ).
          ENDIF.
        ENDLOOP.
        mv_pc = lv_routine.
      WHEN 1. mo_memory->w16( iv_addr = ( it_ops[ 1 ] + it_ops[ 2 ] * 2 ) MOD 65536 iv_val = it_ops[ 3 ] ).
      WHEN 2. mo_memory->w8( iv_addr = ( it_ops[ 1 ] + it_ops[ 2 ] ) MOD 65536 iv_val = it_ops[ 3 ] ).
      WHEN 3. mo_objects->put_prop( iv_obj = it_ops[ 1 ] iv_prop = it_ops[ 2 ] iv_val = it_ops[ 3 ] ).
      WHEN 4.
        " sread - wait for input (game prints its own prompt)
        mv_read_text_buf = it_ops[ 1 ].
        mv_read_parse_buf = it_ops[ 2 ].
        mv_waiting = abap_true.
      WHEN 5.
        DATA lv_char TYPE c LENGTH 1.
        lv_char = cl_abap_conv_in_ce=>uccpi( it_ops[ 1 ] ).
        print_text( CONV string( lv_char ) ).
      WHEN 6.
        DATA(lv_num) = to_signed( it_ops[ 1 ] ).
        DATA(lv_numstr) = CONV string( lv_num ).
        print_text( lv_numstr ).
      WHEN 7.
        lv_store = read_byte( ).
        DATA(lv_range) = to_signed( it_ops[ 1 ] ).
        IF lv_range <= 0.
          set_var( iv_var = lv_store iv_val = 0 ).
        ELSE.
          DATA lv_random TYPE i.
          CALL FUNCTION 'GENERAL_GET_RANDOM_INT' EXPORTING range = lv_range IMPORTING random = lv_random.
          set_var( iv_var = lv_store iv_val = lv_random ).
        ENDIF.
      WHEN 8. mo_stack->push( it_ops[ 1 ] ).
      WHEN 9. set_var( iv_var = it_ops[ 1 ] iv_val = mo_stack->pop( ) ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

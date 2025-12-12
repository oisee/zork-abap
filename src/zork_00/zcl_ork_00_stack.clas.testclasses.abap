*"* use this source file for your ABAP unit test classes
CLASS ltcl_stack DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_stack TYPE REF TO zcl_ork_00_stack.

    METHODS setup.
    METHODS test_constructor FOR TESTING.
    METHODS test_push_pop FOR TESTING.
    METHODS test_peek FOR TESTING.
    METHODS test_pop_empty FOR TESTING.
    METHODS test_overflow FOR TESTING.
    METHODS test_local_variables FOR TESTING.
    METHODS test_call_return FOR TESTING.
    METHODS test_nested_calls FOR TESTING.
    METHODS test_set_frame_local FOR TESTING.
    METHODS test_get_depth FOR TESTING.
    METHODS test_stack_isolation FOR TESTING.
ENDCLASS.


CLASS ltcl_stack IMPLEMENTATION.

  METHOD setup.
    mo_stack = NEW zcl_ork_00_stack( ).
  ENDMETHOD.

  METHOD test_constructor.
    cl_abap_unit_assert=>assert_not_initial(
      act = mo_stack
      msg = 'Stack object should be created' ).

    " Initial frame should exist
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_depth( )
      exp = 1
      msg = 'Should have initial frame' ).
  ENDMETHOD.

  METHOD test_push_pop.
    mo_stack->push( 100 ).
    mo_stack->push( 200 ).
    mo_stack->push( 300 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->pop( )
      exp = 300
      msg = 'First pop should return 300' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->pop( )
      exp = 200
      msg = 'Second pop should return 200' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->pop( )
      exp = 100
      msg = 'Third pop should return 100' ).
  ENDMETHOD.

  METHOD test_peek.
    mo_stack->push( 42 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->peek( )
      exp = 42
      msg = 'Peek should return 42' ).

    " Peek should not remove the value
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->peek( )
      exp = 42
      msg = 'Peek should still return 42' ).

    " Pop should still return same value
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->pop( )
      exp = 42
      msg = 'Pop should return 42' ).
  ENDMETHOD.

  METHOD test_pop_empty.
    " Popping empty stack should return 0
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->pop( )
      exp = 0
      msg = 'Pop from empty stack should return 0' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->peek( )
      exp = 0
      msg = 'Peek on empty stack should return 0' ).
  ENDMETHOD.

  METHOD test_overflow.
    " Values should wrap at 65536
    mo_stack->push( 70000 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->pop( )
      exp = 4464
      msg = '70000 mod 65536 = 4464' ).
  ENDMETHOD.

  METHOD test_local_variables.
    " Create a call frame with locals
    mo_stack->call(
      iv_return_pc  = 1000
      iv_return_var = 5
      iv_num_locals = 3 ).

    " Set and get locals (1-based)
    mo_stack->set_local( iv_var = 1 iv_val = 111 ).
    mo_stack->set_local( iv_var = 2 iv_val = 222 ).
    mo_stack->set_local( iv_var = 3 iv_val = 333 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 1 )
      exp = 111
      msg = 'Local 1 should be 111' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 2 )
      exp = 222
      msg = 'Local 2 should be 222' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 3 )
      exp = 333
      msg = 'Local 3 should be 333' ).

    " Non-existent local should return 0
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 4 )
      exp = 0
      msg = 'Local 4 should be 0 (does not exist)' ).
  ENDMETHOD.

  METHOD test_call_return.
    " Make a call
    mo_stack->call(
      iv_return_pc  = 500
      iv_return_var = 3
      iv_num_locals = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_depth( )
      exp = 2
      msg = 'Should have 2 frames after call' ).

    " Return from call
    DATA lv_return_pc TYPE i.
    DATA lv_return_var TYPE i.
    mo_stack->ret(
      IMPORTING
        ev_return_pc = lv_return_pc
        ev_return_var = lv_return_var ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_return_pc
      exp = 500
      msg = 'Return PC should be 500' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_return_var
      exp = 3
      msg = 'Return var should be 3' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_depth( )
      exp = 1
      msg = 'Should have 1 frame after return' ).
  ENDMETHOD.

  METHOD test_nested_calls.
    " First call
    mo_stack->call(
      iv_return_pc  = 100
      iv_return_var = 1
      iv_num_locals = 1 ).
    mo_stack->set_local( iv_var = 1 iv_val = 1111 ).
    mo_stack->push( 11 ).

    " Second call
    mo_stack->call(
      iv_return_pc  = 200
      iv_return_var = 2
      iv_num_locals = 1 ).
    mo_stack->set_local( iv_var = 1 iv_val = 2222 ).
    mo_stack->push( 22 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_depth( )
      exp = 3
      msg = 'Should have 3 frames' ).

    " Inner frame should have its own locals
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 1 )
      exp = 2222
      msg = 'Inner local should be 2222' ).

    " Return from second call
    DATA lv_return_pc TYPE i.
    DATA lv_return_var TYPE i.
    mo_stack->ret( IMPORTING ev_return_pc = lv_return_pc ev_return_var = lv_return_var ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_return_pc
      exp = 200
      msg = 'Second return PC should be 200' ).

    " Now in first frame - its locals should be visible
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 1 )
      exp = 1111
      msg = 'Outer local should be 1111' ).

    " First frame's stack should be intact
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->pop( )
      exp = 11
      msg = 'First frame stack value should be 11' ).
  ENDMETHOD.

  METHOD test_set_frame_local.
    " Create a call frame with locals
    mo_stack->call(
      iv_return_pc  = 1000
      iv_return_var = 5
      iv_num_locals = 3 ).

    " Use set_frame_local (for call initialization)
    mo_stack->set_frame_local( iv_idx = 1 iv_val = 999 ).
    mo_stack->set_frame_local( iv_idx = 2 iv_val = 888 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 1 )
      exp = 999
      msg = 'Frame local 1 should be 999' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_local( 2 )
      exp = 888
      msg = 'Frame local 2 should be 888' ).
  ENDMETHOD.

  METHOD test_get_depth.
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_depth( )
      exp = 1
      msg = 'Initial depth should be 1' ).

    mo_stack->call( iv_return_pc = 1 iv_return_var = 0 iv_num_locals = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_depth( )
      exp = 2
      msg = 'Depth after call should be 2' ).

    mo_stack->call( iv_return_pc = 2 iv_return_var = 0 iv_num_locals = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->get_depth( )
      exp = 3
      msg = 'Depth after second call should be 3' ).
  ENDMETHOD.

  METHOD test_stack_isolation.
    " Push values in initial frame
    mo_stack->push( 1 ).
    mo_stack->push( 2 ).

    " Call creates new frame with separate stack
    mo_stack->call( iv_return_pc = 100 iv_return_var = 0 iv_num_locals = 0 ).

    " New frame's stack should be empty
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->peek( )
      exp = 0
      msg = 'New frame stack should be empty' ).

    " Push to new frame
    mo_stack->push( 99 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->pop( )
      exp = 99
      msg = 'Should pop 99 from new frame' ).

    " Return to original frame
    DATA lv_pc TYPE i.
    DATA lv_var TYPE i.
    mo_stack->ret( IMPORTING ev_return_pc = lv_pc ev_return_var = lv_var ).

    " Original frame's stack should be intact
    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->pop( )
      exp = 2
      msg = 'Original frame should have 2' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stack->pop( )
      exp = 1
      msg = 'Original frame should have 1' ).
  ENDMETHOD.

ENDCLASS.

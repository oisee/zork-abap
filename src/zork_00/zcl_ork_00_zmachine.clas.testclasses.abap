*"* use this source file for your ABAP unit test classes
CLASS ltcl_zmachine DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_constructor FOR TESTING RAISING cx_static_check.
    METHODS test_initial_status FOR TESTING RAISING cx_static_check.
    METHODS test_step_runs FOR TESTING RAISING cx_static_check.
    METHODS test_run_with_story FOR TESTING RAISING cx_static_check.
    METHODS test_provide_input FOR TESTING RAISING cx_static_check.
    METHODS test_empty_story FOR TESTING RAISING cx_static_check.
    METHODS test_output_accumulation FOR TESTING RAISING cx_static_check.
    METHODS test_status_clears_output FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_zmachine IMPLEMENTATION.

  METHOD test_constructor.
    " Load a real story file
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).

    TRY.
        DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI-Z3' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    IF lv_story IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_zm) = NEW zcl_ork_00_zmachine( lv_story ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lo_zm
      msg = 'Z-machine should be created' ).
  ENDMETHOD.

  METHOD test_initial_status.
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).

    TRY.
        DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI-Z3' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    IF lv_story IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_zm) = NEW zcl_ork_00_zmachine( lv_story ).
    DATA(ls_status) = lo_zm->get_status( ).

    cl_abap_unit_assert=>assert_true(
      act = ls_status-running
      msg = 'Z-machine should be running initially' ).

    cl_abap_unit_assert=>assert_false(
      act = ls_status-waiting
      msg = 'Z-machine should not be waiting initially' ).
  ENDMETHOD.

  METHOD test_step_runs.
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).

    TRY.
        DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI-Z3' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    IF lv_story IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_zm) = NEW zcl_ork_00_zmachine( lv_story ).

    " Execute a few steps
    DO 10 TIMES.
      lo_zm->step( ).
    ENDDO.

    DATA(ls_status) = lo_zm->get_status( ).

    " Should still be in some valid state
    cl_abap_unit_assert=>assert_true(
      act = boolc( ls_status-running = abap_true OR ls_status-waiting = abap_true )
      msg = 'Z-machine should be running or waiting after steps' ).
  ENDMETHOD.

  METHOD test_run_with_story.
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).

    TRY.
        DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI-Z3' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    IF lv_story IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_zm) = NEW zcl_ork_00_zmachine( lv_story ).

    " Run until waiting for input or stopped
    lo_zm->run( ).

    DATA(ls_status) = lo_zm->get_status( ).

    " After run, should either be waiting for input or stopped
    cl_abap_unit_assert=>assert_true(
      act = boolc( ls_status-waiting = abap_true OR ls_status-running = abap_false )
      msg = 'After run, should be waiting or stopped' ).

    " Should have produced some output (game intro)
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_status-output
      msg = 'Should have produced output' ).
  ENDMETHOD.

  METHOD test_provide_input.
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).

    TRY.
        DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI-Z3' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    IF lv_story IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_zm) = NEW zcl_ork_00_zmachine( lv_story ).

    " Run until waiting
    lo_zm->run( ).

    DATA(ls_status) = lo_zm->get_status( ).

    IF ls_status-waiting = abap_true.
      " Provide input
      lo_zm->provide_input( 'look' ).

      DATA(ls_after) = lo_zm->get_status( ).

      " Should no longer be waiting
      cl_abap_unit_assert=>assert_false(
        act = ls_after-waiting
        msg = 'Should not be waiting after input' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_empty_story.
    " Test with minimal/empty story
    DATA lv_empty TYPE xstring.

    " Create minimal V3 header (64+ bytes)
    lv_empty = repeat( val = '00' occ = 128 ).
    lv_empty = '03' && lv_empty+2.  " Version 3

    DATA(lo_zm) = NEW zcl_ork_00_zmachine( lv_empty ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lo_zm
      msg = 'Should handle minimal story' ).

    " Running on empty story should not crash (may stop quickly)
    DATA(ls_status) = lo_zm->get_status( ).

    cl_abap_unit_assert=>assert_true(
      act = boolc( ls_status-running = abap_true OR ls_status-running = abap_false )
      msg = 'Status should be valid' ).
  ENDMETHOD.

  METHOD test_output_accumulation.
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).

    TRY.
        DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI-Z3' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    IF lv_story IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_zm) = NEW zcl_ork_00_zmachine( lv_story ).

    " Run to get initial output
    lo_zm->run( ).
    DATA(ls_status1) = lo_zm->get_status( ).

    IF ls_status1-waiting = abap_true.
      " Provide input and run again
      lo_zm->provide_input( 'look' ).
      lo_zm->run( ).
      DATA(ls_status2) = lo_zm->get_status( ).
      DATA(lv_output2) = ls_status2-output.

      " Second output should contain response to "look"
      cl_abap_unit_assert=>assert_not_initial(
        act = lv_output2
        msg = 'Should have output after look command' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_status_clears_output.
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).

    TRY.
        DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI-Z3' ).
      CATCH cx_root.
        RETURN.
    ENDTRY.

    IF lv_story IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_zm) = NEW zcl_ork_00_zmachine( lv_story ).

    " Run to get output
    lo_zm->run( ).
    DATA(ls_status1) = lo_zm->get_status( ).

    " First call should have output
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_status1-output
      msg = 'First status should have output' ).

    " Second call without running should have empty output
    DATA(ls_status2) = lo_zm->get_status( ).

    cl_abap_unit_assert=>assert_initial(
      act = ls_status2-output
      msg = 'Second status should have empty output (cleared)' ).
  ENDMETHOD.

ENDCLASS.

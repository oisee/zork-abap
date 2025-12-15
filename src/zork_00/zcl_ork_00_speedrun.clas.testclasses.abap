*"* use this source file for your ABAP unit test classes
CLASS ltcl_speedrun DEFINITION FINAL FOR TESTING
  DURATION MEDIUM
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_speedrun_zork_mini FOR TESTING.
    METHODS test_speedrun_with_assertions FOR TESTING.
    METHODS test_empty_story FOR TESTING.
    METHODS test_empty_script FOR TESTING.
ENDCLASS.


CLASS ltcl_speedrun IMPLEMENTATION.

  METHOD test_speedrun_zork_mini.
    " Load ZORK-MINI.Z3 and run speedrun script
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).
    DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI.Z3' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lv_story
      msg = 'Should load ZORK-MINI.Z3' ).

    " Load speedrun script
    DATA(lo_script_loader) = NEW zcl_ork_00_script_loader_smw0( ).
    DATA(lt_commands) = lo_script_loader->zif_ork_00_script_loader~load( 'ZORK-MINI-SPEEDRUN.TXT' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lt_commands
      msg = 'Should load speedrun script' ).

    " Run speedrun
    DATA(lo_speedrun) = NEW zcl_ork_00_speedrun(
      iv_story    = lv_story
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    " Check result
    cl_abap_unit_assert=>assert_true(
      act = ls_result-success
      msg = |Speedrun should succeed: { ls_result-error_message }| ).

    cl_abap_unit_assert=>assert_true(
      act = boolc( ls_result-commands_run > 0 )
      msg = 'Should run at least some commands' ).

    " Check log was generated
    DATA(lt_log) = lo_speedrun->get_log( ).
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_log
      msg = 'Should generate log' ).
  ENDMETHOD.


  METHOD test_speedrun_with_assertions.
    " Load ZORK-MINI.Z3 and run test script with assertions
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).
    DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI.Z3' ).

    " Load test script (with assertions)
    DATA(lo_script_loader) = NEW zcl_ork_00_script_loader_smw0( ).
    DATA(lt_commands) = lo_script_loader->zif_ork_00_script_loader~load( 'ZORK-MINI-TEST.TXT' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = lt_commands
      msg = 'Should load test script' ).

    " Run speedrun
    DATA(lo_speedrun) = NEW zcl_ork_00_speedrun(
      iv_story    = lv_story
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    " Check result
    cl_abap_unit_assert=>assert_true(
      act = ls_result-success
      msg = |Test run should succeed: { ls_result-error_message }| ).

    " Should have assertions
    cl_abap_unit_assert=>assert_true(
      act = boolc( ls_result-assertions_total > 0 )
      msg = 'Test script should have assertions' ).

    " All assertions should pass
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-assertions_fail
      exp = 0
      msg = |All assertions should pass. Failed: { ls_result-assertions_fail }/{ ls_result-assertions_total }| ).
  ENDMETHOD.


  METHOD test_empty_story.
    " Test with empty story
    DATA: lv_empty_story TYPE xstring,
          lt_commands    TYPE zif_ork_00_script_loader=>tt_commands.

    APPEND 'look' TO lt_commands.

    DATA(lo_speedrun) = NEW zcl_ork_00_speedrun(
      iv_story    = lv_empty_story
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    " Should fail gracefully
    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Should fail with empty story' ).

    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-error_message
      msg = 'Should have error message' ).
  ENDMETHOD.


  METHOD test_empty_script.
    " Test with empty script
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).
    DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI.Z3' ).

    DATA: lt_commands TYPE zif_ork_00_script_loader=>tt_commands.

    DATA(lo_speedrun) = NEW zcl_ork_00_speedrun(
      iv_story    = lv_story
      it_commands = lt_commands ).

    DATA(ls_result) = lo_speedrun->run( ).

    " Should fail gracefully
    cl_abap_unit_assert=>assert_false(
      act = ls_result-success
      msg = 'Should fail with empty script' ).
  ENDMETHOD.

ENDCLASS.

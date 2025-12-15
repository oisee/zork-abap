*"* use this source file for your ABAP unit test classes
CLASS ltcl_script_loader_smw0 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_ork_00_script_loader_smw0.

    METHODS setup.
    METHODS test_list_scripts FOR TESTING.
    METHODS test_load_speedrun_script FOR TESTING.
    METHODS test_load_test_script FOR TESTING.
    METHODS test_load_nonexistent FOR TESTING.
ENDCLASS.


CLASS ltcl_script_loader_smw0 IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.


  METHOD test_list_scripts.
    " Test that list_scripts returns scripts from SMW0
    DATA(lt_scripts) = mo_cut->zif_ork_00_script_loader~list_scripts( ).

    " Should find at least some TXT files
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_scripts
      msg = 'Should find scripts in SMW0' ).

    " Check for ZORK-MINI-SPEEDRUN.TXT specifically
    DATA(lv_found) = abap_false.
    LOOP AT lt_scripts INTO DATA(ls_script).
      IF ls_script-id CS 'SPEEDRUN'.
        lv_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true(
      act = lv_found
      msg = 'Should find ZORK-MINI-SPEEDRUN.TXT in SMW0' ).
  ENDMETHOD.


  METHOD test_load_speedrun_script.
    " Test loading speedrun script
    DATA(lt_commands) = mo_cut->zif_ork_00_script_loader~load( 'ZORK-MINI-SPEEDRUN.TXT' ).

    " Should return commands
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_commands
      msg = 'Should load speedrun script' ).

    " Should have multiple commands
    cl_abap_unit_assert=>assert_true(
      act = boolc( lines( lt_commands ) > 5 )
      msg = 'Speedrun script should have multiple commands' ).
  ENDMETHOD.


  METHOD test_load_test_script.
    " Test loading test script with assertions
    DATA(lt_commands) = mo_cut->zif_ork_00_script_loader~load( 'ZORK-MINI-TEST.TXT' ).

    " Should return commands
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_commands
      msg = 'Should load test script' ).

    " Should contain assertion lines (starting with %)
    DATA(lv_has_assertions) = abap_false.
    LOOP AT lt_commands INTO DATA(lv_cmd).
      IF strlen( lv_cmd ) > 0 AND lv_cmd(1) = '%'.
        lv_has_assertions = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true(
      act = lv_has_assertions
      msg = 'Test script should contain assertions (% lines)' ).
  ENDMETHOD.


  METHOD test_load_nonexistent.
    " Test loading non-existent script
    DATA(lt_commands) = mo_cut->zif_ork_00_script_loader~load( 'NONEXISTENT.TXT' ).

    " Should return empty
    cl_abap_unit_assert=>assert_initial(
      act = lt_commands
      msg = 'Should return empty for non-existent script' ).
  ENDMETHOD.

ENDCLASS.

*"* use this source file for your ABAP unit test classes
CLASS ltcl_game_loader_smw0 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_ork_00_game_loader_smw0.

    METHODS setup.
    METHODS test_list_games FOR TESTING.
    METHODS test_load_zork_mini FOR TESTING.
    METHODS test_load_nonexistent FOR TESTING.
    METHODS test_pattern_filter FOR TESTING.
ENDCLASS.


CLASS ltcl_game_loader_smw0 IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW #( ).
  ENDMETHOD.


  METHOD test_list_games.
    " Test that list_games returns games from SMW0
    DATA(lt_games) = mo_cut->zif_ork_00_game_loader~list_games( ).

    " Should find at least ZORK-MINI.Z3
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_games
      msg = 'Should find games in SMW0' ).

    " Check for ZORK-MINI.Z3 specifically
    DATA(lv_found) = abap_false.
    LOOP AT lt_games INTO DATA(ls_game).
      IF ls_game-id CS 'ZORK-MINI'.
        lv_found = abap_true.
        " Verify game info
        cl_abap_unit_assert=>assert_equals(
          act = ls_game-version
          exp = 'V3'
          msg = 'ZORK-MINI should be V3' ).
        cl_abap_unit_assert=>assert_not_initial(
          act = ls_game-filesize
          msg = 'Should have filesize' ).
        EXIT.
      ENDIF.
    ENDLOOP.

    cl_abap_unit_assert=>assert_true(
      act = lv_found
      msg = 'Should find ZORK-MINI.Z3 in SMW0' ).
  ENDMETHOD.


  METHOD test_load_zork_mini.
    " Test loading ZORK-MINI.Z3
    DATA(lv_story) = mo_cut->zif_ork_00_game_loader~load( 'ZORK-MINI.Z3' ).

    " Should return binary data
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_story
      msg = 'Should load ZORK-MINI.Z3' ).

    " Check Z-Machine version byte (should be 3)
    DATA(lv_version) = lv_story+0(1).
    cl_abap_unit_assert=>assert_equals(
      act = lv_version
      exp = CONV xstring( '03' )
      msg = 'First byte should be 03 (Z-Machine V3)' ).

    " Check reasonable size (> 10KB)
    cl_abap_unit_assert=>assert_true(
      act = boolc( xstrlen( lv_story ) > 10000 )
      msg = 'Story file should be > 10KB' ).
  ENDMETHOD.


  METHOD test_load_nonexistent.
    " Test loading non-existent game
    DATA(lv_story) = mo_cut->zif_ork_00_game_loader~load( 'NONEXISTENT.Z99' ).

    " Should return empty
    cl_abap_unit_assert=>assert_initial(
      act = lv_story
      msg = 'Should return empty for non-existent game' ).
  ENDMETHOD.


  METHOD test_pattern_filter.
    " Test pattern filter in constructor
    DATA(lo_loader) = NEW zcl_ork_00_game_loader_smw0( '*.TXT' ).
    DATA(lt_games) = lo_loader->zif_ork_00_game_loader~list_games( ).

    " Should NOT find .Z3 files
    LOOP AT lt_games INTO DATA(ls_game).
      cl_abap_unit_assert=>assert_false(
        act = boolc( ls_game-id CS '.Z3' )
        msg = |Should not find .Z3 files with *.TXT pattern: { ls_game-id }| ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

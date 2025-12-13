*&---------------------------------------------------------------------*
*& Report ZORK_00_SPEEDRUN
*& Automated Z-Machine test runner - feed game with script
*&---------------------------------------------------------------------*
REPORT zork_01_speedrun.

*----------------------------------------------------------------------*
* Selection Screen - Game Source
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-010.
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    PARAMETERS: r_gsmw0 RADIOBUTTON GROUP gam DEFAULT 'X' USER-COMMAND gam,
                r_gfile RADIOBUTTON GROUP gam.
  SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: p_game TYPE wwwdatatab-objid AS LISTBOX VISIBLE LENGTH 50
                MODIF ID gsm.
  SELECTION-SCREEN END OF BLOCK b2.

  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
    PARAMETERS: p_gpath TYPE string LOWER CASE MODIF ID gfl.
  SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b10.

*----------------------------------------------------------------------*
* Selection Screen - Script Source
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b40 WITH FRAME TITLE TEXT-040.
  SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
    PARAMETERS: r_ssmw0 RADIOBUTTON GROUP scr DEFAULT 'X' USER-COMMAND scr,
                r_sfile RADIOBUTTON GROUP scr.
  SELECTION-SCREEN END OF BLOCK b4.

  SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
    PARAMETERS: p_script TYPE wwwdatatab-objid AS LISTBOX VISIBLE LENGTH 50
                MODIF ID ssm.
  SELECTION-SCREEN END OF BLOCK b5.

  SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-006.
    PARAMETERS: p_spath TYPE string LOWER CASE MODIF ID sfl.
  SELECTION-SCREEN END OF BLOCK b6.
SELECTION-SCREEN END OF BLOCK b40.

*----------------------------------------------------------------------*
* Initialization - Populate dropdowns
*----------------------------------------------------------------------*
INITIALIZATION.
  DATA: lo_game_loader   TYPE REF TO zcl_ork_00_game_loader_smw0,
        lo_script_loader TYPE REF TO zcl_ork_00_script_loader_smw0,
        lt_values        TYPE vrm_values,
        ls_value         TYPE vrm_value.

  " Populate game dropdown (*.Z*)
  lo_game_loader = NEW zcl_ork_00_game_loader_smw0( '*-Z*' ).
  DATA(lt_games) = lo_game_loader->zif_ork_00_game_loader~list_games( ).

  CLEAR lt_values.
  LOOP AT lt_games INTO DATA(ls_game).
    ls_value-key = ls_game-id.
    ls_value-text = |{ ls_game-id } - { ls_game-description } ({ ls_game-version })|.
    APPEND ls_value TO lt_values.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_GAME'
      values = lt_values.

  IF lines( lt_values ) > 0.
    p_game = lt_values[ 1 ]-key.
  ENDIF.

  " Populate script dropdown (*.TXT)
  lo_script_loader = NEW zcl_ork_00_script_loader_smw0( '*-TXT' ).
  DATA(lt_scripts) = lo_script_loader->zif_ork_00_script_loader~list_scripts( ).

  CLEAR lt_values.
  LOOP AT lt_scripts INTO DATA(ls_script).
    ls_value-key = ls_script-id.
    ls_value-text = |{ ls_script-id } - { ls_script-description }|.
    APPEND ls_value TO lt_values.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_SCRIPT'
      values = lt_values.

  IF lines( lt_values ) > 0.
    p_script = lt_values[ 1 ]-key.
  ENDIF.

*----------------------------------------------------------------------*
* At Selection Screen Output - Enable/Disable fields
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " Game SMW0 dropdown
    IF screen-group1 = 'GSM'.
      screen-active = COND #( WHEN r_gsmw0 = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    " Game file path
    IF screen-group1 = 'GFL'.
      screen-active = COND #( WHEN r_gfile = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    " Script SMW0 dropdown
    IF screen-group1 = 'SSM'.
      screen-active = COND #( WHEN r_ssmw0 = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    " Script file path
    IF screen-group1 = 'SFL'.
      screen-active = COND #( WHEN r_sfile = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

FORM select_script_file.
  DATA: lt_filetab TYPE filetable,
        lv_rc      TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title      = 'Select Script File (commands)'
      file_filter       = 'Text Files (*.txt)|*.txt|All (*.*)|*.*'
      default_extension = 'txt'
    CHANGING
      file_table        = lt_filetab
      rc                = lv_rc ).

  IF lv_rc >= 1.
    p_spath = lt_filetab[ 1 ]-filename.
  ENDIF.
ENDFORM.

FORM select_story_file.
  DATA: lt_filetab TYPE filetable,
        lv_rc      TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title      = 'Select Z-Machine Story File'
      file_filter       = 'Z-Machine (*.z3;*.z5;*.z8)|*.z3;*.z5;*.z8|All (*.*)|*.*'
      default_extension = 'z3'
    CHANGING
      file_table        = lt_filetab
      rc                = lv_rc ).

  IF lv_rc >= 1.
    p_gpath = lt_filetab[ 1 ]-filename.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* At Selection Screen - F4 Help for file paths
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_gpath.
  PERFORM select_story_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_spath.
  PERFORM select_script_file.


START-OF-SELECTION.
  DATA: lv_story    TYPE xstring,
        lt_commands TYPE zif_ork_00_script_loader=>tt_commands,
        lo_speedrun TYPE REF TO zcl_ork_00_speedrun.

  WRITE: / |{ repeat( val = '=' occ = 60 ) }|.
  WRITE: / 'ZORK_00_SPEEDRUN - Z-Machine Test Runner'.
  WRITE: / |{ repeat( val = '=' occ = 60 ) }|.
  SKIP.

  " Load game
  IF r_gsmw0 = abap_true.
    IF p_game IS INITIAL.
      WRITE: / 'Error: No game selected from SMW0' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    WRITE: / |Game: { p_game } (SMW0)|.
    DATA(lo_gl_smw0) = NEW zcl_ork_00_game_loader_smw0( ).
    lv_story = lo_gl_smw0->zif_ork_00_game_loader~load( CONV #( p_game ) ).
  ELSE.
    IF p_gpath IS INITIAL.
      WRITE: / 'Error: No game file path specified' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    WRITE: / |Game: { p_gpath } (File)|.
    DATA(lo_file_loader) = NEW zcl_ork_00_game_loader_file( p_gpath ).
    lv_story = lo_file_loader->zif_ork_00_game_loader~load( ).
  ENDIF.

  IF lv_story IS INITIAL.
    WRITE: / 'Failed to load game file.' COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  WRITE: / |Size: { xstrlen( lv_story ) } bytes|.

  " Load script
  IF r_ssmw0 = abap_true.
    IF p_script IS INITIAL.
      WRITE: / 'Error: No script selected from SMW0' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    WRITE: / |Script: { p_script } (SMW0)|.
    DATA(lo_sl_smw0) = NEW zcl_ork_00_script_loader_smw0( ).
    lt_commands = lo_sl_smw0->zif_ork_00_script_loader~load( CONV #( p_script ) ).
  ELSE.
    IF p_spath IS INITIAL.
      WRITE: / 'Error: No script file path specified' COLOR COL_NEGATIVE.
      RETURN.
    ENDIF.
    WRITE: / |Script: { p_spath } (File)|.
    DATA(lo_sl_file) = NEW zcl_ork_00_script_loader_file( p_spath ).
    lt_commands = lo_sl_file->zif_ork_00_script_loader~load( ).
  ENDIF.

  IF lt_commands IS INITIAL.
    WRITE: / 'Failed to load script or script is empty.' COLOR COL_NEGATIVE.
    RETURN.
  ENDIF.

  WRITE: / |Commands: { lines( lt_commands ) }|.
  SKIP.

  WRITE: / |{ repeat( val = '-' occ = 60 ) }|.
  WRITE: / 'GAME OUTPUT'.
  WRITE: / |{ repeat( val = '-' occ = 60 ) }|.
  SKIP.

  " Run speedrun
  TRY.
      lo_speedrun = NEW zcl_ork_00_speedrun(
        iv_story    = lv_story
        it_commands = lt_commands ).

      DATA(ls_result) = lo_speedrun->run( ).

      " Print color-coded log
      lo_speedrun->print_log( ).

      " Summary
      SKIP.
      WRITE: / |{ repeat( val = '=' occ = 60 ) }|.
      IF ls_result-success = abap_true.
        WRITE: / |Commands: { ls_result-commands_run }/{ ls_result-commands_total }|.

        " Assertion summary
        IF ls_result-assertions_total > 0.
          SKIP.
          WRITE: / |Assertions: { ls_result-assertions_pass }/{ ls_result-assertions_total } passed|.
          IF ls_result-assertions_fail = 0.
            WRITE: / '*** ALL ASSERTIONS PASSED ***' COLOR COL_POSITIVE.
          ELSE.
            WRITE: / |*** { ls_result-assertions_fail } ASSERTION(S) FAILED ***| COLOR COL_NEGATIVE.
          ENDIF.
        ENDIF.
        IF ls_result-game_ended = abap_true.
          WRITE: / 'Game ended during run' COLOR COL_TOTAL.
        ENDIF.
      ELSE.
        WRITE: / |Failed: { ls_result-error_message }| COLOR COL_NEGATIVE.
      ENDIF.
      WRITE: / |{ repeat( val = '=' occ = 60 ) }|.

    CATCH cx_root INTO DATA(lx_error).
      WRITE: / 'ERROR:' COLOR COL_NEGATIVE, lx_error->get_text( ).
  ENDTRY.

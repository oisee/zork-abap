*&---------------------------------------------------------------------*
*& Report ZORK_00_CONSOLE
*& Z-Machine V3 Interactive Console
*& HTML-based display with input field - dynamic line count
*&---------------------------------------------------------------------*
REPORT zork_01_console.

CONSTANTS:
  gc_default_max_lines TYPE i VALUE 36,
  gc_font_size         TYPE i VALUE 14,    " pixels for HTML
  gc_line_height       TYPE f VALUE '1.4'. " HTML line-height multiplier

*----------------------------------------------------------------------*
* Local Class Definition - HTML Display Helper
*----------------------------------------------------------------------*
CLASS lcl_html_display DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_container TYPE REF TO cl_gui_container
          iv_max_lines TYPE i DEFAULT gc_default_max_lines,
      set_max_lines
        IMPORTING iv_max_lines TYPE i,
      get_max_lines
        RETURNING VALUE(rv_max_lines) TYPE i,
      append_text
        IMPORTING iv_text TYPE string,
      append_to_last_line
        IMPORTING iv_text TYPE string,
      clear,
      refresh,
      free.

  PRIVATE SECTION.
    DATA:
      mo_html_viewer TYPE REF TO cl_gui_html_viewer,
      mt_lines       TYPE STANDARD TABLE OF string WITH EMPTY KEY,
      mv_max_lines   TYPE i.

    METHODS:
      build_html
        RETURNING VALUE(rv_html) TYPE string,
      escape_html
        IMPORTING iv_text        TYPE string
        RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.


*----------------------------------------------------------------------*
* Local Class Definition - Console Controller
*----------------------------------------------------------------------*
CLASS lcl_console_controller DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_story TYPE xstring,
      initialize_screen
        IMPORTING io_container TYPE REF TO cl_gui_custom_container,
      handle_command
        IMPORTING iv_command TYPE sy-ucomm,
      cleanup.

  PRIVATE SECTION.
    DATA:
      mo_zmachine   TYPE REF TO zcl_ork_00_zmachine,
      mo_display    TYPE REF TO lcl_html_display,
      mo_container  TYPE REF TO cl_gui_custom_container,
      mv_story      TYPE xstring,
      mv_running    TYPE abap_bool,
      mv_started    TYPE abap_bool.

    METHODS:
      start_game,
      execute_step,
      process_input,
      calculate_max_lines
        RETURNING VALUE(rv_lines) TYPE i.
ENDCLASS.


*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE TEXT-010.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: r_smw0  RADIOBUTTON GROUP src DEFAULT 'X' USER-COMMAND src,
              r_file  RADIOBUTTON GROUP src.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_game TYPE wwwdatatab-objid AS LISTBOX VISIBLE LENGTH 40
              MODIF ID smw.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_path TYPE string LOWER CASE MODIF ID fil.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b10.


*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gv_input TYPE string,
      gv_ok    TYPE sy-ucomm.

DATA: go_controller TYPE REF TO lcl_console_controller,
      go_container  TYPE REF TO cl_gui_custom_container,
      gv_story      TYPE xstring.


*----------------------------------------------------------------------*
* Selection Screen Events
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  " Build game list for dropdown
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).
  DATA(lt_games) = lo_game_loader->zif_ork_00_game_loader~list_games( ).
  LOOP AT lt_games INTO DATA(ls_game).
    ls_value-key = ls_game-id.
    ls_value-text = |{ ls_game-id } ({ ls_game-version }, { ls_game-filesize } bytes)|.
    APPEND ls_value TO lt_values.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING id     = 'P_GAME'
              values = lt_values.

  " Enable/disable fields based on radio button
  LOOP AT SCREEN.
    " SMW0 game dropdown
    IF screen-group1 = 'SMW'.
      screen-active = COND #( WHEN r_smw0 = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
    " File path
    IF screen-group1 = 'FIL'.
      screen-active = COND #( WHEN r_file = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  " File open dialog for local file
  DATA: lt_filetab TYPE filetable,
        lv_rc      TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title      = 'Select Z-Machine Story File'
      file_filter       = 'Z-Machine (*.z3;*.z5;*.z8)|*.z3;*.z5;*.z8|All Files (*.*)|*.*'
      default_extension = 'z3'
    CHANGING
      file_table        = lt_filetab
      rc                = lv_rc ).

  IF lv_rc >= 1.
    READ TABLE lt_filetab INDEX 1 INTO DATA(ls_file).
    p_path = ls_file-filename.
  ENDIF.


*----------------------------------------------------------------------*
* Class Implementations
*----------------------------------------------------------------------*
CLASS lcl_html_display IMPLEMENTATION.

  METHOD constructor.
    mv_max_lines = iv_max_lines.
    mo_html_viewer = NEW #( parent = io_container ).
  ENDMETHOD.

  METHOD set_max_lines.
    mv_max_lines = iv_max_lines.
  ENDMETHOD.

  METHOD get_max_lines.
    rv_max_lines = mv_max_lines.
  ENDMETHOD.

  METHOD append_text.
    DATA: lt_new_lines TYPE STANDARD TABLE OF string,
          lv_text      TYPE string.

    lv_text = iv_text.
    SPLIT lv_text AT cl_abap_char_utilities=>newline INTO TABLE lt_new_lines.

    LOOP AT lt_new_lines INTO DATA(lv_line).
      APPEND lv_line TO mt_lines.
    ENDLOOP.
  ENDMETHOD.

  METHOD append_to_last_line.
    DATA(lv_count) = lines( mt_lines ).
    IF lv_count > 0.
      FIELD-SYMBOLS <fs_line> TYPE string.
      READ TABLE mt_lines INDEX lv_count ASSIGNING <fs_line>.
      IF sy-subrc = 0.
        <fs_line> = <fs_line> && iv_text.
      ENDIF.
    ELSE.
      APPEND iv_text TO mt_lines.
    ENDIF.
  ENDMETHOD.

  METHOD clear.
    CLEAR mt_lines.
  ENDMETHOD.

  METHOD refresh.
    DATA: lt_html TYPE TABLE OF char1024,
          lv_html TYPE string,
          lv_url  TYPE c LENGTH 250.

    lv_html = build_html( ).

    WHILE strlen( lv_html ) > 0.
      IF strlen( lv_html ) > 1024.
        APPEND lv_html+0(1024) TO lt_html.
        lv_html = lv_html+1024.
      ELSE.
        APPEND lv_html TO lt_html.
        CLEAR lv_html.
      ENDIF.
    ENDWHILE.

    mo_html_viewer->load_data(
      IMPORTING assigned_url = lv_url
      CHANGING  data_table   = lt_html ).
    mo_html_viewer->show_url( url = lv_url ).
  ENDMETHOD.

  METHOD build_html.
    DATA: lv_content TYPE string,
          lv_start   TYPE i,
          lv_total   TYPE i.

    lv_total = lines( mt_lines ).
    IF lv_total > mv_max_lines.
      lv_start = lv_total - mv_max_lines + 1.
    ELSE.
      lv_start = 1.
    ENDIF.

    LOOP AT mt_lines INTO DATA(lv_line) FROM lv_start.
      lv_content = lv_content && escape_html( lv_line ) && |<br>|.
    ENDLOOP.

    rv_html =
      |<html><head><style>| &&
      |body \{ background-color: #1a1a2e; color: #00ff00; | &&
      |font-family: 'Courier New', monospace; font-size: { gc_font_size }px; | &&
      |padding: 10px; margin: 0; \}| &&
      |.output \{ white-space: pre-wrap; line-height: { gc_line_height }; \}| &&
      |</style></head><body>| &&
      |<div class="output">{ lv_content }</div>| &&
      |</body></html>|.
  ENDMETHOD.

  METHOD escape_html.
    rv_text = iv_text.
    REPLACE ALL OCCURRENCES OF '&' IN rv_text WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<' IN rv_text WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>' IN rv_text WITH '&gt;'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_text WITH '&quot;'.
  ENDMETHOD.

  METHOD free.
    IF mo_html_viewer IS BOUND.
      mo_html_viewer->free( ).
      FREE mo_html_viewer.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_console_controller IMPLEMENTATION.

  METHOD constructor.
    mv_story = iv_story.
    mv_running = abap_false.
    mv_started = abap_false.
  ENDMETHOD.

  METHOD initialize_screen.
    mo_container = io_container.

    " Calculate max lines based on container height
    DATA(lv_max_lines) = calculate_max_lines( ).

    mo_display = NEW lcl_html_display(
      io_container = io_container
      iv_max_lines = lv_max_lines ).

    start_game( ).
  ENDMETHOD.

  METHOD calculate_max_lines.
    " Use screen rows (sy-srows) minus space for input field and status
    " sy-srows = total screen rows in text mode
    " Reserve ~7 rows for input field, status bar, title, borders
    IF sy-srows > 10.
      rv_lines = sy-srows - 7.
    ELSE.
      rv_lines = gc_default_max_lines.
    ENDIF.

    " Ensure minimum of 10 lines
    IF rv_lines < 10.
      rv_lines = 10.
    ENDIF.
  ENDMETHOD.

  METHOD handle_command.
    CASE iv_command.
      WHEN 'EXIT' OR 'BACK' OR 'CANC' OR '&F03' OR '&F15' OR '&F12'.
        cleanup( ).
        LEAVE PROGRAM.

      WHEN 'ENTER' OR '' OR 'ONLI'.
        process_input( ).
    ENDCASE.
  ENDMETHOD.

  METHOD start_game.
    IF mv_story IS INITIAL.
      mo_display->append_text( |Error: No story file loaded| ).
      mo_display->refresh( ).
      RETURN.
    ENDIF.

    TRY.
        mo_zmachine = NEW zcl_ork_00_zmachine( mv_story ).
        mv_started = abap_true.
        mv_running = abap_true.
        mo_display->clear( ).
        execute_step( ).
      CATCH cx_root INTO DATA(lx_error).
        mo_display->append_text( |Error: { lx_error->get_text( ) }| ).
        mo_display->refresh( ).
    ENDTRY.
  ENDMETHOD.

  METHOD execute_step.
    CHECK mo_zmachine IS BOUND.

    mo_zmachine->run( ).
    DATA(ls_status) = mo_zmachine->get_status( ).

    IF ls_status-output IS NOT INITIAL.
      mo_display->append_text( ls_status-output ).
    ENDIF.

    mv_running = ls_status-running.
    mo_display->refresh( ).
  ENDMETHOD.

  METHOD process_input.
    DATA: lv_cmd TYPE string.

    lv_cmd = gv_input.
    CLEAR gv_input.

    IF lv_cmd IS INITIAL.
      RETURN.
    ENDIF.

    " Check meta commands
    DATA(lv_upper) = to_upper( lv_cmd ).
    CASE lv_upper.
      WHEN '/Q' OR '/QUIT' OR '/EXIT'.
        mo_display->append_to_last_line( lv_cmd ).
        mo_display->append_text( |Goodbye!| ).
        mo_display->refresh( ).
        cleanup( ).
        LEAVE PROGRAM.

      WHEN '/HELP' OR '/?'.
        mo_display->append_to_last_line( lv_cmd ).
        mo_display->append_text( |Commands: /quit /help| ).
        mo_display->append_text( |Game: LOOK, N/S/E/W, INVENTORY, TAKE, DROP| ).
        mo_display->refresh( ).
        RETURN.
    ENDCASE.

    " Echo input on same line as > prompt
    mo_display->append_to_last_line( lv_cmd ).

    " Provide to Z-Machine
    IF mo_zmachine IS BOUND AND mv_running = abap_true.
      mo_zmachine->provide_input( lv_cmd ).
      execute_step( ).
    ENDIF.
  ENDMETHOD.

  METHOD cleanup.
    IF mo_display IS BOUND.
      mo_display->free( ).
      FREE mo_display.
    ENDIF.
    FREE mo_zmachine.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " Load story based on selection
  IF r_smw0 = abap_true.
    IF p_game IS INITIAL.
      MESSAGE 'Please select a game from SMW0' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    DATA(lo_smw0_loader) = NEW zcl_ork_00_game_loader_smw0( ).
    gv_story = lo_smw0_loader->zif_ork_00_game_loader~load( CONV #( p_game ) ).
  ELSE.
    DATA(lo_file_loader) = NEW zcl_ork_00_game_loader_file( p_path ).
    gv_story = lo_file_loader->zif_ork_00_game_loader~load( ).
  ENDIF.

  IF gv_story IS INITIAL.
    MESSAGE 'Failed to load story file' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " Check version
  DATA(lv_version) = zcl_ork_00_game_loader=>get_version( gv_story ).
  IF lv_version <> 3.
    MESSAGE |Warning: Story is V{ lv_version }, interpreter supports V3| TYPE 'W'.
  ENDIF.

  go_controller = NEW lcl_console_controller( gv_story ).
  CALL SCREEN 100.


*----------------------------------------------------------------------*
* Screen 100 PBO
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TITLE'.

  IF go_container IS NOT BOUND.
    CREATE OBJECT go_container
      EXPORTING container_name = 'HTML_CONTAINER'
      EXCEPTIONS OTHERS = 1.

    IF sy-subrc = 0.
      go_controller->initialize_screen( go_container ).
    ENDIF.
  ENDIF.
ENDMODULE.


*----------------------------------------------------------------------*
* Screen 100 PAI
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  go_controller->handle_command( gv_ok ).
  CLEAR gv_ok.
ENDMODULE.

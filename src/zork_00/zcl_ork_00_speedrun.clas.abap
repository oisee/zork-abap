*&---------------------------------------------------------------------*
*& Class ZCL_ORK_00_SPEEDRUN
*& Run Z-Machine games against command scripts and capture log
*&
*& Script syntax:
*&   command     - send command to game
*&   %*pattern*  - expect output containing pattern (wildcard, case-insensitive)
*&   %=exact     - expect exact text (case-sensitive)
*&   %!pattern   - expect NOT containing pattern
*&   # comment   - ignored
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_speedrun DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    " Log entry roles
    CONSTANTS:
      c_role_output   TYPE c LENGTH 1 VALUE 'O',  " Game output
      c_role_input    TYPE c LENGTH 1 VALUE 'I',  " User input (command)
      c_role_system   TYPE c LENGTH 1 VALUE 'S',  " System message
      c_role_error    TYPE c LENGTH 1 VALUE 'E',  " Error message
      c_role_expected TYPE c LENGTH 1 VALUE '+',  " Assertion PASSED (green)
      c_role_failed   TYPE c LENGTH 1 VALUE '-'.  " Assertion FAILED (red)

    " Log entry structure
    TYPES: BEGIN OF ts_log_entry,
             seq  TYPE i,          " Sequence number
             role TYPE c LENGTH 1, " O/I/S/E/+/-
             text TYPE string,     " Content
           END OF ts_log_entry,
           tt_log TYPE STANDARD TABLE OF ts_log_entry WITH EMPTY KEY.

    " Run result structure
    TYPES: BEGIN OF ts_result,
             success          TYPE abap_bool,
             commands_total   TYPE i,
             commands_run     TYPE i,
             assertions_total TYPE i,
             assertions_pass  TYPE i,
             assertions_fail  TYPE i,
             game_ended       TYPE abap_bool,
             error_message    TYPE string,
           END OF ts_result.

    " Constructor - accepts game story and script lines
    METHODS constructor
      IMPORTING
        iv_story    TYPE xstring
        it_commands TYPE zif_ork_00_script_loader=>tt_commands.

    " Run the speedrun
    METHODS run
      RETURNING VALUE(rs_result) TYPE ts_result.

    " Get the log
    METHODS get_log
      RETURNING VALUE(rt_log) TYPE tt_log.

    " Get log as formatted string
    METHODS get_log_as_text
      RETURNING VALUE(rv_text) TYPE string.

    " Print log to WRITE output with colors
    METHODS print_log.

  PRIVATE SECTION.
    " Assertion types
    CONSTANTS:
      c_assert_wildcard TYPE c LENGTH 1 VALUE '*',  " %*pattern*
      c_assert_exact    TYPE c LENGTH 1 VALUE '=',  " %=exact
      c_assert_not      TYPE c LENGTH 1 VALUE '!'.  " %!pattern

    " Parsed script entry
    TYPES: BEGIN OF ts_script_entry,
             is_command    TYPE abap_bool,
             is_assertion  TYPE abap_bool,
             text          TYPE string,
             assert_type   TYPE c LENGTH 1,
             assert_pattern TYPE string,
           END OF ts_script_entry,
           tt_script TYPE STANDARD TABLE OF ts_script_entry WITH EMPTY KEY.

    DATA: mv_story    TYPE xstring,
          mt_script   TYPE tt_script,
          mt_log      TYPE tt_log,
          mv_seq      TYPE i,
          mv_assert_total TYPE i,
          mv_assert_pass  TYPE i,
          mv_assert_fail  TYPE i.

    " Parse script into commands and assertions
    METHODS parse_script
      IMPORTING it_lines  TYPE zif_ork_00_script_loader=>tt_commands.

    " Add entry to log
    METHODS add_log
      IMPORTING
        iv_role TYPE c
        iv_text TYPE string.

    " Add multiline text to log (splits by newline)
    METHODS add_log_multiline
      IMPORTING
        iv_role TYPE c
        iv_text TYPE string.

    " Strip trailing prompt from output
    METHODS strip_prompt
      CHANGING cv_output TYPE string.

    " Check assertions against output
    METHODS check_assertions
      IMPORTING
        iv_output     TYPE string
        it_assertions TYPE tt_script.

    " Check single assertion
    METHODS check_assertion
      IMPORTING
        is_assertion   TYPE ts_script_entry
        iv_output      TYPE string
      RETURNING VALUE(rv_pass) TYPE abap_bool.

ENDCLASS.


CLASS zcl_ork_00_speedrun IMPLEMENTATION.

  METHOD constructor.
    mv_story = iv_story.
    parse_script( it_lines = it_commands ).
    mv_seq = 0.
    mv_assert_total = 0.
    mv_assert_pass = 0.
    mv_assert_fail = 0.
  ENDMETHOD.


  METHOD parse_script.
    DATA ls_entry TYPE ts_script_entry.

    CLEAR mt_script.

    LOOP AT it_lines INTO DATA(lv_line).
      CLEAR ls_entry.

      " Skip empty lines (should already be filtered, but just in case)
      IF lv_line IS INITIAL.
        CONTINUE.
      ENDIF.

      " Check if assertion line (starts with %)
      IF lv_line(1) = '%'.
        ls_entry-is_assertion = abap_true.
        ls_entry-text = lv_line.

        " Parse assertion type
        IF strlen( lv_line ) > 1.
          DATA(lv_type) = lv_line+1(1).
          CASE lv_type.
            WHEN '*'.
              " %*pattern* - wildcard match
              ls_entry-assert_type = c_assert_wildcard.
              " Extract pattern between * and *
              IF strlen( lv_line ) > 2.
                DATA(lv_rest) = lv_line+2.
                " Remove trailing * if present
                DATA(lv_len) = strlen( lv_rest ).
                IF lv_len > 0 AND lv_len > 0 AND substring( val = lv_rest off = lv_len - 1 len = 1 ) = '*'.
                  lv_len = lv_len - 1.
                  ls_entry-assert_pattern = lv_rest(lv_len).
                ELSE.
                  ls_entry-assert_pattern = lv_rest.
                ENDIF.
              ENDIF.
            WHEN '='.
              " %=exact - exact match
              ls_entry-assert_type = c_assert_exact.
              IF strlen( lv_line ) > 2.
                ls_entry-assert_pattern = lv_line+2.
              ENDIF.
            WHEN '!'.
              " %!pattern - NOT match
              ls_entry-assert_type = c_assert_not.
              IF strlen( lv_line ) > 2.
                ls_entry-assert_pattern = lv_line+2.
              ENDIF.
            WHEN OTHERS.
              " Default to wildcard if no type specified: %pattern
              ls_entry-assert_type = c_assert_wildcard.
              ls_entry-assert_pattern = lv_line+1.
          ENDCASE.
        ENDIF.

        APPEND ls_entry TO mt_script.
      ELSE.
        " Regular command
        ls_entry-is_command = abap_true.
        ls_entry-text = lv_line.
        APPEND ls_entry TO mt_script.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD run.
    DATA: lo_zmachine    TYPE REF TO zcl_ork_00_zmachine,
          lv_output      TYPE string,
          lt_assertions  TYPE tt_script,
          lv_cmd_count   TYPE i.

    CLEAR mt_log.
    mv_seq = 0.
    mv_assert_total = 0.
    mv_assert_pass = 0.
    mv_assert_fail = 0.

    " Validate inputs
    IF mv_story IS INITIAL.
      add_log( iv_role = c_role_error iv_text = 'No story file provided' ).
      rs_result-success = abap_false.
      rs_result-error_message = 'No story file provided'.
      RETURN.
    ENDIF.

    IF mt_script IS INITIAL.
      add_log( iv_role = c_role_error iv_text = 'No script provided' ).
      rs_result-success = abap_false.
      rs_result-error_message = 'No script provided'.
      RETURN.
    ENDIF.

    " Count commands
    LOOP AT mt_script INTO DATA(ls_entry) WHERE is_command = abap_true.
      rs_result-commands_total = rs_result-commands_total + 1.
    ENDLOOP.

    " Initialize Z-Machine
    TRY.
        lo_zmachine = NEW zcl_ork_00_zmachine( mv_story ).

        " Get initial output (game intro)
        lo_zmachine->run( ).
        DATA(ls_status) = lo_zmachine->get_status( ).
        lv_output = ls_status-output.
        strip_prompt( CHANGING cv_output = lv_output ).
        add_log_multiline( iv_role = c_role_output iv_text = lv_output ).

        " Collect assertions until first command
        CLEAR lt_assertions.

        " Process script entries
        LOOP AT mt_script INTO ls_entry.
          IF ls_entry-is_assertion = abap_true.
            " Collect assertion to check after next output
            APPEND ls_entry TO lt_assertions.
          ELSE.
            " It's a command

            " First check pending assertions against current output
            IF lt_assertions IS NOT INITIAL.
              check_assertions( iv_output = lv_output it_assertions = lt_assertions ).
              CLEAR lt_assertions.
            ENDIF.

            lv_cmd_count = lv_cmd_count + 1.
            rs_result-commands_run = lv_cmd_count.

            " Log the input command
            add_log( iv_role = c_role_input iv_text = ls_entry-text ).

            " Provide input to Z-Machine
            lo_zmachine->provide_input( ls_entry-text ).

            " Run until waiting for input again
            lo_zmachine->run( ).
            ls_status = lo_zmachine->get_status( ).

            " Log output
            IF ls_status-output IS NOT INITIAL.
              lv_output = ls_status-output.
              strip_prompt( CHANGING cv_output = lv_output ).
              add_log_multiline( iv_role = c_role_output iv_text = lv_output ).
            ELSE.
              CLEAR lv_output.
            ENDIF.

            " Check if game ended
            IF ls_status-running = abap_false.
              rs_result-game_ended = abap_true.
              add_log( iv_role = c_role_system iv_text = '--- Game ended ---' ).
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.

        " Check any remaining assertions
        IF lt_assertions IS NOT INITIAL.
          check_assertions( iv_output = lv_output it_assertions = lt_assertions ).
        ENDIF.

        rs_result-success = abap_true.
        rs_result-assertions_total = mv_assert_total.
        rs_result-assertions_pass = mv_assert_pass.
        rs_result-assertions_fail = mv_assert_fail.

      CATCH cx_root INTO DATA(lx_error).
        rs_result-success = abap_false.
        rs_result-error_message = lx_error->get_text( ).
        add_log( iv_role = c_role_error iv_text = rs_result-error_message ).
    ENDTRY.
  ENDMETHOD.


  METHOD check_assertions.
    LOOP AT it_assertions INTO DATA(ls_assert).
      mv_assert_total = mv_assert_total + 1.

      DATA(lv_pass) = check_assertion( is_assertion = ls_assert iv_output = iv_output ).

      IF lv_pass = abap_true.
        mv_assert_pass = mv_assert_pass + 1.
        add_log( iv_role = c_role_expected iv_text = |[OK] { ls_assert-text }| ).
      ELSE.
        mv_assert_fail = mv_assert_fail + 1.
        add_log( iv_role = c_role_failed iv_text = |[FAIL] { ls_assert-text }| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_assertion.
    DATA: lv_output_lower  TYPE string,
          lv_pattern_lower TYPE string.

    rv_pass = abap_false.

    CASE is_assertion-assert_type.
      WHEN c_assert_wildcard.
        " Case-insensitive contains
        lv_output_lower = iv_output.
        lv_pattern_lower = is_assertion-assert_pattern.
        TRANSLATE lv_output_lower TO LOWER CASE.
        TRANSLATE lv_pattern_lower TO LOWER CASE.
        IF lv_output_lower CS lv_pattern_lower.
          rv_pass = abap_true.
        ENDIF.

      WHEN c_assert_exact.
        " Case-sensitive contains
        IF iv_output CS is_assertion-assert_pattern.
          rv_pass = abap_true.
        ENDIF.

      WHEN c_assert_not.
        " NOT contains (case-insensitive)
        lv_output_lower = iv_output.
        lv_pattern_lower = is_assertion-assert_pattern.
        TRANSLATE lv_output_lower TO LOWER CASE.
        TRANSLATE lv_pattern_lower TO LOWER CASE.
        IF NOT lv_output_lower CS lv_pattern_lower.
          rv_pass = abap_true.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD get_log.
    rt_log = mt_log.
  ENDMETHOD.


  METHOD get_log_as_text.
    LOOP AT mt_log INTO DATA(ls_entry).
      CASE ls_entry-role.
        WHEN c_role_input.
          rv_text = rv_text && |>{ ls_entry-text }| && cl_abap_char_utilities=>newline.
        WHEN c_role_output OR c_role_system.
          rv_text = rv_text && ls_entry-text && cl_abap_char_utilities=>newline.
        WHEN c_role_error OR c_role_failed.
          rv_text = rv_text && |ERROR: { ls_entry-text }| && cl_abap_char_utilities=>newline.
        WHEN c_role_expected.
          rv_text = rv_text && ls_entry-text && cl_abap_char_utilities=>newline.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD print_log.
    LOOP AT mt_log INTO DATA(ls_entry).
      CASE ls_entry-role.
        WHEN c_role_input.
          " User input - highlight
          WRITE: / |>{ ls_entry-text }| COLOR COL_KEY.
        WHEN c_role_output.
          " Game output - normal
          WRITE: / ls_entry-text.
        WHEN c_role_system.
          " System message - total
          WRITE: / ls_entry-text COLOR COL_TOTAL.
        WHEN c_role_expected.
          " Assertion passed - green
          WRITE: / ls_entry-text COLOR COL_POSITIVE.
        WHEN c_role_failed.
          " Assertion failed - red
          WRITE: / ls_entry-text COLOR COL_NEGATIVE.
        WHEN c_role_error.
          " Error - red
          WRITE: / ls_entry-text COLOR COL_NEGATIVE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_log.
    DATA ls_entry TYPE ts_log_entry.

    mv_seq = mv_seq + 1.
    ls_entry-seq = mv_seq.
    ls_entry-role = iv_role.
    ls_entry-text = iv_text.
    APPEND ls_entry TO mt_log.
  ENDMETHOD.


  METHOD add_log_multiline.
    DATA lt_lines TYPE string_table.

    IF iv_text IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT iv_text AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    LOOP AT lt_lines INTO DATA(lv_line).
      add_log( iv_role = iv_role iv_text = lv_line ).
    ENDLOOP.
  ENDMETHOD.


  METHOD strip_prompt.
    DATA lt_lines TYPE string_table.

    SPLIT cv_output AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    " Remove trailing empty lines and ">" prompts
    WHILE lines( lt_lines ) > 0.
      DATA(lv_last_idx) = lines( lt_lines ).
      DATA(lv_last) = lt_lines[ lv_last_idx ].
      IF lv_last = '>' OR lv_last IS INITIAL.
        DELETE lt_lines INDEX lv_last_idx.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.

    " Rebuild output
    CLEAR cv_output.
    LOOP AT lt_lines INTO DATA(lv_line).
      IF sy-tabix > 1.
        cv_output = cv_output && cl_abap_char_utilities=>newline.
      ENDIF.
      cv_output = cv_output && lv_line.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


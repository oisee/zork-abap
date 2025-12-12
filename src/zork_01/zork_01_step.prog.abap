*&---------------------------------------------------------------------*
*& Report zork_00_RUN - Z-Machine V3 Runner
*& Loads ZORK.Z3 from SMW0 ($ZORK package)
*&---------------------------------------------------------------------*
REPORT zork_01_step.

DATA: go_zmachine TYPE REF TO zcl_ork_00_zmachine,
      gv_story    TYPE xstring.

PARAMETERS: p_cmd TYPE string LOWER CASE DEFAULT 'look'.

START-OF-SELECTION.
  PERFORM main.

FORM main.
  DATA: ls_status TYPE zcl_ork_00_zmachine=>ty_status.

  PERFORM load_story CHANGING gv_story.

  IF gv_story IS INITIAL.
    WRITE: / 'Error: Could not load story file'.
    RETURN.
  ENDIF.

  TRY.
      go_zmachine = NEW zcl_ork_00_zmachine( gv_story ).
    CATCH cx_root INTO DATA(lx_error).
      WRITE: / 'Error:', lx_error->get_text( ).
      RETURN.
  ENDTRY.

  WRITE: / 'Z-Machine initialized'.

  go_zmachine->run( ).
  ls_status = go_zmachine->get_status( ).
  PERFORM display_output USING ls_status-output.

  IF ls_status-waiting = abap_true.
    WRITE: / 'Command:', p_cmd.
    go_zmachine->provide_input( p_cmd ).
    go_zmachine->run( ).
    ls_status = go_zmachine->get_status( ).
    PERFORM display_output USING ls_status-output.
  ENDIF.

  WRITE: / 'Running:', ls_status-running, 'Waiting:', ls_status-waiting.
ENDFORM.

FORM display_output USING pv_output TYPE string.
  DATA: lt_lines TYPE STANDARD TABLE OF string.
  IF pv_output IS INITIAL.
    RETURN.
  ENDIF.
  SPLIT pv_output AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
  LOOP AT lt_lines INTO DATA(lv_line).
    WRITE: / lv_line.
  ENDLOOP.
ENDFORM.

FORM load_story CHANGING cv_story TYPE xstring.
  " Load ZORK.Z3 from SMW0 ($ZORK package)
  DATA: lt_mime   TYPE STANDARD TABLE OF w3mime,
        ls_key    TYPE wwwdatatab,
        lt_params TYPE STANDARD TABLE OF wwwparams,
        ls_param  TYPE wwwparams,
        lv_size   TYPE i.

  ls_key-relid = 'MI'.
  ls_key-objid = 'ZORK.Z3'.

  SELECT * FROM wwwparams INTO TABLE lt_params
    WHERE relid = ls_key-relid AND objid = ls_key-objid.

  IF sy-subrc <> 0.
    WRITE: / 'ZORK.Z3 not found in SMW0 ($ZORK package)'.
    RETURN.
  ENDIF.

  READ TABLE lt_params INTO ls_param WITH KEY name = 'filesize'.
  IF sy-subrc = 0.
    lv_size = ls_param-value.
  ENDIF.

  CALL FUNCTION 'WWWDATA_IMPORT'
    EXPORTING
      key    = ls_key
    TABLES
      mime   = lt_mime
    EXCEPTIONS
      OTHERS = 1.

  IF sy-subrc <> 0.
    WRITE: / 'Error loading ZORK.Z3'.
    RETURN.
  ENDIF.

  LOOP AT lt_mime INTO DATA(ls_mime).
    CONCATENATE cv_story ls_mime-line INTO cv_story IN BYTE MODE.
  ENDLOOP.
  cv_story = cv_story(lv_size).

  WRITE: / 'Loaded ZORK.Z3,', lv_size, 'bytes'.
ENDFORM.

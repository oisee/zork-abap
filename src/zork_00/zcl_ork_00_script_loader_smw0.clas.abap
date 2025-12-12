*&---------------------------------------------------------------------*
*& Class ZCL_ORK_00_SCRIPT_LOADER_SMW0
*& Load command scripts from SMW0 (MIME Repository)
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_script_loader_smw0 DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ork_00_script_loader.

    " Constructor with optional pattern filter
    METHODS constructor
      IMPORTING iv_pattern TYPE string DEFAULT '*-TXT'.

  PRIVATE SECTION.
    DATA mv_pattern TYPE string.

    " Load raw text from SMW0
    METHODS load_text
      IMPORTING iv_objid       TYPE wwwdatatab-objid
      RETURNING VALUE(rv_text) TYPE string.

ENDCLASS.


CLASS zcl_ork_00_script_loader_smw0 IMPLEMENTATION.

  METHOD constructor.
    mv_pattern = iv_pattern.
  ENDMETHOD.


  METHOD zif_ork_00_script_loader~list_scripts.
    DATA: lt_params  TYPE STANDARD TABLE OF wwwparams,
          ls_script  TYPE zif_ork_00_script_loader=>ts_script_info,
          lv_pattern TYPE string.

    " Convert pattern for LIKE comparison
    lv_pattern = mv_pattern.
    REPLACE ALL OCCURRENCES OF '*' IN lv_pattern WITH '%'.

    " Get all MIME objects matching pattern
    SELECT DISTINCT objid FROM wwwparams
      INTO TABLE @DATA(lt_objids)
      WHERE relid = 'MI'
        AND objid LIKE @lv_pattern.

    LOOP AT lt_objids INTO DATA(ls_objid).
      CLEAR ls_script.
      ls_script-id = ls_objid-objid.

      " Get description parameter
      SELECT SINGLE value FROM wwwparams
        INTO @DATA(lv_desc)
        WHERE relid = 'MI'
          AND objid = @ls_objid-objid
          AND name = 'description'.

      IF sy-subrc = 0 AND lv_desc IS NOT INITIAL.
        ls_script-description = lv_desc.
      ELSE.
        ls_script-description = ls_script-id.
      ENDIF.

      APPEND ls_script TO rt_list.
    ENDLOOP.

    " Sort by name
    SORT rt_list BY id.
  ENDMETHOD.


  METHOD zif_ork_00_script_loader~load.
    DATA: lv_text TYPE string,
          lt_lines TYPE string_table.

    IF iv_id IS INITIAL.
      RETURN.
    ENDIF.

    " Load raw text from SMW0
    lv_text = load_text( CONV #( iv_id ) ).

    IF lv_text IS INITIAL.
      RETURN.
    ENDIF.

    " Split into lines
    SPLIT lv_text AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.

    " Each line is a command (skip empty lines and comments)
    LOOP AT lt_lines INTO DATA(lv_line).
      " Also handle CR+LF
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_line WITH ''.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_line WITH ' '.

      " Trim whitespace
      CONDENSE lv_line.

      " Skip empty lines
      IF lv_line IS INITIAL.
        CONTINUE.
      ENDIF.

      " Skip comments (starting with #)
      IF strlen( lv_line ) > 0 AND lv_line(1) = '#'.
        CONTINUE.
      ENDIF.

      APPEND lv_line TO rt_commands.
    ENDLOOP.
  ENDMETHOD.


  METHOD load_text.
    DATA: lt_mime   TYPE w3mimetabtype,
          ls_key    TYPE wwwdatatab,
          lt_params TYPE STANDARD TABLE OF wwwparams,
          ls_param  TYPE wwwparams,
          lv_size   TYPE i,
          lv_xstr   TYPE xstring.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_objid.

    " Get file size
    SELECT * FROM wwwparams INTO TABLE lt_params
      WHERE relid = ls_key-relid AND objid = ls_key-objid.

    IF sy-subrc <> 0.
      RETURN.  " Not found
    ENDIF.

    READ TABLE lt_params INTO ls_param WITH KEY name = 'filesize'.
    IF sy-subrc = 0.
      lv_size = ls_param-value.
    ENDIF.

    " Import binary data
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING key  = ls_key
      TABLES    mime = lt_mime
      EXCEPTIONS OTHERS = 1.

    IF sy-subrc <> 0.
      RETURN.  " Import failed
    ENDIF.

    " Convert to xstring
    CONCATENATE LINES OF lt_mime INTO lv_xstr IN BYTE MODE.
    lv_xstr = lv_xstr(lv_size).

    " Convert xstring to string (UTF-8)
    rv_text = cl_abap_codepage=>convert_from( lv_xstr ).
  ENDMETHOD.

ENDCLASS.


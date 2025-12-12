*&---------------------------------------------------------------------*
*& Class ZCL_ORK_00_GAME_LOADER_SMW0
*& Load Z-Machine games from SMW0 (MIME Repository)
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_game_loader_smw0 DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ork_00_game_loader.

    " Constructor with optional pattern filter
    METHODS constructor
      IMPORTING iv_pattern TYPE string DEFAULT '*-Z*'.

  PRIVATE SECTION.
    DATA mv_pattern TYPE string.

ENDCLASS.


CLASS zcl_ork_00_game_loader_smw0 IMPLEMENTATION.

  METHOD constructor.
    mv_pattern = iv_pattern.
  ENDMETHOD.


  METHOD zif_ork_00_game_loader~list_games.
    DATA: lt_params  TYPE STANDARD TABLE OF wwwparams,
          ls_game    TYPE zif_ork_00_game_loader=>ts_game_info,
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
      CLEAR ls_game.
      ls_game-id = ls_objid-objid.

      " Get parameters for this object
      SELECT * FROM wwwparams INTO TABLE lt_params
        WHERE relid = 'MI' AND objid = ls_objid-objid.

      LOOP AT lt_params INTO DATA(ls_param).
        CASE ls_param-name.
          WHEN 'filesize'.
            ls_game-filesize = ls_param-value.
          WHEN 'description'.
            ls_game-description = ls_param-value.
          WHEN 'filename'.
            ls_game-filename = ls_param-value.
        ENDCASE.
      ENDLOOP.

      " Default description if empty
      IF ls_game-description IS INITIAL.
        ls_game-description = ls_game-id.
      ENDIF.

      " Determine version from extension
      DATA(lv_ext) = to_upper( ls_game-filename ).
      IF lv_ext CS '.Z3'.
        ls_game-version = 'V3'.
      ELSEIF lv_ext CS '.Z4'.
        ls_game-version = 'V4'.
      ELSEIF lv_ext CS '.Z5'.
        ls_game-version = 'V5'.
      ELSEIF lv_ext CS '.Z8'.
        ls_game-version = 'V8'.
      ELSE.
        ls_game-version = '?'.
      ENDIF.

      APPEND ls_game TO rt_list.
    ENDLOOP.

    " Sort by name
    SORT rt_list BY id.
  ENDMETHOD.


  METHOD zif_ork_00_game_loader~load.
    DATA: lt_mime   TYPE STANDARD TABLE OF w3mime,
          ls_key    TYPE wwwdatatab,
          lt_params TYPE STANDARD TABLE OF wwwparams,
          ls_param  TYPE wwwparams,
          lv_size   TYPE i.

    ls_key-relid = 'MI'.
    ls_key-objid = iv_id.

    " Get file size
    SELECT * FROM wwwparams INTO TABLE lt_params
      WHERE relid = ls_key-relid AND objid = ls_key-objid.

    IF sy-subrc <> 0.
      RETURN.  " Game not found
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
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING input_length = lv_size
      IMPORTING buffer       = rv_story
      TABLES    binary_tab   = lt_mime.
  ENDMETHOD.

ENDCLASS.

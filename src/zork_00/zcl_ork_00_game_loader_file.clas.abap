*&---------------------------------------------------------------------*
*& Class ZCL_ORK_00_GAME_LOADER_FILE
*& Load Z-Machine games from frontend file
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_game_loader_file DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_ork_00_game_loader.

    " Constructor with file path (opens dialog if empty)
    METHODS constructor
      IMPORTING iv_filepath TYPE string OPTIONAL.

    " Get the loaded file path
    METHODS get_filepath
      RETURNING VALUE(rv_filepath) TYPE string.

  PRIVATE SECTION.
    DATA mv_filepath TYPE string.

ENDCLASS.


CLASS zcl_ork_00_game_loader_file IMPLEMENTATION.

  METHOD constructor.
    mv_filepath = iv_filepath.
  ENDMETHOD.


  METHOD get_filepath.
    rv_filepath = mv_filepath.
  ENDMETHOD.


  METHOD zif_ork_00_game_loader~list_games.
    " File loader doesn't support listing - returns empty
    CLEAR rt_list.
  ENDMETHOD.


  METHOD zif_ork_00_game_loader~load.
    DATA: lt_filetab TYPE filetable,
          lv_rc      TYPE i,
          lt_data    TYPE STANDARD TABLE OF x255,
          lv_path    TYPE string,
          lv_len     TYPE i.

    lv_path = mv_filepath.

    " Use iv_id as path if provided
    IF iv_id IS NOT INITIAL.
      lv_path = iv_id.
    ENDIF.

    " If no path, open file dialog
    IF lv_path IS INITIAL.
      cl_gui_frontend_services=>file_open_dialog(
        EXPORTING
          window_title      = 'Select Z-Machine Story File'
          file_filter       = 'Z-Machine (*.z3;*.z5;*.z8)|*.z3;*.z5;*.z8|All (*.*)|*.*'
          default_extension = 'z3'
        CHANGING
          file_table        = lt_filetab
          rc                = lv_rc ).

      IF lv_rc < 1.
        RETURN.  " User cancelled
      ENDIF.

      READ TABLE lt_filetab INDEX 1 INTO DATA(ls_file).
      lv_path = ls_file-filename.
      mv_filepath = lv_path.
    ENDIF.

    " Upload binary file from frontend
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename   = lv_path
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_len
      CHANGING
        data_tab   = lt_data
      EXCEPTIONS
        OTHERS     = 1 ).

    IF sy-subrc <> 0.
      RETURN.  " Upload failed
    ENDIF.

    " Convert to xstring
    CONCATENATE LINES OF lt_data INTO rv_story IN BYTE MODE.
    rv_story = rv_story(lv_len).

  ENDMETHOD.

ENDCLASS.

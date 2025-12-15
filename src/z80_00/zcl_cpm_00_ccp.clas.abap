CLASS zcl_cpm_00_ccp DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_file_info,
             name        TYPE string,
             extension   TYPE string,
             size        TYPE i,
             description TYPE string,
           END OF ts_file_info,
           tt_file_list TYPE STANDARD TABLE OF ts_file_info WITH EMPTY KEY.

    CONSTANTS c_crlf TYPE string VALUE cl_abap_char_utilities=>cr_lf.

    METHODS constructor.
    METHODS reset.
    METHODS process_command
      IMPORTING iv_command TYPE string
      EXPORTING ev_output  TYPE string
                ev_run_program TYPE abap_bool
                ev_program_data TYPE xstring
                ev_program_name TYPE string.

    METHODS get_prompt RETURNING VALUE(rv_prompt) TYPE string.
    METHODS get_welcome RETURNING VALUE(rv_welcome) TYPE string.

  PRIVATE SECTION.
    DATA mv_current_drive TYPE c LENGTH 1 VALUE 'A'.
    DATA mv_current_user TYPE i VALUE 0.

    METHODS cmd_dir
      IMPORTING iv_pattern TYPE string
      RETURNING VALUE(rv_output) TYPE string.

    METHODS cmd_type
      IMPORTING iv_filename TYPE string
      RETURNING VALUE(rv_output) TYPE string.

    METHODS cmd_help
      RETURNING VALUE(rv_output) TYPE string.

    METHODS cmd_era
      IMPORTING iv_filename TYPE string
      RETURNING VALUE(rv_output) TYPE string.

    METHODS load_smw0_file
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rv_data) TYPE xstring.

    METHODS format_size
      IMPORTING iv_size TYPE i
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.


CLASS zcl_cpm_00_ccp IMPLEMENTATION.

  METHOD constructor.
    reset( ).
  ENDMETHOD.

  METHOD reset.
    mv_current_drive = 'A'.
    mv_current_user = 0.
  ENDMETHOD.

  METHOD get_prompt.
    rv_prompt = |{ mv_current_drive }>|.
  ENDMETHOD.

  METHOD get_welcome.
    rv_welcome =
      |CP/M 2.2 on SAP HANA{ c_crlf }| &&
      |64K TPA  Z80 Emulator{ c_crlf }| &&
      |{ c_crlf }| &&
      |Type HELP for commands{ c_crlf }| &&
      |{ c_crlf }|.
  ENDMETHOD.

  METHOD process_command.
    DATA: lv_cmd TYPE string,
          lv_arg TYPE string,
          lv_upper TYPE string.

    CLEAR: ev_output, ev_run_program, ev_program_data, ev_program_name.

    lv_upper = to_upper( iv_command ).
    CONDENSE lv_upper.

    IF lv_upper IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT lv_upper AT space INTO lv_cmd lv_arg.

    IF strlen( lv_cmd ) = 2 AND lv_cmd+1(1) = ':'.
      DATA(lv_drive) = lv_cmd+0(1).
      IF lv_drive >= 'A' AND lv_drive <= 'P'.
        mv_current_drive = lv_drive.
        RETURN.
      ENDIF.
    ENDIF.

    CASE lv_cmd.
      WHEN 'DIR' OR 'LS'.
        IF lv_arg IS INITIAL.
          lv_arg = '*.*'.
        ENDIF.
        ev_output = cmd_dir( lv_arg ).

      WHEN 'TYPE' OR 'CAT'.
        IF lv_arg IS INITIAL.
          ev_output = |Missing filename{ c_crlf }|.
        ELSE.
          ev_output = cmd_type( lv_arg ).
        ENDIF.

      WHEN 'HELP' OR '?'.
        ev_output = cmd_help( ).

      WHEN 'ERA' OR 'DEL' OR 'RM'.
        ev_output = cmd_era( lv_arg ).

      WHEN 'REN'.
        ev_output = |REN not implemented{ c_crlf }|.

      WHEN 'USER'.
        IF lv_arg IS NOT INITIAL.
          mv_current_user = lv_arg.
        ENDIF.
        ev_output = |User = { mv_current_user }{ c_crlf }|.

      WHEN 'CLS' OR 'CLEAR'.
        DATA(lv_esc) = cl_abap_conv_in_ce=>uccpi( 27 ).
        ev_output = lv_esc && '[2J' && lv_esc && '[H'.

      WHEN 'VER' OR 'VERSION'.
        ev_output =
          |CP/M 2.2 Emulator for SAP HANA{ c_crlf }| &&
          |Z80 CPU: ZCL_CPU_Z80{ c_crlf }| &&
          |BDOS: ZCL_CPM_EMULATOR{ c_crlf }| &&
          |CCP: ZCL_CPM_00_CCP{ c_crlf }|.

      WHEN 'RESET'.
        DELETE FROM SHARED MEMORY indx(zk) ID 'CPM_APC_CONNECTIONS'.
        ev_output = |Connection counter reset{ c_crlf }|.

      WHEN 'EXIT' OR 'BYE' OR 'QUIT'.
        ev_output = |Goodbye!{ c_crlf }|.

      WHEN OTHERS.
        DATA(lv_filename) = lv_cmd.
        IF NOT lv_filename CS '.'.
          lv_filename = lv_filename && '.COM'.
        ENDIF.

        DATA(lv_data) = load_smw0_file( lv_filename ).
        IF lv_data IS NOT INITIAL.
          ev_run_program = abap_true.
          ev_program_data = lv_data.
          ev_program_name = lv_filename.
          ev_output = |Loading { lv_filename }...{ c_crlf }|.
        ELSE.
          ev_output = |{ lv_cmd }?{ c_crlf }|.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD cmd_dir.
    DATA: lt_files TYPE tt_file_list,
          lv_count TYPE i,
          lv_total_size TYPE i,
          ls_file TYPE ts_file_info,
          lv_col TYPE i,
          lv_pos TYPE i.

    " Query SMW0 for actual files
    SELECT DISTINCT objid FROM wwwparams
      INTO TABLE @DATA(lt_objids)
      WHERE relid = 'MI'.

    LOOP AT lt_objids INTO DATA(ls_objid).
      DATA(lv_objname) = CONV string( ls_objid-objid ).

      " Split into name and extension
      lv_pos = find( val = lv_objname sub = '.' occ = -1 ).
      IF lv_pos >= 0.
        ls_file-name = to_upper( lv_objname+0(lv_pos) ).
        DATA(lv_ext_pos) = lv_pos + 1.
        ls_file-extension = to_upper( lv_objname+lv_ext_pos ).
      ELSE.
        ls_file-name = to_upper( lv_objname ).
        ls_file-extension = ''.
      ENDIF.

      " Truncate to CP/M 8.3 format
      IF strlen( ls_file-name ) > 8.
        ls_file-name = ls_file-name+0(8).
      ENDIF.
      IF strlen( ls_file-extension ) > 3.
        ls_file-extension = ls_file-extension+0(3).
      ENDIF.

      " Filter: only show COM, DAT, BIN, BAS, TXT files
      CASE ls_file-extension.
        WHEN 'COM' OR 'DAT' OR 'BIN' OR 'BAS' OR 'TXT'.
          ls_file-size = 0.
          APPEND ls_file TO lt_files.
      ENDCASE.
    ENDLOOP.

    SORT lt_files BY name extension.

    IF lt_files IS INITIAL.
      rv_output = |No files found{ c_crlf }|.
      RETURN.
    ENDIF.

    rv_output = |{ c_crlf }Directory of { mv_current_drive }:{ c_crlf }{ c_crlf }|.

    LOOP AT lt_files INTO ls_file.
      " Pad name to 8 chars, extension to 3 chars
      DATA(lv_dname) = |{ ls_file-name WIDTH = 8 }|.
      DATA(lv_dext) = |{ ls_file-extension WIDTH = 3 }|.

      rv_output = rv_output && |{ lv_dname } { lv_dext }  |.

      lv_col = lv_col + 1.
      IF lv_col >= 4.
        rv_output = rv_output && c_crlf.
        lv_col = 0.
      ENDIF.

      lv_count = lv_count + 1.
      lv_total_size = lv_total_size + ls_file-size.
    ENDLOOP.

    IF lv_col > 0.
      rv_output = rv_output && c_crlf.
    ENDIF.

    rv_output = rv_output && |{ c_crlf }{ lv_count } file(s), { format_size( lv_total_size ) }{ c_crlf }|.
  ENDMETHOD.

  METHOD cmd_type.
    DATA(lv_data) = load_smw0_file( iv_filename ).

    IF lv_data IS INITIAL.
      rv_output = |File not found: { iv_filename }{ c_crlf }|.
      RETURN.
    ENDIF.

    DATA: lv_text TYPE string,
          lv_i TYPE i,
          lv_byte TYPE x LENGTH 1,
          lv_char TYPE c LENGTH 1,
          lv_val TYPE i,
          lv_len TYPE i.

    lv_len = xstrlen( lv_data ).
    IF lv_len > 4096.
      lv_len = 4096.
    ENDIF.

    lv_i = 0.
    WHILE lv_i < lv_len.
      lv_byte = lv_data+lv_i(1).
      lv_val = lv_byte.

      IF lv_val = 26.
        EXIT.
      ENDIF.

      IF lv_val >= 32 AND lv_val < 127.
        lv_char = cl_abap_conv_in_ce=>uccpi( lv_val ).
        lv_text = lv_text && lv_char.
      ELSEIF lv_val = 10.
        lv_text = lv_text && c_crlf.
      ELSEIF lv_val = 9.
        lv_text = lv_text && '    '.
      ENDIF.

      lv_i = lv_i + 1.
    ENDWHILE.

    rv_output = lv_text && c_crlf.
  ENDMETHOD.

  METHOD cmd_help.
    rv_output =
      |{ c_crlf }| &&
      |CP/M Commands:{ c_crlf }| &&
      |  DIR [pattern]  - List files (e.g., DIR *.COM){ c_crlf }| &&
      |  TYPE filename  - Display text file{ c_crlf }| &&
      |  filename       - Run .COM program{ c_crlf }| &&
      |  A: B: ...      - Change drive{ c_crlf }| &&
      |  USER n         - Set user number{ c_crlf }| &&
      |  CLS            - Clear screen{ c_crlf }| &&
      |  VER            - Show version{ c_crlf }| &&
      |  RESET          - Reset connection counter{ c_crlf }| &&
      |  HELP           - This help{ c_crlf }| &&
      |  EXIT           - End session{ c_crlf }| &&
      |{ c_crlf }| &&
      |Files are loaded from SMW0 repository.{ c_crlf }| &&
      |{ c_crlf }|.
  ENDMETHOD.

  METHOD cmd_era.
    rv_output = |Read-only file system{ c_crlf }|.
  ENDMETHOD.

  METHOD load_smw0_file.
    DATA: lt_mime TYPE w3mimetabtype,
          ls_key  TYPE wwwdatatab,
          lv_size TYPE i,
          lv_objid TYPE wwwparams-objid.

    SELECT SINGLE objid FROM wwwparams
      INTO lv_objid
      WHERE relid = 'MI'
        AND objid = iv_name.

    IF sy-subrc <> 0.
      DATA lt_all TYPE STANDARD TABLE OF wwwparams-objid.
      SELECT DISTINCT objid FROM wwwparams
        INTO TABLE lt_all
        WHERE relid = 'MI'.

      LOOP AT lt_all INTO DATA(lv_obj).
        IF to_upper( lv_obj ) = to_upper( iv_name ).
          lv_objid = lv_obj.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_objid IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.

    ls_key-relid = 'MI'.
    ls_key-objid = lv_objid.

    DATA lv_size_str TYPE wwwparams-value.
    SELECT SINGLE value FROM wwwparams
      INTO lv_size_str
      WHERE relid = 'MI'
        AND objid = lv_objid
        AND name = 'filesize'.

    IF sy-subrc = 0.
      lv_size = lv_size_str.
    ENDIF.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING key  = ls_key
      TABLES    mime = lt_mime
      EXCEPTIONS OTHERS = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING input_length = lv_size
      IMPORTING buffer       = rv_data
      TABLES    binary_tab   = lt_mime.
  ENDMETHOD.

  METHOD format_size.
    IF iv_size >= 1048576.
      rv_text = |{ iv_size / 1048576 DECIMALS = 1 }MB|.
    ELSEIF iv_size >= 1024.
      rv_text = |{ iv_size / 1024 DECIMALS = 0 }KB|.
    ELSE.
      rv_text = |{ iv_size }B|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
CLASS zcl_cpm_00_apc DEFINITION
  PUBLIC
  INHERITING FROM cl_apc_wsp_ext_stateful_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_apc_wsp_extension~on_accept REDEFINITION.
    METHODS if_apc_wsp_extension~on_start REDEFINITION.
    METHODS if_apc_wsp_extension~on_message REDEFINITION.
    METHODS if_apc_wsp_extension~on_close REDEFINITION.
    METHODS if_apc_wsp_extension~on_error REDEFINITION.

    CLASS-METHODS get_connection_count RETURNING VALUE(rv_count) TYPE i.

  PRIVATE SECTION.
    CONSTANTS c_crlf TYPE string VALUE cl_abap_char_utilities=>cr_lf.
    CONSTANTS c_memory_id TYPE char32 VALUE 'CPM_APC_CONNECTIONS'.

    DATA mo_cpm TYPE REF TO zcl_cpm_emulator.
    DATA mo_ccp TYPE REF TO zcl_cpm_00_ccp.
    DATA mv_session_id TYPE string.
    DATA mv_running_program TYPE abap_bool.

    CLASS-METHODS increment_connections.
    CLASS-METHODS decrement_connections.

    METHODS send_text
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager
                iv_text           TYPE string.

    METHODS run_program_and_output
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager.

    METHODS send_prompt
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager.

    METHODS get_welcome_banner
      RETURNING VALUE(rv_text) TYPE string.

    METHODS load_companion_files
      IMPORTING iv_program_name TYPE string.

    METHODS load_smw0_file
      IMPORTING iv_name TYPE string
      RETURNING VALUE(rv_data) TYPE xstring.
ENDCLASS.

CLASS zcl_cpm_00_apc IMPLEMENTATION.

  METHOD if_apc_wsp_extension~on_accept.
    e_connect_mode = co_connect_mode_accept.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_start.
    increment_connections( ).
    mv_session_id = cl_system_uuid=>create_uuid_c32_static( ).
    DATA(lv_count) = get_connection_count( ).

    mo_ccp = NEW zcl_cpm_00_ccp( ).
    mo_cpm = NEW zcl_cpm_emulator( ).
    mv_running_program = abap_false.

    send_text( i_message_manager = i_message_manager
               iv_text = get_welcome_banner( ) ).
    send_text( i_message_manager = i_message_manager
               iv_text = |Session: { mv_session_id(8) }  Active: { lv_count }{ c_crlf }| ).
    send_text( i_message_manager = i_message_manager
               iv_text = mo_ccp->get_welcome( ) ).
    send_prompt( i_message_manager ).
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_message.
    DATA: lv_output TYPE string,
          lv_run_program TYPE abap_bool,
          lv_program_data TYPE xstring,
          lv_program_name TYPE string.

    DATA(lv_input) = i_message->get_text( ).

    IF mo_ccp IS NOT BOUND.
      send_text( i_message_manager = i_message_manager
                 iv_text = |ERROR: CCP not initialized!{ c_crlf }| ).
      send_prompt( i_message_manager ).
      RETURN.
    ENDIF.

    IF mv_running_program = abap_true AND mo_cpm->is_waiting_input( ) = abap_true.
      mo_cpm->provide_input( lv_input && cl_abap_char_utilities=>cr_lf ).
      run_program_and_output( i_message_manager ).
      RETURN.
    ENDIF.

    mo_ccp->process_command(
      EXPORTING iv_command = lv_input
      IMPORTING ev_output = lv_output
                ev_run_program = lv_run_program
                ev_program_data = lv_program_data
                ev_program_name = lv_program_name ).

    IF lv_output IS NOT INITIAL.
      send_text( i_message_manager = i_message_manager
                 iv_text = lv_output ).
    ENDIF.

    IF lv_run_program = abap_true AND lv_program_data IS NOT INITIAL.
      mv_running_program = abap_true.
      mo_cpm->reset( ).

      " Load companion files (.DAT, etc.) before running
      load_companion_files( lv_program_name ).

      mo_cpm->load_program( iv_data = lv_program_data iv_addr = 256 ).
      run_program_and_output( i_message_manager ).
    ELSE.
      send_prompt( i_message_manager ).
    ENDIF.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_close.
    decrement_connections( ).
    CLEAR: mo_cpm, mo_ccp.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_error.
    decrement_connections( ).
    CLEAR: mo_cpm, mo_ccp.
  ENDMETHOD.

  METHOD send_text.
    DATA(lo_message) = i_message_manager->create_message( ).
    lo_message->set_text( iv_text ).
    i_message_manager->send( lo_message ).
  ENDMETHOD.

  METHOD send_prompt.
    send_text( i_message_manager = i_message_manager
               iv_text = mo_ccp->get_prompt( ) ).
  ENDMETHOD.

  METHOD run_program_and_output.
    mo_cpm->run( iv_max_cycles = 10000000 ).
    DATA(lv_output) = mo_cpm->get_output( ).

    IF lv_output IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_output WITH c_crlf.
      send_text( i_message_manager = i_message_manager
                 iv_text = lv_output ).
    ENDIF.

    IF mo_cpm->is_running( ) = abap_false.
      mv_running_program = abap_false.
      send_text( i_message_manager = i_message_manager
                 iv_text = c_crlf ).
      send_prompt( i_message_manager ).
    ENDIF.
  ENDMETHOD.

  METHOD get_welcome_banner.
    rv_text =
      |{ c_crlf }| &&
      | ╔════════════════════════════════════════════╗{ c_crlf }| &&
      | ║     C P / M   o n   S A P   H A N A        ║{ c_crlf }| &&
      | ╠════════════════════════════════════════════╣{ c_crlf }| &&
      | ║   Z80 CP/M 2.2 Emulator in ABAP            ║{ c_crlf }| &&
      | ║   SMW0 File System                         ║{ c_crlf }| &&
      | ╚════════════════════════════════════════════╝{ c_crlf }| &&
      |{ c_crlf }|.
  ENDMETHOD.

  METHOD load_companion_files.
    " Extract base name from program (e.g., ZORK1.COM -> ZORK1)
    DATA(lv_base) = iv_program_name.
    DATA(lv_pos) = find( val = lv_base sub = '.' ).
    IF lv_pos > 0.
      lv_base = lv_base+0(lv_pos).
    ENDIF.

    " Try to load companion .DAT file
    DATA(lv_dat_name) = lv_base && '.DAT'.
    DATA(lv_dat_data) = load_smw0_file( lv_dat_name ).
    IF lv_dat_data IS NOT INITIAL.
      mo_cpm->register_file( iv_filename = lv_dat_name iv_data = lv_dat_data ).
    ENDIF.

    " Try to load companion .OVR file (overlay)
    DATA(lv_ovr_name) = lv_base && '.OVR'.
    DATA(lv_ovr_data) = load_smw0_file( lv_ovr_name ).
    IF lv_ovr_data IS NOT INITIAL.
      mo_cpm->register_file( iv_filename = lv_ovr_name iv_data = lv_ovr_data ).
    ENDIF.
  ENDMETHOD.

  METHOD load_smw0_file.
    DATA: lt_mime TYPE w3mimetabtype,
          ls_key  TYPE wwwdatatab,
          lv_size TYPE i,
          lv_objid TYPE wwwparams-objid.

    " Try exact match first
    SELECT SINGLE objid FROM wwwparams
      INTO lv_objid
      WHERE relid = 'MI'
        AND objid = iv_name.

    IF sy-subrc <> 0.
      " Try case-insensitive match
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

    " Get file size
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

  METHOD increment_connections.
    DATA lv_count TYPE i.
    IMPORT count = lv_count FROM SHARED MEMORY indx(zk) ID c_memory_id.
    IF sy-subrc <> 0.
      lv_count = 0.
    ENDIF.
    lv_count = lv_count + 1.
    EXPORT count = lv_count TO SHARED MEMORY indx(zk) ID c_memory_id.
  ENDMETHOD.

  METHOD decrement_connections.
    DATA lv_count TYPE i.
    IMPORT count = lv_count FROM SHARED MEMORY indx(zk) ID c_memory_id.
    IF sy-subrc = 0 AND lv_count > 0.
      lv_count = lv_count - 1.
      EXPORT count = lv_count TO SHARED MEMORY indx(zk) ID c_memory_id.
    ENDIF.
  ENDMETHOD.

  METHOD get_connection_count.
    IMPORT count = rv_count FROM SHARED MEMORY indx(zk) ID c_memory_id.
    IF sy-subrc <> 0.
      rv_count = 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
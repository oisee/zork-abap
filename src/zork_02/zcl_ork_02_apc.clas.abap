CLASS zcl_ork_02_apc DEFINITION
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

    " Class method to get active connection count
    CLASS-METHODS get_connection_count RETURNING VALUE(rv_count) TYPE i.

  PRIVATE SECTION.
    CONSTANTS c_crlf TYPE string VALUE cl_abap_char_utilities=>cr_lf.
    CONSTANTS c_game_id TYPE string VALUE 'ZORK-MINI.Z3'.
    CONSTANTS c_memory_id TYPE char32 VALUE 'ZORK_APC_CONNECTIONS'.

    DATA mo_zmachine TYPE REF TO zcl_ork_00_zmachine.
    DATA mv_session_id TYPE string.

    CLASS-METHODS increment_connections.
    CLASS-METHODS decrement_connections.

    METHODS send_text
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager
                iv_text           TYPE string.

    METHODS run_and_output
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager.

    METHODS get_welcome_banner
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_ork_02_apc IMPLEMENTATION.

  METHOD if_apc_wsp_extension~on_accept.
    " Accept the WebSocket connection
    e_connect_mode = co_connect_mode_accept.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_start.
    " Track connection
    increment_connections( ).
    mv_session_id = cl_system_uuid=>create_uuid_c32_static( ).

    " Send welcome banner with session info
    DATA(lv_count) = get_connection_count( ).
    send_text( i_message_manager = i_message_manager
               iv_text = get_welcome_banner( ) ).
    send_text( i_message_manager = i_message_manager
               iv_text = |Session: { mv_session_id(8) }  Active connections: { lv_count }{ c_crlf }{ c_crlf }| ).

    " Load game from SMW0
    DATA(lo_loader) = NEW zcl_ork_00_game_loader_smw0( ).
    DATA(lv_story) = lo_loader->zif_ork_00_game_loader~load( c_game_id ).

    IF lv_story IS INITIAL.
      send_text( i_message_manager = i_message_manager
                 iv_text = |{ c_crlf }ERROR: Game { c_game_id } not found in SMW0!{ c_crlf }| ).
      RETURN.
    ENDIF.

    " Initialize Z-Machine
    mo_zmachine = NEW zcl_ork_00_zmachine( lv_story ).

    " Run until first input prompt
    run_and_output( i_message_manager ).
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_message.
    IF mo_zmachine IS NOT BOUND.
      send_text( i_message_manager = i_message_manager
                 iv_text = |{ c_crlf }ERROR: Z-Machine not initialized!{ c_crlf }| ).
      RETURN.
    ENDIF.

    " Get input from client
    DATA(lv_input) = i_message->get_text( ).

    " Provide input to Z-Machine
    mo_zmachine->provide_input( lv_input ).

    " Run until next input prompt
    run_and_output( i_message_manager ).
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_close.
    decrement_connections( ).
    CLEAR mo_zmachine.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_error.
    decrement_connections( ).
    CLEAR mo_zmachine.
  ENDMETHOD.

  METHOD send_text.
    DATA(lo_message) = i_message_manager->create_message( ).
    lo_message->set_text( iv_text ).
    i_message_manager->send( lo_message ).
  ENDMETHOD.

  METHOD run_and_output.
    " Run Z-Machine until it waits for input or stops
    mo_zmachine->run( ).

    " Get output and status
    DATA(ls_status) = mo_zmachine->get_status( ).

    " Send output to client (convert newlines for terminal)
    IF ls_status-output IS NOT INITIAL.
      DATA(lv_output) = ls_status-output.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_output WITH c_crlf.
      send_text( i_message_manager = i_message_manager
                 iv_text = lv_output ).
    ENDIF.

    " Check if game ended
    IF ls_status-running = abap_false.
      send_text( i_message_manager = i_message_manager
                 iv_text = |{ c_crlf }{ c_crlf }*** GAME OVER ***{ c_crlf }| ).
    ENDIF.
  ENDMETHOD.

  METHOD get_welcome_banner.
    rv_text =
      |{ c_crlf }| &&
      | ╔════════════════════════════════════════════╗{ c_crlf }| &&
      | ║     Z O R K   o n   S A P   H A N A        ║{ c_crlf }| &&
      | ╠════════════════════════════════════════════╣{ c_crlf }| &&
      | ║   Z-Machine V3 Interpreter in ABAP         ║{ c_crlf }| &&
      | ║   Running: { c_game_id WIDTH = 32 }║{ c_crlf }| &&
      | ╚════════════════════════════════════════════╝{ c_crlf }| &&
      |{ c_crlf }|.
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
CLASS zcl_apc_bbs_echo DEFINITION
  PUBLIC
  INHERITING FROM cl_apc_wsp_ext_stateless_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS if_apc_wsp_extension~on_start REDEFINITION.
    METHODS if_apc_wsp_extension~on_message REDEFINITION.
    METHODS if_apc_wsp_extension~on_close REDEFINITION.
    METHODS if_apc_wsp_extension~on_error REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS c_crlf TYPE string VALUE cl_abap_char_utilities=>cr_lf.

    METHODS get_welcome_screen
      RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS zcl_apc_bbs_echo IMPLEMENTATION.

  METHOD if_apc_wsp_extension~on_start.
    " Send welcome banner when connection starts
    DATA(lo_message) = i_message_manager->create_message( ).
    lo_message->set_text( get_welcome_screen( ) ).
    i_message_manager->send( lo_message ).
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_message.
    " Echo received text back
    DATA(lv_received) = i_message->get_text( ).
    DATA(lv_response) = |{ c_crlf }> HOST RECEIVED: { lv_received }{ c_crlf }|.

    DATA(lo_response) = i_message_manager->create_message( ).
    lo_response->set_text( lv_response ).
    i_message_manager->send( lo_response ).
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_close.
    " Cleanup - nothing needed for echo test
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_error.
    " Error handling - could log here if needed
  ENDMETHOD.

  METHOD get_welcome_screen.
    rv_text =
      |{ c_crlf }| &&
      | ╔════════════════════════════════════════════╗{ c_crlf }| &&
      | ║      S A P   H A N A   B B S   v1.0        ║{ c_crlf }| &&
      | ╠════════════════════════════════════════════╣{ c_crlf }| &&
      | ║                                            ║{ c_crlf }| &&
      | ║   [ SYSTEM READY ]                         ║{ c_crlf }| &&
      | ║   [ CONNECTED TO APC SOCKET ]              ║{ c_crlf }| &&
      | ║                                            ║{ c_crlf }| &&
      | ╚════════════════════════════════════════════╝{ c_crlf }| &&
      |{ c_crlf }| &&
      | READY.{ c_crlf }|.
  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Class ZCL_ORK_02_APC - Z-Machine WebSocket Terminal with AI Mode
*&---------------------------------------------------------------------*
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

    CLASS-METHODS get_connection_count RETURNING VALUE(rv_count) TYPE i.
    CLASS-METHODS class_constructor.

  PRIVATE SECTION.
    CONSTANTS c_crlf TYPE string VALUE cl_abap_char_utilities=>cr_lf.
    CONSTANTS c_game_id TYPE string VALUE 'ZORK-MINI.Z3'.
    CONSTANTS c_memory_id TYPE char32 VALUE 'ZORK_APC_SESSIONS'.
    CONSTANTS c_default_env TYPE string VALUE 'DEFAULT.ENV'.
    CONSTANTS c_session_expiry_minutes TYPE i VALUE 30.

    TYPES: BEGIN OF ty_session,
             session_id TYPE char32,
             started_at TYPE timestampl,
           END OF ty_session.
    TYPES ty_sessions TYPE SORTED TABLE OF ty_session WITH UNIQUE KEY session_id.

    CONSTANTS c_mode_play  TYPE string VALUE 'play'.
    CONSTANTS c_mode_watch TYPE string VALUE 'watch'.

    CLASS-DATA gv_esc    TYPE string.
    CLASS-DATA gv_yellow TYPE string.
    CLASS-DATA gv_green  TYPE string.
    CLASS-DATA gv_cyan   TYPE string.
    CLASS-DATA gv_red    TYPE string.
    CLASS-DATA gv_reset  TYPE string.
    CLASS-DATA gv_dim    TYPE string.

    DATA mo_zmachine TYPE REF TO zcl_ork_00_zmachine.
    DATA mv_session_id TYPE string.
    DATA mv_mode TYPE string VALUE 'play'.
    DATA mv_ai_paused TYPE abap_bool.
    DATA mv_ai_goal TYPE string.
    DATA mv_ai_turn TYPE i.
    DATA mv_llm_bin TYPE string.
    DATA mv_env_file TYPE string.
    DATA mv_last_player_cmd TYPE string.

    DATA mo_llm       TYPE REF TO zif_llm_00_llm_lazy.
    DATA mo_game_tool TYPE REF TO zcl_llm_00_agent_t_game.
    DATA mo_signal    TYPE REF TO zcl_llm_00_agent_t_signal.
    DATA mo_registry  TYPE REF TO zcl_llm_00_tool_registry.
    DATA mo_trace     TYPE REF TO zif_llm_00_trace.
    DATA mo_session   TYPE REF TO zif_llm_00_session.

    CLASS-METHODS register_session IMPORTING iv_session_id TYPE char32.
    CLASS-METHODS unregister_session IMPORTING iv_session_id TYPE char32.
    CLASS-METHODS cleanup_expired_sessions CHANGING ct_sessions TYPE ty_sessions.

    METHODS send_text
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager
                iv_text           TYPE string.

    METHODS send_json
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager
                iv_type           TYPE string
                iv_data           TYPE string OPTIONAL.

    METHODS run_and_output
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager
                iv_command        TYPE string OPTIONAL.

    METHODS get_welcome_banner
      RETURNING VALUE(rv_text) TYPE string.

    METHODS init_ai_framework
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager.

    METHODS init_game_tracker.

    METHODS run_ai_turn
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager.

    METHODS run_ai_steps
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager
                iv_steps          TYPE i.

    METHODS build_ai_prompt
      RETURNING VALUE(rv_) TYPE string.

    METHODS save_session.

    METHODS handle_export
      IMPORTING i_message_manager TYPE REF TO if_apc_wsp_message_manager
                iv_format         TYPE string.

ENDCLASS.

CLASS zcl_ork_02_apc IMPLEMENTATION.

  METHOD class_constructor.
    DATA lv_xstr TYPE xstring.
    lv_xstr = '1B'.
    gv_esc = cl_abap_conv_codepage=>create_in( )->convert( source = lv_xstr ).
    gv_yellow = gv_esc && '[33m'.
    gv_green  = gv_esc && '[32m'.
    gv_cyan   = gv_esc && '[36m'.
    gv_red    = gv_esc && '[31m'.
    gv_reset  = gv_esc && '[0m'.
    gv_dim    = gv_esc && '[2m'.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_accept.
    e_connect_mode = co_connect_mode_accept.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_start.
    mv_session_id = cl_system_uuid=>create_uuid_c32_static( ).
    register_session( CONV #( mv_session_id ) ).
    mv_mode = c_mode_play.
    mv_ai_paused = abap_false.
    mv_ai_turn = 0.
    mv_llm_bin = |$ZRAY_{ sy-uname }|.
    mv_env_file = c_default_env.

    DATA(lv_count) = get_connection_count( ).
    send_text( i_message_manager = i_message_manager iv_text = get_welcome_banner( ) ).
    send_text( i_message_manager = i_message_manager
               iv_text = |Session: { mv_session_id(8) }  User: { sy-uname }  Connections: { lv_count }{ c_crlf }| ).

    DATA(lo_loader) = NEW zcl_ork_00_game_loader_smw0( ).
    DATA(lv_story) = lo_loader->zif_ork_00_game_loader~load( c_game_id ).

    IF lv_story IS INITIAL.
      send_text( i_message_manager = i_message_manager
                 iv_text = |{ c_crlf }ERROR: Game { c_game_id } not found in SMW0!{ c_crlf }| ).
      RETURN.
    ENDIF.

    mo_zmachine = NEW zcl_ork_00_zmachine( lv_story ).
    init_game_tracker( ).
    run_and_output( i_message_manager ).
    send_json( i_message_manager = i_message_manager iv_type = 'mode' iv_data = mv_mode ).
  ENDMETHOD.

  METHOD init_game_tracker.
    mo_game_tool = NEW zcl_llm_00_agent_t_game( ).
    mo_game_tool->set_zmachine( mo_zmachine ).
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_message.
    IF mo_zmachine IS NOT BOUND.
      send_text( i_message_manager = i_message_manager
                 iv_text = |{ c_crlf }ERROR: Z-Machine not initialized!{ c_crlf }| ).
      RETURN.
    ENDIF.

    DATA(lv_input) = i_message->get_text( ).

    IF lv_input CS '{'.
      DATA lv_cmd TYPE string.
      DATA lv_val TYPE string.

      FIND REGEX '"cmd"\s*:\s*"([^"]+)"' IN lv_input SUBMATCHES lv_cmd.
      FIND REGEX '"value"\s*:\s*"([^"]*)"' IN lv_input SUBMATCHES lv_val.

      CASE lv_cmd.
        WHEN 'mode'.
          IF mv_mode = c_mode_watch AND lv_val = c_mode_play.
            save_session( ).
          ENDIF.
          mv_mode = lv_val.
          send_json( i_message_manager = i_message_manager iv_type = 'mode' iv_data = mv_mode ).
          IF mv_mode = c_mode_watch.
            send_text( i_message_manager = i_message_manager
                       iv_text = |{ c_crlf }{ c_crlf }{ gv_yellow }>>> AI SPECTATOR MODE <<<{ gv_reset }{ c_crlf }| ).
            send_text( i_message_manager = i_message_manager
                       iv_text = |{ gv_yellow }Watching AI play... Use controls to step.{ gv_reset }{ c_crlf }{ c_crlf }| ).
            mv_ai_paused = abap_true.
            init_ai_framework( i_message_manager ).
          ELSE.
            send_text( i_message_manager = i_message_manager
                       iv_text = |{ c_crlf }{ gv_green }>>> PLAY MODE <<<{ gv_reset }{ c_crlf }| ).
          ENDIF.

        WHEN 'env'.
          mv_env_file = COND #( WHEN lv_val IS NOT INITIAL THEN lv_val ELSE c_default_env ).
          CLEAR: mo_llm, mo_registry.

        WHEN 'step'.
          IF mv_mode = c_mode_watch.
            DATA(lv_steps) = 1.
            IF lv_val IS NOT INITIAL.
              lv_steps = CONV i( lv_val ).
              IF lv_steps < 1. lv_steps = 1. ENDIF.
              IF lv_steps > 50. lv_steps = 50. ENDIF.
            ENDIF.
            run_ai_steps( i_message_manager = i_message_manager iv_steps = lv_steps ).
          ENDIF.

        WHEN 'pause'.
          mv_ai_paused = abap_true.
          send_json( i_message_manager = i_message_manager iv_type = 'paused' iv_data = 'true' ).

        WHEN 'resume'.
          mv_ai_paused = abap_false.
          send_json( i_message_manager = i_message_manager iv_type = 'paused' iv_data = 'false' ).

        WHEN 'export'.
          handle_export( i_message_manager = i_message_manager iv_format = lv_val ).

        WHEN 'map'.
          IF mo_game_tool IS BOUND.
            DATA(lv_map) = mo_game_tool->get_map_text( ).
            REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_map WITH c_crlf.
            send_text( i_message_manager = i_message_manager
                       iv_text = |{ c_crlf }{ gv_cyan }{ lv_map }{ gv_reset }{ c_crlf }| ).
          ENDIF.

        WHEN OTHERS.
      ENDCASE.
    ELSE.
      IF mv_mode = c_mode_play.
        mv_last_player_cmd = lv_input.
        mo_zmachine->provide_input( lv_input ).
        run_and_output( i_message_manager = i_message_manager iv_command = lv_input ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_close.
    save_session( ).
    unregister_session( CONV #( mv_session_id ) ).
    CLEAR: mo_zmachine, mo_llm, mo_registry, mo_game_tool, mo_signal, mo_session.
  ENDMETHOD.

  METHOD if_apc_wsp_extension~on_error.
    save_session( ).
    unregister_session( CONV #( mv_session_id ) ).
    CLEAR: mo_zmachine, mo_llm, mo_registry, mo_game_tool, mo_signal, mo_session.
  ENDMETHOD.

  METHOD send_text.
    DATA(lo_message) = i_message_manager->create_message( ).
    lo_message->set_text( iv_text ).
    i_message_manager->send( lo_message ).
  ENDMETHOD.

  METHOD send_json.
    DATA(lv_json) = |\{"type":"{ iv_type }","data":"{ iv_data }"\}|.
    DATA(lo_message) = i_message_manager->create_message( ).
    lo_message->set_text( lv_json ).
    i_message_manager->send( lo_message ).
  ENDMETHOD.

  METHOD run_and_output.
    mo_zmachine->run( ).
    DATA(ls_status) = mo_zmachine->get_status( ).
    IF ls_status-output IS NOT INITIAL.
      DATA(lv_output) = ls_status-output.
      IF iv_command IS NOT INITIAL AND mo_game_tool IS BOUND.
        mo_game_tool->log_move( iv_command = iv_command iv_response = lv_output ).
      ENDIF.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_output WITH c_crlf.
      send_text( i_message_manager = i_message_manager iv_text = lv_output ).
    ENDIF.
    IF ls_status-running = abap_false.
      send_text( i_message_manager = i_message_manager
                 iv_text = |{ c_crlf }{ c_crlf }*** GAME OVER ***{ c_crlf }| ).
      send_json( i_message_manager = i_message_manager iv_type = 'gameover' iv_data = 'true' ).
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

  METHOD init_ai_framework.
    IF mo_llm IS BOUND AND mo_registry IS BOUND.
      RETURN.
    ENDIF.

    send_text( i_message_manager = i_message_manager
               iv_text = |{ gv_cyan }Loading LLM: { mv_llm_bin }/{ mv_env_file }{ gv_reset }{ c_crlf }| ).

    TRY.
        DATA(lo_file) = zcl_llm_00_file_bin=>new( iv_bin = mv_llm_bin iv_name = mv_env_file ).
        mo_llm = zcl_llm_00_llm_lazy=>new_from_file( lo_file ).
        mo_trace = zcl_llm_00_trace=>new( iv_level = 1 iv_console = abap_false ).

        IF mo_game_tool IS BOUND.
          mo_game_tool->attach_zmachine( mo_zmachine ).
        ENDIF.

        mo_signal = NEW zcl_llm_00_agent_t_signal( ).
        mo_registry = zcl_llm_00_tool_registry=>new( ).
        mo_registry->register( mo_game_tool ).
        mo_registry->register( mo_signal ).

        IF mv_ai_goal IS INITIAL.
          mv_ai_goal = 'Explore the game world and try to complete the adventure'.
        ENDIF.

        mo_session = zcl_llm_00_session=>new(
          iv_question = |AI playing { c_game_id }: { mv_ai_goal }|
        ).
        mo_session->start_hyper_run( ).

        DATA(ls_cfg) = mo_llm->get_config( ).
        mo_game_tool->set_model_info( iv_model_name = ls_cfg-model_name iv_env_file = mv_env_file ).
        send_text( i_message_manager = i_message_manager
                   iv_text = |{ gv_green }LLM ready: { ls_cfg-model_name }{ gv_reset }{ c_crlf }| ).
        send_text( i_message_manager = i_message_manager
                   iv_text = |{ gv_cyan }Session: { mo_session->get_id( ) }{ gv_reset }{ c_crlf }| ).
        send_text( i_message_manager = i_message_manager
                   iv_text = |{ gv_cyan }Click [Step] to start AI playing{ gv_reset }{ c_crlf }| ).

      CATCH cx_root INTO DATA(lx_err).
        send_text( i_message_manager = i_message_manager
                   iv_text = |{ gv_red }LLM Error: { lx_err->get_text( ) }{ gv_reset }{ c_crlf }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD run_ai_steps.
    DO iv_steps TIMES.
      IF mo_game_tool->is_running( ) = abap_false OR mo_signal->is_exit_signaled( ) = abap_true.
        EXIT.
      ENDIF.
      IF mv_ai_paused = abap_true AND sy-index > 1.
        EXIT.
      ENDIF.
      run_ai_turn( i_message_manager ).
    ENDDO.

    IF mo_game_tool->is_running( ) = abap_false OR mo_signal->is_exit_signaled( ) = abap_true.
      save_session( ).
    ENDIF.
  ENDMETHOD.

  METHOD run_ai_turn.
    DATA lv_start TYPE timestampl.
    DATA lv_end TYPE timestampl.

    IF mo_llm IS NOT BOUND OR mo_registry IS NOT BOUND.
      send_text( i_message_manager = i_message_manager
                 iv_text = |{ c_crlf }{ gv_red }AI not initialized!{ gv_reset }{ c_crlf }| ).
      RETURN.
    ENDIF.

    IF mo_game_tool->is_running( ) = abap_false.
      send_text( i_message_manager = i_message_manager
                 iv_text = |{ c_crlf }{ gv_yellow }[AI] Game has ended.{ gv_reset }{ c_crlf }| ).
      RETURN.
    ENDIF.

    mv_ai_turn = mv_ai_turn + 1.
    GET TIME STAMP FIELD lv_start.

    DATA(lv_call_id) = |turn_{ mv_ai_turn }|.
    IF mo_session IS BOUND.
      mo_session->log_tool_call( iv_tool_name = 'game' iv_arguments = |turn { mv_ai_turn }| iv_call_id = lv_call_id ).
    ENDIF.

    TRY.
        DATA(lv_prompt) = build_ai_prompt( ).
        DATA(lo_executor) = zcl_llm_00_executor=>new(
          io_llm = mo_llm io_registry = mo_registry io_trace = mo_trace ).
        DATA(ls_result) = lo_executor->run( iv_prompt = lv_prompt iv_max_iterations = 1 ).

        GET TIME STAMP FIELD lv_end.
        DATA(lv_duration) = CONV decfloat16( lv_end - lv_start ).
        DATA(lv_duration_s) = COND string(
          WHEN lv_duration < 10 THEN |{ lv_duration DECIMALS = 1 }|
          ELSE |{ CONV i( lv_duration ) }| ).

        DATA(lt_transcript) = mo_game_tool->get_transcript( ).
        DATA(lv_last_cmd) = VALUE #( lt_transcript[ lines( lt_transcript ) ]-command OPTIONAL ).
        DATA(lv_game_output) = mo_game_tool->get_output( ).

        IF mo_session IS BOUND.
          mo_session->log_tool_result(
            iv_call_id = lv_call_id iv_success = abap_true
            iv_result = |{ lv_last_cmd }: { lv_game_output }|
            iv_duration = CONV i( lv_duration * 1000 ) ).
          mo_session->inc_iteration( ).
        ENDIF.

        IF lv_last_cmd IS NOT INITIAL.
          send_text( i_message_manager = i_message_manager
            iv_text = |{ c_crlf }{ gv_dim }AI> { gv_reset }{ gv_green }{ lv_last_cmd }{ gv_reset } { gv_dim }({ lv_duration_s }s){ gv_reset }{ c_crlf }| ).
        ENDIF.

        IF lv_game_output IS NOT INITIAL.
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_game_output WITH c_crlf.
          send_text( i_message_manager = i_message_manager iv_text = |{ lv_game_output }{ c_crlf }| ).
        ENDIF.

        DATA(lt_signals) = mo_signal->get_signals( ).
        LOOP AT lt_signals INTO DATA(ls_sig).
          send_text( i_message_manager = i_message_manager
                     iv_text = |{ gv_yellow }[{ ls_sig-signal }] { ls_sig-message }{ gv_reset }{ c_crlf }| ).
        ENDLOOP.

        send_json( i_message_manager = i_message_manager
                   iv_type = 'turn' iv_data = |{ mv_ai_turn },{ lv_duration_s },{ lv_last_cmd }| ).

        IF mo_game_tool->is_running( ) = abap_false.
          send_text( i_message_manager = i_message_manager
                     iv_text = |{ c_crlf }{ gv_yellow }*** GAME OVER ***{ gv_reset }{ c_crlf }| ).
          send_json( i_message_manager = i_message_manager iv_type = 'gameover' iv_data = 'true' ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_err).
        DATA(lv_err_text) = lx_err->get_text( ).

        IF mo_session IS BOUND.
          mo_session->log_tool_result(
            iv_call_id = lv_call_id iv_success = abap_false iv_result = lv_err_text iv_duration = 0 ).
        ENDIF.

        " Check for API errors (rate limit, ZCX_S from APC context, etc.) - tell client to retry
        IF lv_err_text CS 'RATE_LIMIT' OR lv_err_text CS '429' OR lv_err_text CS 'Too Many'
           OR lv_err_text CS 'ZCX_S'.
          send_text( i_message_manager = i_message_manager
                     iv_text = |{ gv_yellow }[API ERROR] LLM temporarily unavailable - click Step to retry{ gv_reset }{ c_crlf }| ).
          send_json( i_message_manager = i_message_manager iv_type = 'ratelimit' iv_data = 'true' ).
          mv_ai_turn = mv_ai_turn - 1.
          RETURN.
        ENDIF.

        IF lv_err_text CS 'token' OR lv_err_text CS 'length' OR lv_err_text CS 'context'.
          send_text( i_message_manager = i_message_manager
                     iv_text = |{ gv_yellow }[Context too large - compressing...]{ gv_reset }{ c_crlf }| ).
          mo_game_tool->compress_history( iv_keep_recent = 5 ).
        ENDIF.

        send_text( i_message_manager = i_message_manager
                   iv_text = |{ c_crlf }{ gv_red }AI Error: { lv_err_text }{ gv_reset }{ c_crlf }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD build_ai_prompt.
    DATA(lv_nl) = cl_abap_char_utilities=>newline.
    DATA(lv_move_count) = mo_game_tool->get_move_count( ).
    DATA(lv_context) = mo_game_tool->get_context_for_prompt( iv_recent_moves = 10 iv_max_chars = 12000 ).

    IF mv_ai_turn = 1.
      rv_ =
        |You are an AI playing a text adventure game.| && lv_nl && lv_nl &&
        |Goal: { mv_ai_goal }| && lv_nl && lv_nl &&
        |Tools:| && lv_nl &&
        |- game(command): Send command to game| && lv_nl &&
        |- signal(signal, message): DONE/STUCK/BORED/NOTE| && lv_nl && lv_nl &&
        |Commands: look, north/south/east/west, take, drop, examine, inventory, open, read| && lv_nl && lv_nl &&
        |IMPORTANT: Remember what you've done! Don't repeat commands or go in circles.| && lv_nl &&
        |Send ONE command per turn using the game tool.| && lv_nl && lv_nl &&
        lv_context.
    ELSE.
      rv_ =
        |Continue playing the text adventure. Moves so far: { lv_move_count }| && lv_nl && lv_nl &&
        |Goal: { mv_ai_goal }| && lv_nl && lv_nl &&
        |IMPORTANT: Review your history below. Don't repeat actions or revisit explored areas.| && lv_nl && lv_nl &&
        lv_context && lv_nl &&
        |Based on your history above, what's your next move? Use the game tool.|.
    ENDIF.
  ENDMETHOD.

  METHOD handle_export.
    IF mo_game_tool IS NOT BOUND.
      send_text( i_message_manager = i_message_manager
                 iv_text = |{ gv_red }No game data to export.{ gv_reset }{ c_crlf }| ).
      RETURN.
    ENDIF.

    DATA lv_content TYPE string.
    DATA lv_filename TYPE string.

    CASE iv_format.
      WHEN 'json'.
        lv_content = mo_game_tool->export_json( ).
        lv_filename = |zork_session_{ mv_session_id(8) }.json|.
      WHEN 'map'.
        lv_content = mo_game_tool->get_map_text( ).
        lv_filename = |zork_map_{ mv_session_id(8) }.txt|.
      WHEN OTHERS.
        lv_content = mo_game_tool->export_markdown( ).
        lv_filename = |zork_session_{ mv_session_id(8) }.md|.
    ENDCASE.

    DATA(lv_export_json) = |\{"type":"export","filename":"{ lv_filename }","content":"|.
    REPLACE ALL OCCURRENCES OF '\' IN lv_content WITH '\\'.
    REPLACE ALL OCCURRENCES OF '"' IN lv_content WITH '\"'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_content WITH '\n'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_content WITH '\n'.
    lv_export_json = lv_export_json && lv_content && '"}'.

    DATA(lo_message) = i_message_manager->create_message( ).
    lo_message->set_text( lv_export_json ).
    i_message_manager->send( lo_message ).

    send_text( i_message_manager = i_message_manager
               iv_text = |{ gv_green }Export ready: { lv_filename }{ gv_reset }{ c_crlf }| ).
  ENDMETHOD.

  METHOD save_session.
    CHECK mo_session IS BOUND AND mv_ai_turn > 0.
    TRY.
        DATA(lt_transcript) = mo_game_tool->get_transcript( ).
        DATA(lv_summary) = |AI played { mv_ai_turn } turns. Commands: |.
        LOOP AT lt_transcript INTO DATA(ls_move).
          IF sy-tabix > 1. lv_summary = lv_summary && ', '. ENDIF.
          lv_summary = lv_summary && ls_move-command.
          IF sy-tabix > 20. lv_summary = lv_summary && '...'. EXIT. ENDIF.
        ENDLOOP.
        mo_session->set_answer( lv_summary ).
        zcl_llm_00_session_store=>get_instance( )->save( mo_session ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD register_session.
    DATA lt_sessions TYPE ty_sessions.
    DATA ls_session TYPE ty_session.
    IMPORT sessions = lt_sessions FROM SHARED MEMORY indx(zk) ID c_memory_id.
    cleanup_expired_sessions( CHANGING ct_sessions = lt_sessions ).
    ls_session-session_id = iv_session_id.
    GET TIME STAMP FIELD ls_session-started_at.
    INSERT ls_session INTO TABLE lt_sessions.
    EXPORT sessions = lt_sessions TO SHARED MEMORY indx(zk) ID c_memory_id.
  ENDMETHOD.

  METHOD unregister_session.
    DATA lt_sessions TYPE ty_sessions.
    IMPORT sessions = lt_sessions FROM SHARED MEMORY indx(zk) ID c_memory_id.
    CHECK sy-subrc = 0.
    DELETE lt_sessions WHERE session_id = iv_session_id.
    EXPORT sessions = lt_sessions TO SHARED MEMORY indx(zk) ID c_memory_id.
  ENDMETHOD.

  METHOD cleanup_expired_sessions.
    DATA lv_now TYPE timestampl.
    DATA lv_expiry_seconds TYPE decfloat16.
    DATA lt_expired TYPE STANDARD TABLE OF char32.

    GET TIME STAMP FIELD lv_now.
    lv_expiry_seconds = c_session_expiry_minutes * 60.

    LOOP AT ct_sessions INTO DATA(ls_session).
      IF ( lv_now - ls_session-started_at ) > lv_expiry_seconds.
        APPEND ls_session-session_id TO lt_expired.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_expired INTO DATA(lv_session_id).
      DELETE ct_sessions WHERE session_id = lv_session_id.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_connection_count.
    DATA lt_sessions TYPE ty_sessions.
    IMPORT sessions = lt_sessions FROM SHARED MEMORY indx(zk) ID c_memory_id.
    IF sy-subrc <> 0.
      rv_count = 0.
      RETURN.
    ENDIF.
    cleanup_expired_sessions( CHANGING ct_sessions = lt_sessions ).
    EXPORT sessions = lt_sessions TO SHARED MEMORY indx(zk) ID c_memory_id.
    rv_count = lines( lt_sessions ).
  ENDMETHOD.

ENDCLASS.

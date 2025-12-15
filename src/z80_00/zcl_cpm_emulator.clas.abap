CLASS zcl_cpm_emulator DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    " BDOS function numbers
    CONSTANTS: c_bdos_system_reset   TYPE i VALUE 0,
               c_bdos_console_input  TYPE i VALUE 1,
               c_bdos_console_output TYPE i VALUE 2,
               c_bdos_direct_io      TYPE i VALUE 6,
               c_bdos_print_string   TYPE i VALUE 9,
               c_bdos_read_buffer    TYPE i VALUE 10,
               c_bdos_console_status TYPE i VALUE 11,
               c_bdos_version        TYPE i VALUE 12,
               c_bdos_reset_disk     TYPE i VALUE 13,
               c_bdos_select_disk    TYPE i VALUE 14,
               c_bdos_open_file      TYPE i VALUE 15,
               c_bdos_close_file     TYPE i VALUE 16,
               c_bdos_read_seq       TYPE i VALUE 20,
               c_bdos_get_disk       TYPE i VALUE 25,
               c_bdos_set_dma        TYPE i VALUE 26,
               c_bdos_get_user       TYPE i VALUE 32,
               c_bdos_read_rand      TYPE i VALUE 33,
               c_bdos_file_size      TYPE i VALUE 35,
               c_bdos_set_rand       TYPE i VALUE 36.

    " CP/M memory addresses
    CONSTANTS: c_tpa_start   TYPE i VALUE 256,    " 0x0100
               c_bdos_impl   TYPE i VALUE 65024,  " 0xFE00
               c_bios_boot   TYPE i VALUE 65280.  " 0xFF00

    METHODS constructor.
    METHODS reset.
    METHODS load_program IMPORTING iv_data TYPE xstring
                                   iv_addr TYPE i DEFAULT 256.
    METHODS provide_input IMPORTING iv_text TYPE string.
    METHODS setup_fcb IMPORTING iv_fcb_addr TYPE i
                                iv_filename TYPE string.
    METHODS register_file IMPORTING iv_filename TYPE string
                                    iv_data     TYPE xstring.
    TYPES: BEGIN OF ts_trace_entry,
             pc     TYPE i,
             opcode TYPE i,
             sp     TYPE i,
             a      TYPE i,
             bc     TYPE i,
             de     TYPE i,
             hl     TYPE i,
             info   TYPE string,
           END OF ts_trace_entry,
           tt_trace TYPE STANDARD TABLE OF ts_trace_entry WITH EMPTY KEY.

    METHODS run IMPORTING iv_max_cycles TYPE i DEFAULT 10000000
                          iv_trace      TYPE abap_bool DEFAULT abap_false
                EXPORTING et_trace      TYPE tt_trace
                RETURNING VALUE(rv_output) TYPE string.
    METHODS step RETURNING VALUE(rv_continue) TYPE abap_bool.
    METHODS get_output RETURNING VALUE(rv_output) TYPE string.
    METHODS is_running RETURNING VALUE(rv_running) TYPE abap_bool.
    METHODS is_waiting_input RETURNING VALUE(rv_waiting) TYPE abap_bool.
    METHODS clear_waiting_input.
    METHODS set_debug IMPORTING iv_debug TYPE abap_bool.

  PRIVATE SECTION.
    DATA mo_cpu TYPE REF TO zcl_cpu_z80.
    DATA mo_core TYPE REF TO zif_cpu_z80_core.  " Interface for register access
    DATA mo_bus TYPE REF TO zif_cpu_z80_bus.
    DATA mv_running TYPE abap_bool.
    DATA mv_debug TYPE abap_bool.
    DATA mv_waiting_input TYPE abap_bool.  " True when waiting for console input

    " File system support
    DATA mv_dma_addr TYPE i VALUE 128.  " Default DMA at 0x0080
    DATA mv_current_disk TYPE i VALUE 0.
    DATA mv_user_number TYPE i VALUE 0.

    " File data storage
    TYPES: BEGIN OF ts_file_entry,
             fcb_addr TYPE i,
             filename TYPE string,
             data     TYPE xstring,
             position TYPE i,
           END OF ts_file_entry.
    DATA mt_open_files TYPE STANDARD TABLE OF ts_file_entry WITH KEY fcb_addr.
    DATA mt_file_data TYPE STANDARD TABLE OF ts_file_entry WITH KEY filename.

    METHODS setup_cpm_memory.
    METHODS bdos_call IMPORTING iv_func TYPE i
                      RETURNING VALUE(rv_continue) TYPE abap_bool.
    METHODS console_output IMPORTING iv_char TYPE i.
    METHODS read_line IMPORTING iv_max_len TYPE i
                      RETURNING VALUE(rv_line) TYPE string.

    " File system methods
    METHODS fcb_to_filename IMPORTING iv_fcb_addr TYPE i
                            RETURNING VALUE(rv_filename) TYPE string.
    METHODS file_open IMPORTING iv_fcb_addr TYPE i
                      RETURNING VALUE(rv_result) TYPE i.
    METHODS file_close IMPORTING iv_fcb_addr TYPE i
                       RETURNING VALUE(rv_result) TYPE i.
    METHODS file_read_seq IMPORTING iv_fcb_addr TYPE i
                          RETURNING VALUE(rv_result) TYPE i.
    METHODS file_read_rand IMPORTING iv_fcb_addr TYPE i
                           RETURNING VALUE(rv_result) TYPE i.
    METHODS file_size IMPORTING iv_fcb_addr TYPE i
                      RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.

CLASS zcl_cpm_emulator IMPLEMENTATION.

  METHOD constructor.
    DATA lo_bus TYPE REF TO zcl_cpu_z80_bus_simple.
    CREATE OBJECT lo_bus.
    mo_bus = lo_bus.
    CREATE OBJECT mo_cpu EXPORTING io_bus = mo_bus.
    mo_core = mo_cpu.
    mv_running = abap_false.
    mv_debug = abap_false.
    mv_waiting_input = abap_false.
  ENDMETHOD.

  METHOD reset.
    mo_bus->clear_output( ).
    setup_cpm_memory( ).
    mo_cpu->reset( ).
    mo_core->set_pc( c_tpa_start ).
    mo_core->set_sp( 65535 ).
    mo_core->push( 0 ).
    mv_running = abap_true.
    mv_waiting_input = abap_false.
    mv_dma_addr = 128.
    CLEAR mt_open_files.
  ENDMETHOD.

  METHOD setup_cpm_memory.
    " Warm boot at 0000h - JP to BIOS
    mo_bus->write_mem( iv_addr = 0 iv_val = 195 ).
    mo_bus->write_mem( iv_addr = 1 iv_val = 0 ).
    mo_bus->write_mem( iv_addr = 2 iv_val = 255 ).

    " I/O byte at 0003h
    mo_bus->write_mem( iv_addr = 3 iv_val = 0 ).

    " Current disk at 0004h
    mo_bus->write_mem( iv_addr = 4 iv_val = 0 ).

    " BDOS entry at 0005h - JP to BDOS handler
    mo_bus->write_mem( iv_addr = 5 iv_val = 195 ).
    mo_bus->write_mem( iv_addr = 6 iv_val = 0 ).
    mo_bus->write_mem( iv_addr = 7 iv_val = 254 ).

    " BDOS stub at FE00h - RET
    mo_bus->write_mem( iv_addr = c_bdos_impl iv_val = 201 ).

    " BIOS warm boot stub at FF00h - RET
    mo_bus->write_mem( iv_addr = c_bios_boot iv_val = 201 ).
  ENDMETHOD.

  METHOD load_program.
    mo_bus->load( iv_addr = iv_addr iv_data = iv_data ).
  ENDMETHOD.

  METHOD provide_input.
    " Clear waiting flag since we're providing input
    mv_waiting_input = abap_false.
    mo_bus->provide_input( iv_text ).
  ENDMETHOD.

  METHOD setup_fcb.
    " Set up FCB with filename
    DATA lv_i TYPE i.
    DATA lv_drive TYPE i VALUE 0.
    DATA lv_name TYPE string.
    DATA lv_ext TYPE string.
    DATA lv_ch TYPE i.
    DATA lv_filename TYPE string.
    DATA lv_pos TYPE i.

    " Clear FCB (36 bytes)
    DO 36 TIMES.
      lv_i = sy-index - 1.
      mo_bus->write_mem( iv_addr = iv_fcb_addr + lv_i iv_val = 0 ).
    ENDDO.

    lv_filename = to_upper( iv_filename ).

    " Check for drive letter
    IF strlen( lv_filename ) >= 2.
      DATA lv_second TYPE c LENGTH 1.
      lv_second = lv_filename+1(1).
      IF lv_second = ':'.
        DATA lv_first TYPE c LENGTH 1.
        lv_first = lv_filename+0(1).
        lv_drive = cl_abap_conv_out_ce=>uccpi( lv_first ) - 64.  " A=1, B=2, etc
        lv_filename = lv_filename+2.
      ENDIF.
    ENDIF.

    " Split name.ext
    lv_pos = find( val = lv_filename sub = '.' ).
    IF lv_pos >= 0.
      lv_name = lv_filename+0(lv_pos).
      lv_pos = lv_pos + 1.
      lv_ext = lv_filename+lv_pos.
    ELSE.
      lv_name = lv_filename.
      lv_ext = ''.
    ENDIF.

    " Truncate
    IF strlen( lv_name ) > 8.
      lv_name = lv_name+0(8).
    ENDIF.
    IF strlen( lv_ext ) > 3.
      lv_ext = lv_ext+0(3).
    ENDIF.

    " Write drive
    mo_bus->write_mem( iv_addr = iv_fcb_addr iv_val = lv_drive ).

    " Write filename (padded with spaces)
    DO 8 TIMES.
      lv_i = sy-index - 1.
      IF lv_i < strlen( lv_name ).
        DATA lv_char TYPE c LENGTH 1.
        lv_char = lv_name+lv_i(1).
        lv_ch = cl_abap_conv_out_ce=>uccpi( lv_char ).
      ELSE.
        lv_ch = 32.  " Space
      ENDIF.
      mo_bus->write_mem( iv_addr = iv_fcb_addr + 1 + lv_i iv_val = lv_ch ).
    ENDDO.

    " Write extension (padded with spaces)
    DO 3 TIMES.
      lv_i = sy-index - 1.
      IF lv_i < strlen( lv_ext ).
        lv_char = lv_ext+lv_i(1).
        lv_ch = cl_abap_conv_out_ce=>uccpi( lv_char ).
      ELSE.
        lv_ch = 32.  " Space
      ENDIF.
      mo_bus->write_mem( iv_addr = iv_fcb_addr + 9 + lv_i iv_val = lv_ch ).
    ENDDO.
  ENDMETHOD.

  METHOD register_file.
    " Register file data for CP/M file operations
    DATA ls_file TYPE ts_file_entry.
    ls_file-filename = to_upper( iv_filename ).
    ls_file-data = iv_data.
    ls_file-position = 0.
    APPEND ls_file TO mt_file_data.
  ENDMETHOD.

  METHOD fcb_to_filename.
    " Extract filename from FCB structure
    DATA lv_i TYPE i.
    DATA lv_ch TYPE i.
    DATA lv_name TYPE string.
    DATA lv_ext TYPE string.
    DATA lv_char TYPE c LENGTH 1.

    " Read name (bytes 1-8)
    lv_name = ''.
    DO 8 TIMES.
      lv_i = sy-index.
      lv_ch = mo_bus->read_mem( iv_fcb_addr + lv_i ).
      lv_ch = lv_ch MOD 128.  " Strip high bit
      IF lv_ch = 32.  " Space
        EXIT.
      ENDIF.
      " Convert code point to character
      lv_char = cl_abap_conv_in_ce=>uccpi( lv_ch ).
      lv_name = lv_name && lv_char.
    ENDDO.

    " Read extension (bytes 9-11)
    lv_ext = ''.
    DO 3 TIMES.
      lv_i = sy-index + 8.
      lv_ch = mo_bus->read_mem( iv_fcb_addr + lv_i ).
      lv_ch = lv_ch MOD 128.  " Strip high bit
      IF lv_ch = 32.  " Space
        EXIT.
      ENDIF.
      lv_char = cl_abap_conv_in_ce=>uccpi( lv_ch ).
      lv_ext = lv_ext && lv_char.
    ENDDO.

    IF lv_ext IS NOT INITIAL.
      rv_filename = lv_name && '.' && lv_ext.
    ELSE.
      rv_filename = lv_name.
    ENDIF.
  ENDMETHOD.

  METHOD file_open.
    " BDOS function 15 - Open file
    DATA lv_filename TYPE string.
    DATA ls_file TYPE ts_file_entry.
    DATA ls_open TYPE ts_file_entry.
    DATA lv_found TYPE abap_bool VALUE abap_false.
    DATA lv_size TYPE i.
    DATA lv_records TYPE i.
    DATA lv_extent_records TYPE i.

    lv_filename = fcb_to_filename( iv_fcb_addr ).

    " Look for file in registered file data
    LOOP AT mt_file_data INTO ls_file WHERE filename = to_upper( lv_filename ).
      lv_found = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_found = abap_false.
      rv_result = 255.  " File not found
      RETURN.
    ENDIF.

    " Create open file entry
    ls_open-fcb_addr = iv_fcb_addr.
    ls_open-filename = ls_file-filename.
    ls_open-data = ls_file-data.
    ls_open-position = 0.
    APPEND ls_open TO mt_open_files.

    " Initialize FCB fields
    mo_bus->write_mem( iv_addr = iv_fcb_addr + 12 iv_val = 0 ).  " s1 = 0
    mo_bus->write_mem( iv_addr = iv_fcb_addr + 13 iv_val = 0 ).  " s2 = 0
    mo_bus->write_mem( iv_addr = iv_fcb_addr + 14 iv_val = 0 ).  " rc = 0
    mo_bus->write_mem( iv_addr = iv_fcb_addr + 32 iv_val = 0 ).  " cr = 0

    " Get file size in records (128 bytes each)
    lv_size = xstrlen( ls_file-data ).
    lv_records = ( lv_size + 127 ) / 128.
    lv_extent_records = lv_records.
    IF lv_extent_records > 128.
      lv_extent_records = 128.
    ENDIF.
    mo_bus->write_mem( iv_addr = iv_fcb_addr + 15 iv_val = lv_extent_records MOD 256 ).

    rv_result = 0.  " Success
  ENDMETHOD.

  METHOD file_close.
    " BDOS function 16 - Close file
    DATA lv_idx TYPE sy-tabix.

    READ TABLE mt_open_files TRANSPORTING NO FIELDS WITH KEY fcb_addr = iv_fcb_addr.
    IF sy-subrc = 0.
      lv_idx = sy-tabix.
      DELETE mt_open_files INDEX lv_idx.
      rv_result = 0.
    ELSE.
      rv_result = 255.
    ENDIF.
  ENDMETHOD.

  METHOD file_read_seq.
    " BDOS function 20 - Read sequential
    DATA ls_file TYPE ts_file_entry.
    DATA lv_idx TYPE sy-tabix.
    DATA lv_i TYPE i.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_cr TYPE i.
    DATA lv_val TYPE i.
    DATA lv_pos TYPE i.

    READ TABLE mt_open_files INTO ls_file WITH KEY fcb_addr = iv_fcb_addr.
    IF sy-subrc <> 0.
      rv_result = 1.  " File not open
      RETURN.
    ENDIF.
    lv_idx = sy-tabix.

    " Check for EOF
    IF ls_file-position >= xstrlen( ls_file-data ).
      rv_result = 1.  " EOF
      RETURN.
    ENDIF.

    " Read 128 bytes to DMA address
    lv_pos = ls_file-position.
    DO 128 TIMES.
      lv_i = sy-index - 1.
      IF lv_pos < xstrlen( ls_file-data ).
        lv_byte = ls_file-data+lv_pos(1).
        lv_pos = lv_pos + 1.
        lv_val = lv_byte.
      ELSE.
        lv_val = 26.  " Pad with 0x1A (EOF)
      ENDIF.
      mo_bus->write_mem( iv_addr = mv_dma_addr + lv_i iv_val = lv_val ).
    ENDDO.

    " Update position
    ls_file-position = ls_file-position + 128.
    IF ls_file-position > xstrlen( ls_file-data ).
      ls_file-position = xstrlen( ls_file-data ).
    ENDIF.

    " Update in table
    MODIFY mt_open_files FROM ls_file INDEX lv_idx.

    " Update current record in FCB
    lv_cr = mo_bus->read_mem( iv_fcb_addr + 32 ).
    mo_bus->write_mem( iv_addr = iv_fcb_addr + 32 iv_val = ( lv_cr + 1 ) MOD 256 ).

    rv_result = 0.  " Success
  ENDMETHOD.

  METHOD file_read_rand.
    " BDOS function 33 - Random read
    DATA ls_file TYPE ts_file_entry.
    DATA lv_idx TYPE sy-tabix.
    DATA lv_r0 TYPE i.
    DATA lv_r1 TYPE i.
    DATA lv_r2 TYPE i.
    DATA lv_record TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_i TYPE i.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_val TYPE i.
    DATA lv_pos TYPE i.

    READ TABLE mt_open_files INTO ls_file WITH KEY fcb_addr = iv_fcb_addr.
    IF sy-subrc <> 0.
      rv_result = 6.  " Seek past end
      RETURN.
    ENDIF.
    lv_idx = sy-tabix.

    " Read record number from FCB bytes 33-35
    lv_r0 = mo_bus->read_mem( iv_fcb_addr + 33 ).
    lv_r1 = mo_bus->read_mem( iv_fcb_addr + 34 ).
    lv_r2 = mo_bus->read_mem( iv_fcb_addr + 35 ).
    lv_record = lv_r0 + lv_r1 * 256 + lv_r2 * 65536.

    lv_offset = lv_record * 128.

    " Check for EOF
    IF lv_offset >= xstrlen( ls_file-data ).
      rv_result = 1.  " EOF
      RETURN.
    ENDIF.

    " Read 128 bytes to DMA address
    lv_pos = lv_offset.
    DO 128 TIMES.
      lv_i = sy-index - 1.
      IF lv_pos < xstrlen( ls_file-data ).
        lv_byte = ls_file-data+lv_pos(1).
        lv_pos = lv_pos + 1.
        lv_val = lv_byte.
      ELSE.
        lv_val = 26.  " Pad with 0x1A
      ENDIF.
      mo_bus->write_mem( iv_addr = mv_dma_addr + lv_i iv_val = lv_val ).
    ENDDO.

    " Update position
    ls_file-position = lv_offset + 128.
    MODIFY mt_open_files FROM ls_file INDEX lv_idx.

    rv_result = 0.  " Success
  ENDMETHOD.

  METHOD file_size.
    " BDOS function 35 - Get file size
    DATA lv_filename TYPE string.
    DATA ls_file TYPE ts_file_entry.
    DATA lv_found TYPE abap_bool VALUE abap_false.
    DATA lv_size TYPE i.
    DATA lv_records TYPE i.

    lv_filename = fcb_to_filename( iv_fcb_addr ).

    " Look for file in registered file data
    LOOP AT mt_file_data INTO ls_file WHERE filename = to_upper( lv_filename ).
      lv_found = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_found = abap_false.
      rv_result = 255.
      RETURN.
    ENDIF.

    " Calculate size in 128-byte records
    lv_size = xstrlen( ls_file-data ).
    lv_records = ( lv_size + 127 ) / 128.

    " Write to FCB bytes 33-35
    mo_bus->write_mem( iv_addr = iv_fcb_addr + 33 iv_val = lv_records MOD 256 ).
    mo_bus->write_mem( iv_addr = iv_fcb_addr + 34 iv_val = ( lv_records / 256 ) MOD 256 ).
    mo_bus->write_mem( iv_addr = iv_fcb_addr + 35 iv_val = ( lv_records / 65536 ) MOD 256 ).

    rv_result = 0.
  ENDMETHOD.

  METHOD run.
    DATA lv_cycles TYPE i VALUE 0.
    DATA ls_trace TYPE ts_trace_entry.
    DATA lv_pc TYPE i.
    DATA lv_opcode TYPE i.

    CLEAR et_trace.
    mo_bus->clear_output( ).

    WHILE mv_running = abap_true AND lv_cycles < iv_max_cycles.
      " Exit if waiting for input
      IF mv_waiting_input = abap_true.
        EXIT.
      ENDIF.

      " Capture trace before step
      IF iv_trace = abap_true.
        lv_pc = mo_core->get_pc( ).
        lv_opcode = mo_bus->read_mem( lv_pc ).
        ls_trace-pc = lv_pc.
        ls_trace-opcode = lv_opcode.
        ls_trace-sp = mo_core->get_sp( ).
        ls_trace-a = mo_core->get_a( ).
        ls_trace-bc = mo_core->get_bc( ).
        ls_trace-de = mo_core->get_de( ).
        ls_trace-hl = mo_core->get_hl( ).
        CASE lv_pc.
          WHEN 0. ls_trace-info = 'WARM BOOT'.
          WHEN 5. ls_trace-info = 'BDOS ENTRY'.
          WHEN c_bdos_impl. ls_trace-info = |BDOS FN={ mo_core->get_c( ) }|.
          WHEN c_bios_boot. ls_trace-info = 'BIOS BOOT'.
          WHEN OTHERS.
            CASE lv_opcode.
              WHEN 195. ls_trace-info = 'JP'.
              WHEN 201. ls_trace-info = 'RET'.
              WHEN 205. ls_trace-info = 'CALL'.
              WHEN 118. ls_trace-info = 'HALT'.
              WHEN OTHERS. ls_trace-info = ''.
            ENDCASE.
        ENDCASE.
        APPEND ls_trace TO et_trace.
      ENDIF.

      IF step( ) = abap_false.
        EXIT.
      ENDIF.
      lv_cycles = lv_cycles + 1.
    ENDWHILE.

    rv_output = mo_bus->get_output( ).
  ENDMETHOD.

  METHOD step.
    DATA lv_pc TYPE i.
    DATA lv_func TYPE i.

    lv_pc = mo_core->get_pc( ).

    " Check for BDOS intercept at FE00
    IF lv_pc = c_bdos_impl.
      lv_func = mo_core->get_c( ).
      IF bdos_call( lv_func ) = abap_false.
        rv_continue = abap_false.
        RETURN.
      ENDIF.
      mo_cpu->step( ).
      rv_continue = abap_true.
      RETURN.
    ENDIF.

    " Check for BIOS warm boot at FF00
    IF lv_pc = c_bios_boot.
      mv_running = abap_false.
      rv_continue = abap_false.
      RETURN.
    ENDIF.

    " Normal instruction execution
    mo_cpu->step( ).
    rv_continue = abap_true.
  ENDMETHOD.

  METHOD bdos_call.
    DATA lv_addr TYPE i.
    DATA lv_char TYPE i.
    DATA lv_max_len TYPE i.
    DATA lv_line TYPE string.
    DATA lv_i TYPE i.
    DATA lv_result TYPE i.
    DATA lv_de TYPE i.
    DATA lv_fcb TYPE i.
    DATA lv_ex TYPE i.
    DATA lv_cr TYPE i.
    DATA lv_record TYPE i.

    lv_de = mo_core->get_de( ).

    CASE iv_func.
      WHEN c_bdos_system_reset.
        mv_running = abap_false.
        rv_continue = abap_false.
        RETURN.

      WHEN c_bdos_console_input.
        " Wait for input if not ready
        IF mo_bus->is_input_ready( ) = abap_false.
          mv_waiting_input = abap_true.
          rv_continue = abap_false.
          RETURN.
        ENDIF.
        lv_char = mo_bus->read_io( 1 ).
        mo_core->set_a( lv_char ).
        mo_core->set_l( lv_char ).

      WHEN c_bdos_console_output.
        lv_char = mo_core->get_e( ).
        console_output( lv_char ).

      WHEN c_bdos_direct_io.
        lv_char = mo_core->get_e( ).
        IF lv_char = 255.
          " E=255: Read character (wait for input)
          IF mo_bus->is_input_ready( ) = abap_false.
            mv_waiting_input = abap_true.
            rv_continue = abap_false.
            RETURN.
          ENDIF.
          lv_char = mo_bus->read_io( 1 ).
          mo_core->set_a( lv_char ).
        ELSEIF lv_char = 254.
          " E=254: Check status only (don't wait)
          IF mo_bus->is_input_ready( ) = abap_true.
            mo_core->set_a( 255 ).
          ELSE.
            mo_core->set_a( 0 ).
          ENDIF.
        ELSE.
          " E=0-253: Output character
          console_output( lv_char ).
        ENDIF.

      WHEN c_bdos_print_string.
        lv_addr = lv_de.
        WHILE lv_addr <= 65535.
          lv_char = mo_bus->read_mem( lv_addr ).
          IF lv_char = 36.  " '$' terminator
            EXIT.
          ENDIF.
          console_output( lv_char ).
          lv_addr = lv_addr + 1.
        ENDWHILE.

      WHEN c_bdos_read_buffer.
        " Check if input is available before reading
        IF mo_bus->is_input_ready( ) = abap_false.
          " No input ready - pause execution and wait
          mv_waiting_input = abap_true.
          rv_continue = abap_false.
          RETURN.
        ENDIF.
        lv_addr = lv_de.
        lv_max_len = mo_bus->read_mem( lv_addr ).
        IF lv_max_len = 0.
          lv_max_len = 128.
        ENDIF.
        lv_line = read_line( lv_max_len ).
        mo_bus->write_mem( iv_addr = lv_addr + 1 iv_val = strlen( lv_line ) ).
        lv_i = 0.
        WHILE lv_i < strlen( lv_line ).
          DATA lv_ch TYPE c LENGTH 1.
          lv_ch = lv_line+lv_i(1).
          DATA lv_ord TYPE i.
          lv_ord = cl_abap_conv_out_ce=>uccpi( lv_ch ).
          mo_bus->write_mem( iv_addr = lv_addr + 2 + lv_i iv_val = lv_ord ).
          lv_i = lv_i + 1.
        ENDWHILE.

      WHEN c_bdos_console_status.
        IF mo_bus->is_input_ready( ) = abap_true.
          mo_core->set_a( 255 ).
        ELSE.
          mo_core->set_a( 0 ).
        ENDIF.

      WHEN c_bdos_version.
        mo_core->set_hl( 34 ).  " 0x0022 = CP/M 2.2
        mo_core->set_a( 34 ).

      WHEN c_bdos_reset_disk.
        mo_core->set_a( 0 ).

      WHEN c_bdos_select_disk.
        mv_current_disk = mo_core->get_e( ).
        mo_core->set_a( 0 ).

      WHEN c_bdos_open_file.
        lv_result = file_open( lv_de ).
        mo_core->set_a( lv_result ).

      WHEN c_bdos_close_file.
        lv_result = file_close( lv_de ).
        mo_core->set_a( lv_result ).

      WHEN c_bdos_read_seq.
        lv_result = file_read_seq( lv_de ).
        mo_core->set_a( lv_result ).

      WHEN c_bdos_get_disk.
        mo_core->set_a( mv_current_disk ).

      WHEN c_bdos_set_dma.
        mv_dma_addr = lv_de.

      WHEN c_bdos_get_user.
        IF mo_core->get_e( ) = 255.
          mo_core->set_a( mv_user_number ).
        ELSE.
          mv_user_number = mo_core->get_e( ).
        ENDIF.

      WHEN c_bdos_read_rand.
        lv_result = file_read_rand( lv_de ).
        mo_core->set_a( lv_result ).

      WHEN c_bdos_file_size.
        lv_result = file_size( lv_de ).
        mo_core->set_a( lv_result ).

      WHEN c_bdos_set_rand.
        " Convert sequential position to random record number
        lv_fcb = lv_de.
        lv_ex = mo_bus->read_mem( lv_fcb + 12 ).
        lv_cr = mo_bus->read_mem( lv_fcb + 32 ).
        lv_record = ( lv_ex * 128 ) + lv_cr.
        mo_bus->write_mem( iv_addr = lv_fcb + 33 iv_val = lv_record MOD 256 ).
        mo_bus->write_mem( iv_addr = lv_fcb + 34 iv_val = ( lv_record / 256 ) MOD 256 ).
        mo_bus->write_mem( iv_addr = lv_fcb + 35 iv_val = 0 ).
        mo_core->set_a( 0 ).

      WHEN OTHERS.
        mo_core->set_a( 0 ).

    ENDCASE.

    rv_continue = abap_true.
  ENDMETHOD.

  METHOD console_output.
    IF iv_char >= 32 AND iv_char < 127.
      " Printable ASCII characters
      mo_bus->write_io( iv_port = 1 iv_val = iv_char ).
    ELSEIF iv_char = 10.  " LF - Line Feed
      mo_bus->write_io( iv_port = 1 iv_val = iv_char ).
    ELSEIF iv_char = 13.  " CR - Carriage Return (ignore)
      " Do nothing - CP/M uses CRLF, we only need LF
    ELSEIF iv_char = 8.   " BS - Backspace
      mo_bus->remove_last_output( ).
    ELSEIF iv_char = 7.   " BEL - Bell (ignore)
      " Do nothing
    ELSEIF iv_char = 9.  " Tab
      mo_bus->write_io( iv_port = 1 iv_val = 32 ).
      mo_bus->write_io( iv_port = 1 iv_val = 32 ).
      mo_bus->write_io( iv_port = 1 iv_val = 32 ).
      mo_bus->write_io( iv_port = 1 iv_val = 32 ).
    ENDIF.
  ENDMETHOD.

  METHOD read_line.
    DATA lv_char TYPE i.
    DATA lv_len TYPE i VALUE 0.
    DATA lv_ch TYPE c LENGTH 1.

    rv_line = ''.

    WHILE lv_len < iv_max_len.
      IF mo_bus->is_input_ready( ) = abap_false.
        EXIT.
      ENDIF.
      lv_char = mo_bus->read_io( 1 ).
      IF lv_char = 10 OR lv_char = 13.
        EXIT.
      ENDIF.
      lv_ch = cl_abap_conv_in_ce=>uccpi( lv_char ).
      rv_line = rv_line && lv_ch.
      lv_len = lv_len + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_output.
    rv_output = mo_bus->get_output( ).
  ENDMETHOD.

  METHOD is_running.
    rv_running = mv_running.
  ENDMETHOD.

  METHOD is_waiting_input.
    rv_waiting = mv_waiting_input.
  ENDMETHOD.

  METHOD clear_waiting_input.
    mv_waiting_input = abap_false.
  ENDMETHOD.

  METHOD set_debug.
    mv_debug = iv_debug.
  ENDMETHOD.

ENDCLASS.
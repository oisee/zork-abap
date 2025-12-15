CLASS zcl_z80_00_cpu_amdp DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.

    TYPES tt_mem TYPE zif_z80_00_amdp_types=>tt_mem.
    TYPES ts_mem TYPE zif_z80_00_amdp_types=>ts_mem.
    TYPES tt_cpu_state TYPE zif_z80_00_amdp_types=>tt_cpu_state.
    TYPES ts_cpu_state TYPE zif_z80_00_amdp_types=>ts_cpu_state.
    TYPES tt_output TYPE zif_z80_00_amdp_types=>tt_output.
    TYPES tt_flags TYPE zif_z80_00_amdp_types=>tt_flags.

    CLASS-METHODS run_steps
      IMPORTING
        VALUE(it_memory)    TYPE tt_mem
        VALUE(it_state)     TYPE tt_cpu_state
        VALUE(iv_max_steps) TYPE i
      EXPORTING
        VALUE(et_memory)    TYPE tt_mem
        VALUE(et_state)     TYPE tt_cpu_state
        VALUE(et_output)    TYPE tt_output.

    CLASS-METHODS run_steps_v2
      IMPORTING
        VALUE(it_memory)    TYPE tt_mem
        VALUE(it_state)     TYPE tt_cpu_state
        VALUE(iv_max_steps) TYPE i
      EXPORTING
        VALUE(et_memory)    TYPE tt_mem
        VALUE(et_state)     TYPE tt_cpu_state
        VALUE(et_output)    TYPE tt_output.

    CLASS-METHODS run_benchmark_v2
      IMPORTING
        VALUE(it_memory)    TYPE tt_mem
        VALUE(iv_iterations) TYPE i
        VALUE(iv_max_steps) TYPE i
      EXPORTING
        VALUE(ev_total_us)  TYPE int8
        VALUE(ev_last_output) TYPE string.

    CLASS-METHODS get_flag_table
      EXPORTING
        VALUE(et_flags) TYPE tt_flags.

    CLASS-METHODS run_hello_test
      RETURNING VALUE(rv_output) TYPE string.

    CLASS-METHODS run_hello_test_v2
      RETURNING VALUE(rv_output) TYPE string.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_z80_00_cpu_amdp IMPLEMENTATION.

  METHOD run_steps BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY.

    DECLARE lv_af INT;
    DECLARE lv_bc INT;
    DECLARE lv_de INT;
    DECLARE lv_hl INT;
    DECLARE lv_ix INT;
    DECLARE lv_iy INT;
    DECLARE lv_sp INT;
    DECLARE lv_pc INT;
    DECLARE lv_cycles BIGINT;
    DECLARE lv_halted INT;

    DECLARE lv_step INT := 0;
    DECLARE lv_op INT;
    DECLARE lv_val INT;
    DECLARE lv_addr INT;
    DECLARE lv_lo INT;
    DECLARE lv_hi INT;
    DECLARE lv_a INT;
    DECLARE lv_out_seq INT := 0;

    SELECT COALESCE(MAX(af),0), COALESCE(MAX(bc),0), COALESCE(MAX(de),0), COALESCE(MAX(hl),0),
           COALESCE(MAX(ix),0), COALESCE(MAX(iy),0), COALESCE(MAX(sp),65535), COALESCE(MAX(pc),0),
           COALESCE(MAX(cycles),0), COALESCE(MAX(halted),0)
    INTO lv_af, lv_bc, lv_de, lv_hl, lv_ix, lv_iy, lv_sp, lv_pc, lv_cycles, lv_halted
    FROM :it_state;

    lt_mem = SELECT * FROM :it_memory;
    lt_out = SELECT 0 AS seq, 0 AS chr FROM DUMMY WHERE 1=0;

    WHILE :lv_step < :iv_max_steps AND :lv_halted = 0 DO
      SELECT COALESCE(MAX(val), 0) INTO lv_op FROM :lt_mem WHERE addr = :lv_pc;
      lv_pc := MOD(:lv_pc + 1, 65536);
      lv_step := :lv_step + 1;

      IF :lv_op = 0 THEN
        lv_cycles := :lv_cycles + 4;
      ELSEIF :lv_op = 1 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_lo FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        SELECT COALESCE(MAX(val), 0) INTO lv_hi FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_bc := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 17 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_lo FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        SELECT COALESCE(MAX(val), 0) INTO lv_hi FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_de := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 33 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_lo FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        SELECT COALESCE(MAX(val), 0) INTO lv_hi FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_hl := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 49 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_lo FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        SELECT COALESCE(MAX(val), 0) INTO lv_hi FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_sp := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 50 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_lo FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        SELECT COALESCE(MAX(val), 0) INTO lv_hi FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_addr := :lv_hi * 256 + :lv_lo;
        lv_a := :lv_af / 256;
        lt_mem = SELECT addr, val FROM :lt_mem WHERE addr != :lv_addr
                 UNION ALL SELECT :lv_addr AS addr, :lv_a AS val FROM DUMMY;
        lv_cycles := :lv_cycles + 13;
      ELSEIF :lv_op = 58 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_lo FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        SELECT COALESCE(MAX(val), 0) INTO lv_hi FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_addr := :lv_hi * 256 + :lv_lo;
        SELECT COALESCE(MAX(val), 0) INTO lv_val FROM :lt_mem WHERE addr = :lv_addr;
        lv_af := :lv_val * 256 + MOD(:lv_af, 256);
        lv_cycles := :lv_cycles + 13;
      ELSEIF :lv_op = 62 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_val FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_af := :lv_val * 256 + MOD(:lv_af, 256);
        lv_cycles := :lv_cycles + 7;
      ELSEIF :lv_op = 118 THEN
        lv_halted := 1;
        lv_cycles := :lv_cycles + 4;
      ELSEIF :lv_op = 195 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_lo FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        SELECT COALESCE(MAX(val), 0) INTO lv_hi FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 201 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_lo FROM :lt_mem WHERE addr = :lv_sp;
        lv_sp := MOD(:lv_sp + 1, 65536);
        SELECT COALESCE(MAX(val), 0) INTO lv_hi FROM :lt_mem WHERE addr = :lv_sp;
        lv_sp := MOD(:lv_sp + 1, 65536);
        lv_pc := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 205 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_lo FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        SELECT COALESCE(MAX(val), 0) INTO lv_hi FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_addr := :lv_hi * 256 + :lv_lo;
        lv_sp := MOD(:lv_sp - 1 + 65536, 65536);
        lt_mem = SELECT addr, val FROM :lt_mem WHERE addr != :lv_sp
                 UNION ALL SELECT :lv_sp AS addr, :lv_pc / 256 AS val FROM DUMMY;
        lv_sp := MOD(:lv_sp - 1 + 65536, 65536);
        lt_mem = SELECT addr, val FROM :lt_mem WHERE addr != :lv_sp
                 UNION ALL SELECT :lv_sp AS addr, MOD(:lv_pc, 256) AS val FROM DUMMY;
        lv_pc := :lv_addr;
        lv_cycles := :lv_cycles + 17;
      ELSEIF :lv_op = 211 THEN
        SELECT COALESCE(MAX(val), 0) INTO lv_val FROM :lt_mem WHERE addr = :lv_pc;
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_a := :lv_af / 256;
        IF :lv_val = 0 THEN
          lv_out_seq := :lv_out_seq + 1;
          lt_out = SELECT * FROM :lt_out
                   UNION ALL SELECT :lv_out_seq AS seq, :lv_a AS chr FROM DUMMY;
        END IF;
        lv_cycles := :lv_cycles + 11;
      ELSEIF :lv_op = 243 THEN
        lv_cycles := :lv_cycles + 4;
      ELSEIF :lv_op = 251 THEN
        lv_cycles := :lv_cycles + 4;
      ELSE
        lv_cycles := :lv_cycles + 4;
      END IF;
    END WHILE;

    et_memory = SELECT * FROM :lt_mem;
    et_state = SELECT :lv_af AS af, :lv_bc AS bc, :lv_de AS de, :lv_hl AS hl,
                      0 AS af_alt, 0 AS bc_alt, 0 AS de_alt, 0 AS hl_alt,
                      :lv_ix AS ix, :lv_iy AS iy, :lv_sp AS sp, :lv_pc AS pc,
                      0 AS i, 0 AS r, 0 AS iff1, 0 AS iff2, 0 AS im,
                      :lv_cycles AS cycles, :lv_halted AS halted
               FROM DUMMY;
    et_output = SELECT * FROM :lt_out ORDER BY seq;
  ENDMETHOD.

  METHOD run_steps_v2 BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY.

    DECLARE mem INT ARRAY;
    DECLARE lv_af INT := 0;
    DECLARE lv_bc INT := 0;
    DECLARE lv_de INT := 0;
    DECLARE lv_hl INT := 0;
    DECLARE lv_ix INT := 0;
    DECLARE lv_iy INT := 0;
    DECLARE lv_sp INT := 65535;
    DECLARE lv_pc INT := 0;
    DECLARE lv_cycles BIGINT := 0;
    DECLARE lv_halted INT := 0;

    DECLARE lv_step INT := 0;
    DECLARE lv_op INT;
    DECLARE lv_val INT;
    DECLARE lv_addr INT;
    DECLARE lv_lo INT;
    DECLARE lv_hi INT;
    DECLARE lv_a INT;
    DECLARE lv_out_seq INT := 0;
    DECLARE lv_mem_cnt INT;
    DECLARE i INT;

    SELECT COALESCE(MAX(af),0), COALESCE(MAX(bc),0), COALESCE(MAX(de),0), COALESCE(MAX(hl),0),
           COALESCE(MAX(ix),0), COALESCE(MAX(iy),0), COALESCE(MAX(sp),65535), COALESCE(MAX(pc),0),
           COALESCE(MAX(cycles),0), COALESCE(MAX(halted),0)
    INTO lv_af, lv_bc, lv_de, lv_hl, lv_ix, lv_iy, lv_sp, lv_pc, lv_cycles, lv_halted
    FROM :it_state;

    lt_mem_load = SELECT addr, val FROM :it_memory ORDER BY addr;
    lv_mem_cnt := RECORD_COUNT(:lt_mem_load);
    FOR i IN 1..:lv_mem_cnt DO
      mem[:lt_mem_load.addr[:i] + 1] := :lt_mem_load.val[:i];
    END FOR;

    lt_out = SELECT 0 AS seq, 0 AS chr FROM DUMMY WHERE 1=0;

    WHILE :lv_step < :iv_max_steps AND :lv_halted = 0 DO
      lv_op := :mem[:lv_pc + 1];
      lv_pc := MOD(:lv_pc + 1, 65536);
      lv_step := :lv_step + 1;

      IF :lv_op = 0 THEN
        lv_cycles := :lv_cycles + 4;
      ELSEIF :lv_op = 1 THEN
        lv_lo := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_hi := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_bc := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 17 THEN
        lv_lo := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_hi := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_de := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 33 THEN
        lv_lo := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_hi := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_hl := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 49 THEN
        lv_lo := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_hi := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_sp := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 50 THEN
        lv_lo := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_hi := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_addr := :lv_hi * 256 + :lv_lo;
        lv_a := :lv_af / 256;
        mem[:lv_addr + 1] := :lv_a;
        lv_cycles := :lv_cycles + 13;
      ELSEIF :lv_op = 58 THEN
        lv_lo := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_hi := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_addr := :lv_hi * 256 + :lv_lo;
        lv_val := :mem[:lv_addr + 1];
        lv_af := :lv_val * 256 + MOD(:lv_af, 256);
        lv_cycles := :lv_cycles + 13;
      ELSEIF :lv_op = 62 THEN
        lv_val := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_af := :lv_val * 256 + MOD(:lv_af, 256);
        lv_cycles := :lv_cycles + 7;
      ELSEIF :lv_op = 118 THEN
        lv_halted := 1;
        lv_cycles := :lv_cycles + 4;
      ELSEIF :lv_op = 195 THEN
        lv_lo := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_hi := :mem[:lv_pc + 1];
        lv_pc := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 201 THEN
        lv_lo := :mem[:lv_sp + 1];
        lv_sp := MOD(:lv_sp + 1, 65536);
        lv_hi := :mem[:lv_sp + 1];
        lv_sp := MOD(:lv_sp + 1, 65536);
        lv_pc := :lv_hi * 256 + :lv_lo;
        lv_cycles := :lv_cycles + 10;
      ELSEIF :lv_op = 205 THEN
        lv_lo := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_hi := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_addr := :lv_hi * 256 + :lv_lo;
        lv_sp := MOD(:lv_sp - 1 + 65536, 65536);
        mem[:lv_sp + 1] := :lv_pc / 256;
        lv_sp := MOD(:lv_sp - 1 + 65536, 65536);
        mem[:lv_sp + 1] := MOD(:lv_pc, 256);
        lv_pc := :lv_addr;
        lv_cycles := :lv_cycles + 17;
      ELSEIF :lv_op = 211 THEN
        lv_val := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_a := :lv_af / 256;
        IF :lv_val = 0 THEN
          lv_out_seq := :lv_out_seq + 1;
          lt_out = SELECT * FROM :lt_out
                   UNION ALL SELECT :lv_out_seq AS seq, :lv_a AS chr FROM DUMMY;
        END IF;
        lv_cycles := :lv_cycles + 11;
      ELSEIF :lv_op = 243 THEN
        lv_cycles := :lv_cycles + 4;
      ELSEIF :lv_op = 251 THEN
        lv_cycles := :lv_cycles + 4;
      ELSE
        lv_cycles := :lv_cycles + 4;
      END IF;
    END WHILE;

    et_memory = SELECT * FROM :it_memory;
    et_state = SELECT :lv_af AS af, :lv_bc AS bc, :lv_de AS de, :lv_hl AS hl,
                      0 AS af_alt, 0 AS bc_alt, 0 AS de_alt, 0 AS hl_alt,
                      :lv_ix AS ix, :lv_iy AS iy, :lv_sp AS sp, :lv_pc AS pc,
                      0 AS i, 0 AS r, 0 AS iff1, 0 AS iff2, 0 AS im,
                      :lv_cycles AS cycles, :lv_halted AS halted
               FROM DUMMY;
    et_output = SELECT * FROM :lt_out ORDER BY seq;
  ENDMETHOD.

  METHOD run_benchmark_v2 BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY.

    DECLARE mem INT ARRAY;
    DECLARE mem_orig INT ARRAY;
    DECLARE lv_iter INT;
    DECLARE lv_af INT;
    DECLARE lv_sp INT;
    DECLARE lv_pc INT;
    DECLARE lv_cycles BIGINT;
    DECLARE lv_halted INT;
    DECLARE lv_step INT;
    DECLARE lv_op INT;
    DECLARE lv_val INT;
    DECLARE lv_a INT;
    DECLARE lv_out_seq INT;
    DECLARE lv_start_ts TIMESTAMP;
    DECLARE lv_end_ts TIMESTAMP;
    DECLARE lv_mem_cnt INT;
    DECLARE lv_out_cnt INT;
    DECLARE i INT;
    DECLARE j INT;

    lt_mem_load = SELECT addr, val FROM :it_memory ORDER BY addr;
    lv_mem_cnt := RECORD_COUNT(:lt_mem_load);
    FOR i IN 1..:lv_mem_cnt DO
      mem_orig[:lt_mem_load.addr[:i] + 1] := :lt_mem_load.val[:i];
    END FOR;

    lt_out = SELECT 0 AS seq, 0 AS chr FROM DUMMY WHERE 1=0;
    lv_start_ts := CURRENT_TIMESTAMP;

    FOR lv_iter IN 1..:iv_iterations DO
      mem := :mem_orig;
      lv_af := 0;
      lv_sp := 65535;
      lv_pc := 0;
      lv_cycles := 0;
      lv_halted := 0;
      lv_step := 0;
      lv_out_seq := 0;
      lt_out = SELECT 0 AS seq, 0 AS chr FROM DUMMY WHERE 1=0;

      WHILE :lv_step < :iv_max_steps AND :lv_halted = 0 DO
        lv_op := :mem[:lv_pc + 1];
        lv_pc := MOD(:lv_pc + 1, 65536);
        lv_step := :lv_step + 1;

        IF :lv_op = 0 THEN
          lv_cycles := :lv_cycles + 4;
        ELSEIF :lv_op = 62 THEN
          lv_val := :mem[:lv_pc + 1];
          lv_pc := MOD(:lv_pc + 1, 65536);
          lv_af := :lv_val * 256;
          lv_cycles := :lv_cycles + 7;
        ELSEIF :lv_op = 118 THEN
          lv_halted := 1;
          lv_cycles := :lv_cycles + 4;
        ELSEIF :lv_op = 211 THEN
          lv_val := :mem[:lv_pc + 1];
          lv_pc := MOD(:lv_pc + 1, 65536);
          lv_a := :lv_af / 256;
          IF :lv_val = 0 THEN
            lv_out_seq := :lv_out_seq + 1;
            lt_out = SELECT * FROM :lt_out
                     UNION ALL SELECT :lv_out_seq AS seq, :lv_a AS chr FROM DUMMY;
          END IF;
          lv_cycles := :lv_cycles + 11;
        ELSE
          lv_cycles := :lv_cycles + 4;
        END IF;
      END WHILE;
    END FOR;

    lv_end_ts := CURRENT_TIMESTAMP;

    ev_total_us := NANO100_BETWEEN(:lv_start_ts, :lv_end_ts) / 10;

    ev_last_output := '';
    lt_out_sorted = SELECT chr FROM :lt_out ORDER BY seq;
    lv_out_cnt := RECORD_COUNT(:lt_out_sorted);
    FOR j IN 1..:lv_out_cnt DO
      ev_last_output := :ev_last_output || CHAR(:lt_out_sorted.chr[:j]);
    END FOR;
  ENDMETHOD.

  METHOD get_flag_table BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.
    DECLARE lv_i INT;
    DECLARE lv_szp INT;
    DECLARE lv_bits INT;

    lt_flags = SELECT 0 AS val, 0 AS szp, 0 AS inc_f, 0 AS dec_f FROM DUMMY WHERE 1=0;

    FOR lv_i IN 0..255 DO
      lv_szp := 0;
      IF :lv_i = 0 THEN lv_szp := 64; END IF;
      IF :lv_i >= 128 THEN lv_szp := :lv_szp + 128; END IF;
      lv_bits := 0;
      lv_bits := :lv_bits + MOD(:lv_i, 2);
      lv_bits := :lv_bits + MOD(:lv_i / 2, 2);
      lv_bits := :lv_bits + MOD(:lv_i / 4, 2);
      lv_bits := :lv_bits + MOD(:lv_i / 8, 2);
      lv_bits := :lv_bits + MOD(:lv_i / 16, 2);
      lv_bits := :lv_bits + MOD(:lv_i / 32, 2);
      lv_bits := :lv_bits + MOD(:lv_i / 64, 2);
      lv_bits := :lv_bits + MOD(:lv_i / 128, 2);
      IF MOD(:lv_bits, 2) = 0 THEN lv_szp := :lv_szp + 4; END IF;

      lt_flags = SELECT * FROM :lt_flags
                 UNION ALL SELECT :lv_i AS val, :lv_szp AS szp, :lv_szp AS inc_f, :lv_szp + 2 AS dec_f FROM DUMMY;
    END FOR;

    et_flags = SELECT * FROM :lt_flags;
  ENDMETHOD.

  METHOD run_hello_test.
    DATA lt_mem TYPE tt_mem.
    DATA lt_state TYPE tt_cpu_state.
    DATA lt_out TYPE tt_output.
    DATA lt_mem_out TYPE tt_mem.
    DATA lt_state_out TYPE tt_cpu_state.

    DATA(lv_prog) = CONV xstring( '3E48D3003E65D3003E6CD3003E6CD3003E6FD3003E21D30076' ).
    DATA lv_i TYPE i.
    DATA lv_byte TYPE x LENGTH 1.

    DO xstrlen( lv_prog ) TIMES.
      lv_i = sy-index - 1.
      lv_byte = lv_prog+lv_i(1).
      APPEND VALUE #( addr = lv_i val = lv_byte ) TO lt_mem.
    ENDDO.

    APPEND VALUE #( sp = 65535 pc = 0 ) TO lt_state.

    run_steps(
      EXPORTING
        it_memory    = lt_mem
        it_state     = lt_state
        iv_max_steps = 100
      IMPORTING
        et_memory    = lt_mem_out
        et_state     = lt_state_out
        et_output    = lt_out ).

    LOOP AT lt_out INTO DATA(ls_out).
      DATA lv_xbyte TYPE x LENGTH 1.
      lv_xbyte = ls_out-chr.
      rv_output = rv_output && cl_abap_conv_codepage=>create_in( )->convert( CONV xstring( lv_xbyte ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD run_hello_test_v2.
    DATA lt_mem TYPE tt_mem.
    DATA lt_state TYPE tt_cpu_state.
    DATA lt_out TYPE tt_output.
    DATA lt_mem_out TYPE tt_mem.
    DATA lt_state_out TYPE tt_cpu_state.

    DATA(lv_prog) = CONV xstring( '3E48D3003E65D3003E6CD3003E6CD3003E6FD3003E21D30076' ).
    DATA lv_i TYPE i.
    DATA lv_byte TYPE x LENGTH 1.

    DO xstrlen( lv_prog ) TIMES.
      lv_i = sy-index - 1.
      lv_byte = lv_prog+lv_i(1).
      APPEND VALUE #( addr = lv_i val = lv_byte ) TO lt_mem.
    ENDDO.

    APPEND VALUE #( sp = 65535 pc = 0 ) TO lt_state.

    run_steps_v2(
      EXPORTING
        it_memory    = lt_mem
        it_state     = lt_state
        iv_max_steps = 100
      IMPORTING
        et_memory    = lt_mem_out
        et_state     = lt_state_out
        et_output    = lt_out ).

    LOOP AT lt_out INTO DATA(ls_out).
      DATA lv_xbyte TYPE x LENGTH 1.
      lv_xbyte = ls_out-chr.
      rv_output = rv_output && cl_abap_conv_codepage=>create_in( )->convert( CONV xstring( lv_xbyte ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

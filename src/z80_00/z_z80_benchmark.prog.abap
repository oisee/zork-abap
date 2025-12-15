*&---------------------------------------------------------------------*
*& Report Z_Z80_BENCHMARK
*& Z80 Emulator - Performance Benchmark
*& Estimates equivalent MHz based on opcode execution time
*&---------------------------------------------------------------------*
REPORT z_z80_benchmark.

PARAMETERS: p_runs TYPE i DEFAULT 1000.

START-OF-SELECTION.
  DATA: lv_start    TYPE i,
        lv_end      TYPE i,
        lv_total_us TYPE i,
        lv_run      TYPE i.

  " Hello World program: 13 opcodes, 112 T-states total
  DATA(lv_program) = CONV xstring( '3E48D3003E65D3003E6CD3003E6CD3003E6FD3003E21D30076' ).
  DATA(lv_opcodes_per_run) = 13.
  DATA(lv_cycles_per_run) = 112.

  cl_demo_output=>write( |Z80 Emulator Benchmark| ).
  cl_demo_output=>write( |======================| ).
  cl_demo_output=>write( |Program: Hello World (13 opcodes, 112 T-states)| ).
  cl_demo_output=>write( |Iterations: { p_runs }| ).
  cl_demo_output=>write( || ).

  " Warmup run
  DATA(lo_bus_warmup) = NEW zcl_z80_00_bus( ).
  DATA(lo_cpu_warmup) = NEW zcl_z80_00_cpu( lo_bus_warmup ).
  lo_bus_warmup->zif_z80_00_bus~load( iv_addr = 0 iv_data = lv_program ).
  lo_cpu_warmup->run( iv_max_cycles = 1000 ).

  " Benchmark
  GET RUN TIME FIELD lv_start.

  DO p_runs TIMES.
    DATA(lo_bus) = NEW zcl_z80_00_bus( ).
    DATA(lo_cpu) = NEW zcl_z80_00_cpu( lo_bus ).
    lo_bus->zif_z80_00_bus~load( iv_addr = 0 iv_data = lv_program ).
    lo_cpu->run( iv_max_cycles = 1000 ).
  ENDDO.

  GET RUN TIME FIELD lv_end.

  lv_total_us = lv_end - lv_start.

  " Calculate metrics
  DATA(lv_total_opcodes) = p_runs * lv_opcodes_per_run.
  DATA(lv_total_cycles) = p_runs * lv_cycles_per_run.
  DATA(lv_total_sec) = CONV decfloat34( lv_total_us ) / 1000000.
  DATA(lv_us_per_run) = CONV decfloat34( lv_total_us ) / p_runs.
  DATA(lv_us_per_opcode) = CONV decfloat34( lv_total_us ) / lv_total_opcodes.
  DATA(lv_opcodes_per_sec) = CONV decfloat34( lv_total_opcodes ) / lv_total_sec.
  DATA(lv_cycles_per_sec) = CONV decfloat34( lv_total_cycles ) / lv_total_sec.

  " MHz calculation (cycles per second / 1,000,000)
  DATA(lv_mhz) = lv_cycles_per_sec / 1000000.

  " Original Z80 comparison
  DATA(lv_z80_4mhz_us) = CONV decfloat34( lv_cycles_per_run ) / 4. " 4MHz = 4 cycles/µs

  cl_demo_output=>write( |--- Timing Results ---| ).
  cl_demo_output=>write( |Total time: { lv_total_us } µs ({ lv_total_sec } sec)| ).
  cl_demo_output=>write( |Time per run: { lv_us_per_run } µs| ).
  cl_demo_output=>write( |Time per opcode: { lv_us_per_opcode } µs| ).
  cl_demo_output=>write( || ).

  cl_demo_output=>write( |--- Performance Metrics ---| ).
  cl_demo_output=>write( |Total opcodes executed: { lv_total_opcodes }| ).
  cl_demo_output=>write( |Total T-states: { lv_total_cycles }| ).
  cl_demo_output=>write( |Opcodes/second: { lv_opcodes_per_sec }| ).
  cl_demo_output=>write( |T-states/second: { lv_cycles_per_sec }| ).
  cl_demo_output=>write( || ).

  cl_demo_output=>write( |--- Equivalent Clock Speed ---| ).
  cl_demo_output=>write( |Equivalent MHz: { lv_mhz }| ).
  cl_demo_output=>write( || ).

  cl_demo_output=>write( |--- Comparison ---| ).
  cl_demo_output=>write( |Original Z80 @ 4MHz would run Hello in: { lv_z80_4mhz_us } µs| ).
  cl_demo_output=>write( |This emulator runs Hello in: { lv_us_per_run } µs| ).
  DATA(lv_ratio) = lv_us_per_run / lv_z80_4mhz_us.
  cl_demo_output=>write( |Speed ratio: { lv_ratio }x slower than 4MHz Z80| ).
  cl_demo_output=>write( || ).

  " Verify output
  cl_demo_output=>write( |--- Verification ---| ).
  cl_demo_output=>write( |Last run output: "{ lo_bus->zif_z80_00_bus~get_output( ) }"| ).
  cl_demo_output=>write( |CPU halted: { lo_cpu->is_halted( ) }| ).

  cl_demo_output=>display( ).
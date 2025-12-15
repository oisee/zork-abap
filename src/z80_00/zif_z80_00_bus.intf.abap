INTERFACE zif_z80_00_bus PUBLIC.
************************************************************************
* Z80 Bus Interface
* Abstracts memory and I/O port access
* Implementations: internal tables, HANA tables, or AMDP
************************************************************************

  " Memory read (0-65535)
  METHODS read_mem
    IMPORTING iv_addr       TYPE i
    RETURNING VALUE(rv_val) TYPE i.

  " Memory write (0-65535)
  METHODS write_mem
    IMPORTING iv_addr TYPE i
              iv_val  TYPE i.

  " I/O port read (0-255)
  METHODS read_io
    IMPORTING iv_port       TYPE i
    RETURNING VALUE(rv_val) TYPE i.

  " I/O port write (0-255)
  METHODS write_io
    IMPORTING iv_port TYPE i
              iv_val  TYPE i.

  " Load binary data into memory
  METHODS load
    IMPORTING iv_addr TYPE i
              iv_data TYPE xstring.

  " Console I/O support
  METHODS is_input_ready
    RETURNING VALUE(rv_ready) TYPE abap_bool.

  METHODS get_output
    RETURNING VALUE(rv_output) TYPE string.

  METHODS clear_output.

  METHODS provide_input
    IMPORTING iv_text TYPE string.

ENDINTERFACE.
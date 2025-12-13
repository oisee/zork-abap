*"* use this source file for your ABAP unit test classes
CLASS ltcl_memory DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_memory TYPE REF TO zcl_ork_00_memory.

    METHODS setup.
    METHODS test_constructor FOR TESTING.
    METHODS test_u8_read FOR TESTING.
    METHODS test_u8_boundary FOR TESTING.
    METHODS test_w8_write FOR TESTING.
    METHODS test_u16_read FOR TESTING.
    METHODS test_w16_write FOR TESTING.
    METHODS test_global_variables FOR TESTING.
    METHODS test_header_parsing FOR TESTING.
    METHODS test_empty_story FOR TESTING.
    METHODS test_w8_overflow FOR TESTING.
    METHODS test_w16_overflow FOR TESTING.

    METHODS create_minimal_v3_header
      RETURNING VALUE(rv_data) TYPE xstring.
ENDCLASS.


CLASS ltcl_memory IMPLEMENTATION.

  METHOD setup.
    " Create memory with a minimal V3 story header
    DATA(lv_data) = create_minimal_v3_header( ).
    mo_memory = NEW zcl_ork_00_memory( lv_data ).
  ENDMETHOD.

  METHOD create_minimal_v3_header.
    " Create a minimal 128-byte V3 story file header
    " Byte layout:
    "   0: version (3)
    "   1: flags1
    "   4-5: high_mem
    "   6-7: initial_pc
    "   8-9: dict_addr
    "   10-11: obj_addr
    "   12-13: globals_addr
    "   14-15: static_mem
    "   18-23: serial (6 chars)
    "   24-25: abbrev_addr
    "   26-27: file_len (div 2 for V3)
    "   28-29: checksum

    DATA lv_data TYPE xstring.
    DATA lv_hex TYPE string.

    " 128 bytes of zeros (256 hex chars)
    lv_hex = repeat( val = `00` occ = 128 ).

    " Set version = 3 at offset 0
    lv_hex = `03` && lv_hex+2.

    " Set flags1 = 0 at offset 1 (already 0)

    " Set high_mem = 0x0080 at offset 4-5
    lv_hex = lv_hex+0(8) && `0080` && lv_hex+12.

    " Set initial_pc = 0x0040 at offset 6-7
    lv_hex = lv_hex+0(12) && `0040` && lv_hex+16.

    " Set dict_addr = 0x0060 at offset 8-9
    lv_hex = lv_hex+0(16) && `0060` && lv_hex+20.

    " Set obj_addr = 0x0050 at offset 10-11
    lv_hex = lv_hex+0(20) && `0050` && lv_hex+24.

    " Set globals_addr = 0x0070 at offset 12-13
    lv_hex = lv_hex+0(24) && `0070` && lv_hex+28.

    " Set static_mem = 0x0080 at offset 14-15
    lv_hex = lv_hex+0(28) && `0080` && lv_hex+32.

    " Set serial "123456" at offset 18-23 (ASCII: 31 32 33 34 35 36)
    lv_hex = lv_hex+0(36) && `313233343536` && lv_hex+48.

    " Set abbrev_addr = 0x0030 at offset 24-25
    lv_hex = lv_hex+0(48) && `0030` && lv_hex+52.

    " Set file_len = 64 (0x0040) at offset 26-27 -> actual = 128 bytes
    lv_hex = lv_hex+0(52) && `0040` && lv_hex+56.

    " Set checksum = 0 at offset 28-29 (already 0)

    " Convert hex string to xstring
    lv_data = lv_hex.

    rv_data = lv_data.
  ENDMETHOD.

  METHOD test_constructor.
    cl_abap_unit_assert=>assert_not_initial(
      act = mo_memory
      msg = 'Memory object should be created' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->mem_size
      exp = 128
      msg = 'Memory size should be 128 bytes' ).
  ENDMETHOD.

  METHOD test_header_parsing.
    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->version
      exp = 3
      msg = 'Version should be 3' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->high_mem
      exp = 128
      msg = 'High memory should be 0x0080 = 128' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->initial_pc
      exp = 64
      msg = 'Initial PC should be 0x0040 = 64' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->dict_addr
      exp = 96
      msg = 'Dictionary address should be 0x0060 = 96' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->obj_addr
      exp = 80
      msg = 'Object table address should be 0x0050 = 80' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->globals_addr
      exp = 112
      msg = 'Globals address should be 0x0070 = 112' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->serial
      exp = '123456'
      msg = 'Serial should be 123456' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->file_len
      exp = 128
      msg = 'File length should be 64*2 = 128' ).
  ENDMETHOD.

  METHOD test_u8_read.
    " First byte is version = 3
    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->u8( 0 )
      exp = 3
      msg = 'Byte at offset 0 should be 3' ).
  ENDMETHOD.

  METHOD test_u8_boundary.
    " Reading out of bounds should return 0
    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->u8( -1 )
      exp = 0
      msg = 'Negative offset should return 0' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->u8( 1000 )
      exp = 0
      msg = 'Out of bounds offset should return 0' ).
  ENDMETHOD.

  METHOD test_w8_write.
    " Write a byte and read it back
    mo_memory->w8( iv_addr = 100 iv_val = 42 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->u8( 100 )
      exp = 42
      msg = 'Written byte should be read back' ).
  ENDMETHOD.

  METHOD test_w8_overflow.
    " Writing value > 255 should wrap
    mo_memory->w8( iv_addr = 100 iv_val = 300 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->u8( 100 )
      exp = 44
      msg = '300 mod 256 = 44' ).
  ENDMETHOD.

  METHOD test_u16_read.
    " Read word at offset 4-5 (high_mem = 0x0080)
    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->u16( 4 )
      exp = 128
      msg = 'Word at offset 4 should be 0x0080 = 128' ).
  ENDMETHOD.

  METHOD test_w16_write.
    " Write a word and read it back
    mo_memory->w16( iv_addr = 100 iv_val = 12345 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->u16( 100 )
      exp = 12345
      msg = 'Written word should be read back' ).

    " Verify big-endian storage
    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->u8( 100 )
      exp = 48
      msg = 'High byte should be 12345 div 256 = 48' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->u8( 101 )
      exp = 57
      msg = 'Low byte should be 12345 mod 256 = 57' ).
  ENDMETHOD.

  METHOD test_w16_overflow.
    " Writing value > 65535 should wrap
    mo_memory->w16( iv_addr = 100 iv_val = 70000 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->u16( 100 )
      exp = 4464
      msg = '70000 mod 65536 = 4464' ).
  ENDMETHOD.

  METHOD test_global_variables.
    " Globals start at globals_addr (112)
    " Global 16 is at offset 0, global 17 at offset 2, etc.

    " Set global 16
    mo_memory->set_global( iv_var = 16 iv_val = 1000 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->get_global( 16 )
      exp = 1000
      msg = 'Global 16 should be 1000' ).

    " Set global 17
    mo_memory->set_global( iv_var = 17 iv_val = 2000 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->get_global( 17 )
      exp = 2000
      msg = 'Global 17 should be 2000' ).

    " Verify global 16 is still intact
    cl_abap_unit_assert=>assert_equals(
      act = mo_memory->get_global( 16 )
      exp = 1000
      msg = 'Global 16 should still be 1000' ).
  ENDMETHOD.

  METHOD test_empty_story.
    " Test with very small/empty story
    DATA(lv_empty) = VALUE xstring( ).
    DATA(lo_empty_mem) = NEW zcl_ork_00_memory( lv_empty ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_empty_mem->mem_size
      exp = 0
      msg = 'Empty story should have size 0' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_empty_mem->version
      exp = 0
      msg = 'Empty story should have version 0' ).
  ENDMETHOD.

ENDCLASS.

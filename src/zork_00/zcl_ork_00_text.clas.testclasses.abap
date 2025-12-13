*"* use this source file for your ABAP unit test classes
CLASS ltcl_text DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_encode_simple FOR TESTING.
    METHODS test_encode_short_word FOR TESTING.
    METHODS test_encode_padding FOR TESTING.
    METHODS test_encode_uppercase FOR TESTING.
    METHODS test_alphabet_constants FOR TESTING.
    METHODS test_decode_with_story FOR TESTING.
    METHODS test_decode_multiline FOR TESTING.
ENDCLASS.


CLASS ltcl_text IMPLEMENTATION.

  METHOD test_alphabet_constants.
    " Verify alphabet constants are correct
    cl_abap_unit_assert=>assert_equals(
      act = zcl_ork_00_text=>c_a0
      exp = 'abcdefghijklmnopqrstuvwxyz'
      msg = 'A0 alphabet should be lowercase' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_ork_00_text=>c_a1
      exp = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      msg = 'A1 alphabet should be uppercase' ).

    cl_abap_unit_assert=>assert_equals(
      act = strlen( zcl_ork_00_text=>c_a2 )
      exp = 26
      msg = 'A2 alphabet should have 26 chars' ).
  ENDMETHOD.

  METHOD test_encode_simple.
    " Test encoding a simple 6-char word
    DATA lv_word1 TYPE i.
    DATA lv_word2 TYPE i.

    " Create minimal memory
    DATA lv_data TYPE xstring.
    lv_data = repeat( val = '00' occ = 128 ).
    lv_data = '03' && lv_data+2.
    DATA(lo_mem) = NEW zcl_ork_00_memory( lv_data ).
    DATA(lo_text) = NEW zcl_ork_00_text( lo_mem ).

    " Encode "abcdef"
    " a=6, b=7, c=8, d=9, e=10, f=11 (z-chars for a-f)
    lo_text->encode_for_dict(
      EXPORTING iv_text = 'abcdef'
      IMPORTING ev_word1 = lv_word1 ev_word2 = lv_word2 ).

    " Word1 = (6 << 10) | (7 << 5) | 8 = 6144 + 224 + 8 = 6376
    cl_abap_unit_assert=>assert_equals(
      act = lv_word1
      exp = 6376
      msg = 'Word1 for "abcdef" should be 6376' ).

    " Word2 = 0x8000 | (9 << 10) | (10 << 5) | 11 = 32768 + 9216 + 320 + 11 = 42315
    cl_abap_unit_assert=>assert_equals(
      act = lv_word2
      exp = 42315
      msg = 'Word2 for "abcdef" should be 42315' ).
  ENDMETHOD.

  METHOD test_encode_short_word.
    " Test encoding a word shorter than 6 chars (should pad with 5)
    DATA lv_word1 TYPE i.
    DATA lv_word2 TYPE i.

    DATA lv_data TYPE xstring.
    lv_data = repeat( val = '00' occ = 128 ).
    lv_data = '03' && lv_data+2.
    DATA(lo_mem) = NEW zcl_ork_00_memory( lv_data ).
    DATA(lo_text) = NEW zcl_ork_00_text( lo_mem ).

    " Encode "ab" -> padded to "ab5555" where 5 is padding z-char
    " a=6, b=7, pad=5
    lo_text->encode_for_dict(
      EXPORTING iv_text = 'ab'
      IMPORTING ev_word1 = lv_word1 ev_word2 = lv_word2 ).

    " Word1 = (6 << 10) | (7 << 5) | 5 = 6144 + 224 + 5 = 6373
    cl_abap_unit_assert=>assert_equals(
      act = lv_word1
      exp = 6373
      msg = 'Word1 for "ab" should be 6373' ).

    " Word2 = 0x8000 | (5 << 10) | (5 << 5) | 5 = 32768 + 5120 + 160 + 5 = 38053
    cl_abap_unit_assert=>assert_equals(
      act = lv_word2
      exp = 38053
      msg = 'Word2 for "ab" should be 38053' ).
  ENDMETHOD.

  METHOD test_encode_padding.
    " Test that words longer than 6 chars are truncated
    DATA lv_word1 TYPE i.
    DATA lv_word2 TYPE i.
    DATA lv_word1_trunc TYPE i.
    DATA lv_word2_trunc TYPE i.

    DATA lv_data TYPE xstring.
    lv_data = repeat( val = '00' occ = 128 ).
    lv_data = '03' && lv_data+2.
    DATA(lo_mem) = NEW zcl_ork_00_memory( lv_data ).
    DATA(lo_text) = NEW zcl_ork_00_text( lo_mem ).

    " Encode "abcdefgh" should be same as "abcdef"
    lo_text->encode_for_dict(
      EXPORTING iv_text = 'abcdefgh'
      IMPORTING ev_word1 = lv_word1 ev_word2 = lv_word2 ).

    lo_text->encode_for_dict(
      EXPORTING iv_text = 'abcdef'
      IMPORTING ev_word1 = lv_word1_trunc ev_word2 = lv_word2_trunc ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_word1
      exp = lv_word1_trunc
      msg = 'Long word should truncate to 6 chars (word1)' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_word2
      exp = lv_word2_trunc
      msg = 'Long word should truncate to 6 chars (word2)' ).
  ENDMETHOD.

  METHOD test_encode_uppercase.
    " Test that uppercase is converted to lowercase
    DATA lv_word1_lower TYPE i.
    DATA lv_word2_lower TYPE i.
    DATA lv_word1_upper TYPE i.
    DATA lv_word2_upper TYPE i.

    DATA lv_data TYPE xstring.
    lv_data = repeat( val = '00' occ = 128 ).
    lv_data = '03' && lv_data+2.
    DATA(lo_mem) = NEW zcl_ork_00_memory( lv_data ).
    DATA(lo_text) = NEW zcl_ork_00_text( lo_mem ).

    lo_text->encode_for_dict(
      EXPORTING iv_text = 'hello'
      IMPORTING ev_word1 = lv_word1_lower ev_word2 = lv_word2_lower ).

    lo_text->encode_for_dict(
      EXPORTING iv_text = 'HELLO'
      IMPORTING ev_word1 = lv_word1_upper ev_word2 = lv_word2_upper ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_word1_upper
      exp = lv_word1_lower
      msg = 'HELLO should encode same as hello (word1)' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_word2_upper
      exp = lv_word2_lower
      msg = 'HELLO should encode same as hello (word2)' ).
  ENDMETHOD.

  METHOD test_decode_with_story.
    " Test decoding with actual story file if available
    DATA(lo_game_loader) = NEW zcl_ork_00_game_loader_smw0( ).

    TRY.
        DATA(lv_story) = lo_game_loader->zif_ork_00_game_loader~load( 'ZORK-MINI-Z3' ).
      CATCH cx_root.
        " Skip test if story not available
        RETURN.
    ENDTRY.

    IF lv_story IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_memory) = NEW zcl_ork_00_memory( lv_story ).
    DATA(lo_text) = NEW zcl_ork_00_text( lo_memory ).

    " Decode text at initial PC (usually has some intro text)
    DATA lv_text TYPE string.
    DATA lv_len TYPE i.

    " Find a known text location in the story
    " The dictionary entries usually contain text
    IF lo_memory->dict_addr > 0.
      " Dictionary header: num_seps, seps, entry_len, num_entries, entries
      DATA(lv_dict) = lo_memory->dict_addr.
      DATA(lv_num_seps) = lo_memory->u8( lv_dict ).
      DATA(lv_header_size) = 1 + lv_num_seps + 1 + 2.
      DATA(lv_entries) = lv_dict + lv_header_size.

      " First dictionary entry should have encoded text
      lo_text->decode(
        EXPORTING iv_addr = lv_entries
        IMPORTING ev_text = lv_text ev_len = lv_len ).

      cl_abap_unit_assert=>assert_not_initial(
        act = lv_len
        msg = 'Should decode some bytes from dictionary' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_decode_multiline.
    " Test decoding Z-string with newlines (multi-line text)
    " Newlines in Z-machine: A2 shift (z-char 5), then z-char 7 (index 1 in A2)
    "
    " Encode "a^b" where ^ is newline:
    "   'a' = z-char 6 (A0 index 0)
    "   shift to A2 = z-char 5
    "   newline (A2 index 1) = z-char 7
    "   'b' = z-char 7 (A0 index 1)
    "   padding = z-char 5
    "   padding = z-char 5
    "
    " Pack into words:
    "   Word1: (6 << 10) | (5 << 5) | 7 = 6144 + 160 + 7 = 6311
    "   Word2: 0x8000 | (7 << 10) | (5 << 5) | 5 = 32768 + 7168 + 160 + 5 = 40101
    "
    " Build header (64 bytes) + z-string at offset 64
    DATA lv_header TYPE xstring.
    DATA lv_zstring TYPE xstring.
    DATA lv_data TYPE xstring.

    " Create minimal memory with version 3 header (64 bytes of zeros with version)
    lv_header = '03'.  " Version 3 at byte 0
    DO 63 TIMES.
      lv_header = lv_header && '00'.
    ENDDO.

    " Word1 = 6311 = 0x18A7, Word2 = 40101 = 0x9CA5
    lv_zstring = '18A79CA5'.

    lv_data = lv_header && lv_zstring.

    DATA(lo_mem) = NEW zcl_ork_00_memory( lv_data ).
    DATA(lo_text) = NEW zcl_ork_00_text( lo_mem ).

    DATA lv_text TYPE string.
    DATA lv_len TYPE i.

    lo_text->decode(
      EXPORTING iv_addr = 64
      IMPORTING ev_text = lv_text ev_len = lv_len ).

    " Should contain newline between 'a' and 'b'
    cl_abap_unit_assert=>assert_equals(
      act = lv_len
      exp = 4
      msg = 'Should consume 4 bytes (2 words)' ).

    " Verify newline is present
    DATA(lv_has_newline) = xsdbool( lv_text CS cl_abap_char_utilities=>newline ).
    cl_abap_unit_assert=>assert_true(
      act = lv_has_newline
      msg = 'Decoded text should contain a newline character' ).

    " Split and check parts
    SPLIT lv_text AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_lines).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_lines )
      exp = 2
      msg = 'Should have 2 lines after split' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_lines[ 1 ]
      exp = 'a'
      msg = 'First line should be "a"' ).

    " Second line starts with 'b' (may have trailing padding chars)
    cl_abap_unit_assert=>assert_char_cp(
      act = lt_lines[ 2 ]
      exp = 'b*'
      msg = 'Second line should start with "b"' ).
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Z-Machine Text - Direct port from z3_minimal.py
*& Z-string encoding and decoding
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_text DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    " Alphabets (Z-chars 6-31 map to indices 0-25)
    CONSTANTS c_a0 TYPE string VALUE 'abcdefghijklmnopqrstuvwxyz'.
    CONSTANTS c_a1 TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    CONSTANTS c_a2 TYPE string VALUE '  0123456789.,!?_#''"/\-:()'. " Note: index 1 = newline

    METHODS constructor IMPORTING io_mem TYPE REF TO zcl_ork_00_memory.

    " Decode Z-string at address, returns text and bytes consumed
    METHODS decode
      IMPORTING iv_addr TYPE i
      EXPORTING ev_text TYPE string
                ev_len  TYPE i.

    " Encode text for dictionary lookup (V3: 6 chars -> 2 words)
    METHODS encode_for_dict
      IMPORTING iv_text TYPE string
      EXPORTING ev_word1 TYPE i
                ev_word2 TYPE i.

  PRIVATE SECTION.
    DATA mo_mem TYPE REF TO zcl_ork_00_memory.

    METHODS char_at
      IMPORTING iv_str TYPE string iv_idx TYPE i
      RETURNING VALUE(rv_char) TYPE string.
ENDCLASS.

CLASS zcl_ork_00_text IMPLEMENTATION.

  METHOD constructor.
    mo_mem = io_mem.
  ENDMETHOD.

  METHOD char_at.
    " Get character at index from string (0-based)
    IF iv_idx >= 0 AND iv_idx < strlen( iv_str ).
      rv_char = iv_str+iv_idx(1).
    ELSE.
      rv_char = ''.
    ENDIF.
  ENDMETHOD.

  METHOD decode.
    " Decode Z-string at address, returns (text, bytes_consumed)
    " State machine for 10-bit ZSCII:
    "   lv_zscii_hi = -1: Normal state
    "   lv_zscii_hi = -2: Waiting for high bits (just saw zc=6 in A2)
    "   lv_zscii_hi >= 0: Have high bits, waiting for low bits
    DATA: lv_addr      TYPE i,
          lv_word      TYPE i,
          lv_alphabet  TYPE i,
          lv_abbrev    TYPE i,
          lv_zscii_hi  TYPE i,
          lv_zscii     TYPE i,
          lv_start     TYPE i,
          lv_zc        TYPE i,
          lv_idx       TYPE i,
          lv_char      TYPE string,
          lv_abbr_addr TYPE i,
          lv_abbr_text TYPE string,
          lv_abbr_len  TYPE i.

    ev_text = ''.
    lv_addr = iv_addr.
    lv_start = iv_addr.
    lv_alphabet = 0.
    lv_abbrev = 0.
    lv_zscii_hi = -1.

    DO.
      lv_word = mo_mem->u16( lv_addr ).
      lv_addr = lv_addr + 2.

      " Extract 3 5-bit Z-characters from word
      DATA lt_zchars TYPE STANDARD TABLE OF i WITH EMPTY KEY.
      CLEAR lt_zchars.
      APPEND ( lv_word DIV 1024 ) MOD 32 TO lt_zchars.  " bits 14-10
      APPEND ( lv_word DIV 32 ) MOD 32 TO lt_zchars.    " bits 9-5
      APPEND lv_word MOD 32 TO lt_zchars.               " bits 4-0

      LOOP AT lt_zchars INTO lv_zc.
        IF lv_abbrev > 0.
          " Abbreviation: lookup and expand
          DATA(lv_abbr_num) = ( lv_abbrev - 1 ) * 32 + lv_zc.
          lv_abbr_addr = mo_mem->u16( mo_mem->abbrev_addr + lv_abbr_num * 2 ) * 2.
          decode( EXPORTING iv_addr = lv_abbr_addr
                  IMPORTING ev_text = lv_abbr_text ev_len = lv_abbr_len ).
          ev_text = ev_text && lv_abbr_text.
          lv_abbrev = 0.

        ELSEIF lv_zscii_hi >= 0.
          " Second half of 10-bit ZSCII (we have high bits, this is low bits)
          lv_zscii = lv_zscii_hi * 32 + lv_zc.
          IF lv_zscii >= 32 AND lv_zscii <= 126.
            ev_text = ev_text && cl_abap_conv_in_ce=>uccpi( lv_zscii ).
          ELSEIF lv_zscii = 13.
            ev_text = ev_text && cl_abap_char_utilities=>newline.
          ENDIF.
          lv_zscii_hi = -1.

        ELSEIF lv_zscii_hi = -2.
          " First half of 10-bit ZSCII (this z-char is the high bits)
          lv_zscii_hi = lv_zc.

        ELSEIF lv_zc = 0.
          ev_text = ev_text && ` `.

        ELSEIF lv_zc >= 1 AND lv_zc <= 3.
          " Abbreviation marker
          lv_abbrev = lv_zc.

        ELSEIF lv_zc = 4.
          " Shift to A1
          lv_alphabet = 1.

        ELSEIF lv_zc = 5.
          " Shift to A2
          lv_alphabet = 2.

        ELSEIF lv_zc = 6 AND lv_alphabet = 2.
          " 10-bit ZSCII follows - next two z-chars form a 10-bit value
          lv_zscii_hi = -2.  " Signal waiting for high bits
          lv_alphabet = 0.

        ELSE.
          " Regular character (zchars 6-31 -> index 0-25)
          lv_idx = lv_zc - 6.
          IF lv_idx >= 0 AND lv_idx < 26.
            CASE lv_alphabet.
              WHEN 0.
                lv_char = char_at( iv_str = c_a0 iv_idx = lv_idx ).
              WHEN 1.
                lv_char = char_at( iv_str = c_a1 iv_idx = lv_idx ).
              WHEN 2.
                " A2 index 1 is ^ which means newline
                IF lv_idx = 1.
                  lv_char = cl_abap_char_utilities=>newline.
                ELSE.
                  lv_char = char_at( iv_str = c_a2 iv_idx = lv_idx ).
                ENDIF.
            ENDCASE.
            ev_text = ev_text && lv_char.
          ENDIF.
          lv_alphabet = 0.  " Reset shift
        ENDIF.
      ENDLOOP.

      " Check end bit (bit 15)
      IF lv_word >= 32768.
        EXIT.
      ENDIF.

      " Safety limit
      IF lv_addr - lv_start > 1000.
        EXIT.
      ENDIF.
    ENDDO.

    ev_len = lv_addr - lv_start.
  ENDMETHOD.

  METHOD encode_for_dict.
    " Encode text for dictionary lookup (V3: 6 chars -> 2 words)
    DATA: lv_text TYPE string,
          lv_len  TYPE i,
          lv_ch   TYPE string,
          lv_z1   TYPE i, lv_z2 TYPE i, lv_z3 TYPE i,
          lv_z4   TYPE i, lv_z5 TYPE i, lv_z6 TYPE i.

    " Lowercase and take max 6 chars
    lv_text = iv_text.
    TRANSLATE lv_text TO LOWER CASE.
    lv_len = strlen( lv_text ).
    IF lv_len > 6.
      lv_len = 6.
    ENDIF.

    " Convert each char to z-char (pad with 5)
    lv_z1 = 5. lv_z2 = 5. lv_z3 = 5. lv_z4 = 5. lv_z5 = 5. lv_z6 = 5.

    DATA lv_pos TYPE i.

    IF lv_len >= 1.
      lv_ch = lv_text+0(1).
      " Find in A0 (a-z)
      lv_pos = 0.
      WHILE lv_pos < 26.
        IF char_at( iv_str = c_a0 iv_idx = lv_pos ) = lv_ch.
          lv_z1 = lv_pos + 6.
          EXIT.
        ENDIF.
        lv_pos = lv_pos + 1.
      ENDWHILE.
    ENDIF.

    IF lv_len >= 2.
      lv_ch = lv_text+1(1).
      lv_pos = 0.
      WHILE lv_pos < 26.
        IF char_at( iv_str = c_a0 iv_idx = lv_pos ) = lv_ch.
          lv_z2 = lv_pos + 6.
          EXIT.
        ENDIF.
        lv_pos = lv_pos + 1.
      ENDWHILE.
    ENDIF.

    IF lv_len >= 3.
      lv_ch = lv_text+2(1).
      lv_pos = 0.
      WHILE lv_pos < 26.
        IF char_at( iv_str = c_a0 iv_idx = lv_pos ) = lv_ch.
          lv_z3 = lv_pos + 6.
          EXIT.
        ENDIF.
        lv_pos = lv_pos + 1.
      ENDWHILE.
    ENDIF.

    IF lv_len >= 4.
      lv_ch = lv_text+3(1).
      lv_pos = 0.
      WHILE lv_pos < 26.
        IF char_at( iv_str = c_a0 iv_idx = lv_pos ) = lv_ch.
          lv_z4 = lv_pos + 6.
          EXIT.
        ENDIF.
        lv_pos = lv_pos + 1.
      ENDWHILE.
    ENDIF.

    IF lv_len >= 5.
      lv_ch = lv_text+4(1).
      lv_pos = 0.
      WHILE lv_pos < 26.
        IF char_at( iv_str = c_a0 iv_idx = lv_pos ) = lv_ch.
          lv_z5 = lv_pos + 6.
          EXIT.
        ENDIF.
        lv_pos = lv_pos + 1.
      ENDWHILE.
    ENDIF.

    IF lv_len >= 6.
      lv_ch = lv_text+5(1).
      lv_pos = 0.
      WHILE lv_pos < 26.
        IF char_at( iv_str = c_a0 iv_idx = lv_pos ) = lv_ch.
          lv_z6 = lv_pos + 6.
          EXIT.
        ENDIF.
        lv_pos = lv_pos + 1.
      ENDWHILE.
    ENDIF.

    " Pack into 2 words: (z1 << 10) | (z2 << 5) | z3
    ev_word1 = lv_z1 * 1024 + lv_z2 * 32 + lv_z3.
    " Second word has end bit: 0x8000 | (z4 << 10) | (z5 << 5) | z6
    ev_word2 = 32768 + lv_z4 * 1024 + lv_z5 * 32 + lv_z6.
  ENDMETHOD.

ENDCLASS.

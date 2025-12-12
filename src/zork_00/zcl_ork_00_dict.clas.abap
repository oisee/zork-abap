*&---------------------------------------------------------------------*
*& Z-Machine Dictionary - Direct port from z3_minimal.py
*& Dictionary lookup and tokenization
*&---------------------------------------------------------------------*
CLASS zcl_ork_00_dict DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_mem  TYPE REF TO zcl_ork_00_memory
                io_text TYPE REF TO zcl_ork_00_text.

    " Find word in dictionary, returns address or 0
    METHODS lookup
      IMPORTING iv_word TYPE string
      RETURNING VALUE(rv_addr) TYPE i.

    " Tokenize input text into parse buffer
    METHODS tokenize
      IMPORTING iv_text      TYPE string
                iv_text_buf  TYPE i
                iv_parse_buf TYPE i.

  PRIVATE SECTION.
    DATA mo_mem  TYPE REF TO zcl_ork_00_memory.
    DATA mo_text TYPE REF TO zcl_ork_00_text.

    " Parsed dictionary header
    DATA mv_num_separators TYPE i.
    DATA mt_separators TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA mv_entry_length TYPE i.
    DATA mv_num_entries TYPE i.
    DATA mv_entries_addr TYPE i.

    METHODS parse_header.

    METHODS is_separator
      IMPORTING iv_char TYPE i
      RETURNING VALUE(rv_is) TYPE abap_bool.
ENDCLASS.

CLASS zcl_ork_00_dict IMPLEMENTATION.

  METHOD constructor.
    mo_mem = io_mem.
    mo_text = io_text.
    parse_header( ).
  ENDMETHOD.

  METHOD parse_header.
    " Parse dictionary header
    DATA(lv_base) = mo_mem->dict_addr.

    mv_num_separators = mo_mem->u8( lv_base ).

    " Read separators
    CLEAR mt_separators.
    DATA lv_i TYPE i.
    lv_i = 0.
    WHILE lv_i < mv_num_separators.
      APPEND mo_mem->u8( lv_base + 1 + lv_i ) TO mt_separators.
      lv_i = lv_i + 1.
    ENDWHILE.

    DATA(lv_header_size) = 1 + mv_num_separators.
    mv_entry_length = mo_mem->u8( lv_base + lv_header_size ).
    mv_num_entries = mo_mem->u16( lv_base + lv_header_size + 1 ).
    mv_entries_addr = lv_base + lv_header_size + 3.

    " Handle negative entry count (unsorted dictionary)
    IF mv_num_entries >= 32768.
      mv_num_entries = 65536 - mv_num_entries.
    ENDIF.
  ENDMETHOD.

  METHOD is_separator.
    rv_is = abap_false.
    LOOP AT mt_separators INTO DATA(lv_sep).
      IF lv_sep = iv_char.
        rv_is = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD lookup.
    " Find word in dictionary using binary search
    DATA: lv_word1 TYPE i,
          lv_word2 TYPE i,
          lv_lo    TYPE i,
          lv_hi    TYPE i,
          lv_mid   TYPE i,
          lv_addr  TYPE i,
          lv_e1    TYPE i,
          lv_e2    TYPE i.

    " Encode word
    mo_text->encode_for_dict(
      EXPORTING iv_text = iv_word
      IMPORTING ev_word1 = lv_word1 ev_word2 = lv_word2 ).

    " Binary search
    lv_lo = 0.
    lv_hi = mv_num_entries - 1.

    WHILE lv_lo <= lv_hi.
      lv_mid = ( lv_lo + lv_hi ) DIV 2.
      lv_addr = mv_entries_addr + lv_mid * mv_entry_length.

      " Read entry words
      lv_e1 = mo_mem->u16( lv_addr ).
      lv_e2 = mo_mem->u16( lv_addr + 2 ).

      " Compare (2 words = 4 bytes)
      IF lv_word1 = lv_e1 AND lv_word2 = lv_e2.
        rv_addr = lv_addr.
        RETURN.
      ELSEIF lv_word1 < lv_e1 OR ( lv_word1 = lv_e1 AND lv_word2 < lv_e2 ).
        lv_hi = lv_mid - 1.
      ELSE.
        lv_lo = lv_mid + 1.
      ENDIF.
    ENDWHILE.

    rv_addr = 0.
  ENDMETHOD.

  METHOD tokenize.
    " Tokenize input text into parse buffer
    DATA: lv_max_words TYPE i,
          lv_num_words TYPE i,
          lv_i         TYPE i,
          lv_len       TYPE i,
          lv_ch        TYPE i,
          lv_in_word   TYPE abap_bool,
          lv_word_start TYPE i,
          lv_word_len  TYPE i,
          lv_word      TYPE string,
          lv_dict_addr TYPE i,
          lv_parse_addr TYPE i.

    lv_max_words = mo_mem->u8( iv_parse_buf ).
    IF lv_max_words = 0.
      RETURN.
    ENDIF.

    lv_num_words = 0.
    lv_len = strlen( iv_text ).
    lv_i = 0.
    lv_in_word = abap_false.
    lv_parse_addr = iv_parse_buf + 2.

    WHILE lv_i < lv_len AND lv_num_words < lv_max_words.
      " Get character code
      IF lv_i < lv_len.
        DATA(lv_char) = iv_text+lv_i(1).
        " Simple ASCII conversion
        lv_ch = 0.
        CASE lv_char.
          WHEN ` `. lv_ch = 32.
          WHEN `a`. lv_ch = 97.  WHEN `b`. lv_ch = 98.  WHEN `c`. lv_ch = 99.
          WHEN `d`. lv_ch = 100. WHEN `e`. lv_ch = 101. WHEN `f`. lv_ch = 102.
          WHEN `g`. lv_ch = 103. WHEN `h`. lv_ch = 104. WHEN `i`. lv_ch = 105.
          WHEN `j`. lv_ch = 106. WHEN `k`. lv_ch = 107. WHEN `l`. lv_ch = 108.
          WHEN `m`. lv_ch = 109. WHEN `n`. lv_ch = 110. WHEN `o`. lv_ch = 111.
          WHEN `p`. lv_ch = 112. WHEN `q`. lv_ch = 113. WHEN `r`. lv_ch = 114.
          WHEN `s`. lv_ch = 115. WHEN `t`. lv_ch = 116. WHEN `u`. lv_ch = 117.
          WHEN `v`. lv_ch = 118. WHEN `w`. lv_ch = 119. WHEN `x`. lv_ch = 120.
          WHEN `y`. lv_ch = 121. WHEN `z`. lv_ch = 122.
          WHEN `,`. lv_ch = 44.  WHEN `.`. lv_ch = 46.  WHEN `"`. lv_ch = 34.
          WHEN OTHERS. lv_ch = 32.
        ENDCASE.
      ELSE.
        lv_ch = 32.
      ENDIF.

      " Check if space or separator
      DATA(lv_is_sep) = abap_false.
      IF lv_ch = 32.
        lv_is_sep = abap_true.
      ELSE.
        lv_is_sep = is_separator( lv_ch ).
      ENDIF.

      IF lv_is_sep = abap_true.
        " End current word if any
        IF lv_in_word = abap_true.
          lv_word_len = lv_i - lv_word_start.
          IF lv_word_len > 0.
            lv_word = iv_text+lv_word_start(lv_word_len).
            lv_dict_addr = lookup( lv_word ).

            " Write parse entry: addr(2) + len(1) + pos(1)
            mo_mem->w16( iv_addr = lv_parse_addr iv_val = lv_dict_addr ).
            mo_mem->w8( iv_addr = lv_parse_addr + 2 iv_val = lv_word_len ).
            mo_mem->w8( iv_addr = lv_parse_addr + 3 iv_val = lv_word_start + 1 ).
            lv_parse_addr = lv_parse_addr + 4.
            lv_num_words = lv_num_words + 1.
          ENDIF.
          lv_in_word = abap_false.
        ENDIF.

        " If separator (not space), treat as separate token
        IF lv_ch <> 32 AND lv_num_words < lv_max_words.
          lv_word = lv_char.
          lv_dict_addr = lookup( lv_word ).

          mo_mem->w16( iv_addr = lv_parse_addr iv_val = lv_dict_addr ).
          mo_mem->w8( iv_addr = lv_parse_addr + 2 iv_val = 1 ).
          mo_mem->w8( iv_addr = lv_parse_addr + 3 iv_val = lv_i + 1 ).
          lv_parse_addr = lv_parse_addr + 4.
          lv_num_words = lv_num_words + 1.
        ENDIF.
      ELSEIF lv_in_word = abap_false.
        " Regular character - start or continue word
        lv_word_start = lv_i.
        lv_in_word = abap_true.
      ENDIF.

      lv_i = lv_i + 1.
    ENDWHILE.

    " Handle final word
    IF lv_in_word = abap_true AND lv_num_words < lv_max_words.
      lv_word_len = lv_i - lv_word_start.
      IF lv_word_len > 0.
        lv_word = iv_text+lv_word_start(lv_word_len).
        lv_dict_addr = lookup( lv_word ).

        mo_mem->w16( iv_addr = lv_parse_addr iv_val = lv_dict_addr ).
        mo_mem->w8( iv_addr = lv_parse_addr + 2 iv_val = lv_word_len ).
        mo_mem->w8( iv_addr = lv_parse_addr + 3 iv_val = lv_word_start + 1 ).
        lv_num_words = lv_num_words + 1.
      ENDIF.
    ENDIF.

    " Write word count
    mo_mem->w8( iv_addr = iv_parse_buf + 1 iv_val = lv_num_words ).
  ENDMETHOD.

ENDCLASS.

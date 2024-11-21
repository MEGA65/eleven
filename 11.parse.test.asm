; ------
; MACROS
; ------
!macro SET_CURSOR_POS .col, .row {
  clc
  ldy #.col
  ldx #.row
  jsr $fff0
}

!macro PRESS_ANY_KEY {
-:
  jsr $ffe4
  beq -
}

!macro FORCE_ADD_VAR_TO_VARTABLE .name, .type {
  +FORCE_ADD_VAR_TO_VARTABLE_NO_INC .name, .type
  +INCREMENT_ELCNT_OF_TY
}

!macro FORCE_ADD_VAR_TO_VARTABLE_NO_INC .name, .type {
  +ASSIGN_STRING_PTR_TO_IMM var_name, .name
  +ASSIGN_U8V_EQ_IMM ty, .type
  jsr add_varname_to_vartable
}

!macro STR_MATCH .str1, .str2 {
  +ASSIGN_U16V_EQ_ADDR tmp_ptr, .str1
  +ASSIGN_U16V_EQ_ADDR s_ptr, .str2
  jsr cmp_tmp_ptr_to_s_str
}

!macro STR_MATCH_PTR_TO_STR .ptr, .str {
  +ASSIGN_U16V_EQ_U16V tmp_ptr, .ptr
  +ASSIGN_U16V_EQ_ADDR s_ptr, .str
  jsr cmp_tmp_ptr_to_s_str
}

!macro U16_MATCH .uint1, .uint2 {
  lda .uint1
  cmp .uint2
  bne +
  lda .uint1+1
  cmp .uint2+1
  bne +

  clc
  bra ++

+:
  sec
++:
}

!macro FAIL_REASON .txt {
    lda #$00
    sta parser_error
    sta cur_line_len
    +ASSIGN_U16V_EQ_ADDR s_ptr, parser_error
    jsr append_inline_text_to_str
!pet $0d,150,$09,.txt,$05,$00

  lda #$01
  sta fail_reason_flag

  sec
}


passed_cnt:
!byte $00
failed_cnt:
!byte $00
total_cnt:
!byte $00
row_cnt:
!byte $00
fail_reason_flag:
!byte $00

run_tests:
  lda #$00
  sta passed_cnt
  sta failed_cnt
  sta total_cnt
  sta row_cnt

  +ASSIGN_U16V_EQ_ADDR TESTPTR, test_array

  ; lower-case
  +PRINT_CHR $0e

  ; 80x50
  +PRINT_CHR 27
  +PRINT_CHR '5'

  lda #$00
  sta $d020
  sta $d021

  jsr set_font_a

  +PRINT_CHR 147  ; clear-screen
  +PRINT_CHR $05  ; white-text
  +PRINT_INLINE_CR "^\"11.parse\""
  +PRINT_INLINE_CR "---------------------"
  +PRINT_INLINE_CR "'11.parse' test suite"
  +PRINT_INLINE_CR "---------------------"

@loop_next_page:
  +ASSIGN_U8V_EQ_IMM row_cnt, $04
  +SET_CURSOR_POS 0, 4
  ; + @ = clear text from cursor-pos to end of screen
  +PRINT_CHR 27 ; escape
  +PRINT_CHR '@' 

@loop_next_test:
  lda #$00
  sta fail_reason_flag

  ldy #$00
  lda (TESTPTR),y
  iny
  ora (TESTPTR),y
  lbeq @bail_out  ; if word pointer is zero, then bail out

  inc total_cnt

  +PRINT_CHR $05  ; white
  +PRINT_INLINE "[    ] "

  +PRINT_PPSTR TESTPTR

  ; go back to start of line
  +PRINT_CHR 27 ; escape
  +PRINT_CHR 'J'

  inw TESTPTR
  inw TESTPTR

  +ASSIGN_U16V_EQ_DEREF_U16V tmp_ptr, TESTPTR

  jsr (tmp_ptr)  ; test__read_next_line
  bcc +

  inc failed_cnt
  jsr print_inline_text
!pet $05, "[", $1c, "FAIL", $05, "] ",$00

  lda fail_reason_flag
  beq @skip_fail_reason

    +ASSIGN_U16V_EQ_ADDR ret_ptr_lo, parser_error

    inc row_cnt
    jsr print_text

@skip_fail_reason:
  bra ++

+:
  inc passed_cnt
  jsr print_inline_text
!pet $05, "[", $1e, "PASS", $05, "] ",$00

++:
  inw TESTPTR
  inw TESTPTR

  +PRINT_CHR $0d

  inc row_cnt
  +CMP_U8V_TO_IMM row_cnt, 47
  bcc @skip_next_page_indicator
    +PRINT_CHR 154  ; light blue
    +PRINT_INLINE "[Press any key to continue...]"
    +PRESS_ANY_KEY
    jmp @loop_next_page
@skip_next_page_indicator:

  jmp @loop_next_test

@bail_out:
  +PRINT_INLINE_CR "---------------------"
  +PRINT_INLINE "TOTAL TESTS: "
  +PRINT_U8V total_cnt
  +PRINT_CHR $0d

  +PRINT_CHR $1e  ; green
  +PRINT_INLINE "TOTAL PASSED: "
  +PRINT_U8V passed_cnt
  +PRINT_CHR $0d

  +PRINT_CHR $1c  ; red
  +PRINT_INLINE "TOTAL FAILED: "
  +PRINT_U8V failed_cnt
  +PRINT_CHR $0d
  +PRINT_CHR $05  ; white

  +PRINT_INLINE_CR "---------------------"
  
  rts


; -----
; TESTS
; -----

;-------------------
test__read_next_line:
;-------------------
  jsr prepare_SRCPTR

; feed a dummy string into #8030000
  jsr feed_dummy_string
_test_str:
!pet "#output \"fishy\"", $00

  jsr read_next_line

  +STR_MATCH _test_str, cur_src_line
  rts


;------------------------------
test__single_quote_comment_trim:
;------------------------------
  jsr prepare_SRCPTR
  jsr feed_dummy_string
!pet "test trim   ' comment", $00
  bra +
@_expected:
!pet "test trim   ", $00

+:
  jsr read_next_line

  jsr single_quote_comment_trim

  +STR_MATCH_TO_SPTR @_expected
  rts


;----------------------
test__strip_tr_from_end:
;----------------------
  jsr prepare_SRCPTR
  jsr feed_dummy_string
!pet "test trim end   ", $00
  bra +
@_expected:
!pet "test trim end", $00

+:
  jsr read_next_line

  jsr strip_tr_from_end

  +STR_MATCH_TO_SPTR @_expected
  rts


;-----------------------
test__add_to_label_table:
;-----------------------
  lda #$00
  sta verbose
  jsr prepare_SRCPTR

  ; SCENARIO1: 1st label
  ; --------------------
  jsr feed_dummy_string
!pet ".testlabel1", $00
  bra +
@_expected:
!pet "testlabel1", $00

+:
  ; set a dummy destination line number
  lda #$05
  sta dest_lineno
  lda #$00
  sta dest_lineno+1

  jsr read_next_line

  jsr add_to_label_table

  ldz #$00  ; index into label_name[] array
  jsr get_label_name_item_at_idx

  +STR_MATCH_TO_SPTR @_expected
  bcc +
  +FAIL_REASON "SCEN1: 1st label"
  rts

  ; - - - - -

+:
  ; SCENARIO2: 1st label lineno
  ; ---------------------------
  ldz #$00
  jsr get_label_lineno_at_idx
  cpx #$06    ; NOTE: oddly, the line number recorded is +1 ($06 instead of $05)
  cpy #$00
  bne @s2_failed
  bra @s2_passed
@s2_failed:
  +FAIL_REASON "SCEN2: 1st label lineno"
  rts

  ; - - - - -
@s2_passed:

+:
  ; SCENARIO3: 2nd label
  ; --------------------
  jsr feed_dummy_string
!pet ".testlabel2", $00
  bra +
@_expected2:
!pet "testlabel2", $00
+:

  ; set a dummy destination line number
  lda #$0a
  sta dest_lineno
  lda #$00
  sta dest_lineno+1
  jsr read_next_line

  jsr add_to_label_table

  ldz #$01
  jsr get_label_name_item_at_idx

  +STR_MATCH_TO_SPTR @_expected2
  bcc +
  +FAIL_REASON "SCEN3: 2nd label"
  rts

+:
  ; SCENARIO4: 2nd label lineno
  ; ---------------------------
  ldz #$01
  jsr get_label_lineno_at_idx
  cpx #$0b    ; NOTE: oddly, the line number recorded is +1 ($0b instead of $0a)
  bne @s4_failed
  cpy #$00
  bne @s4_failed
  bra @s4_passed
@s4_failed:
  +FAIL_REASON "SCEN4: 2nd label lineno"
  rts

@s4_passed:
  clc
  rts


;----------------------------
test__add_varname_to_vartable:
;----------------------------
  ; SCEN1: add a string var
  ; -----------------------
  +FORCE_ADD_VAR_TO_VARTABLE_NO_INC "fishy$", TYP_STR

  jsr add_varname_to_vartable

  +SET_IS_PTR_TO_VARTABLE_AT_TY_IDX
  +UPDATE_IS_PTR_TO_LATEST_ELEMENT_COUNT_IDX
  
  +ASSIGN_U16V_EQ_DEREF_U16V s_ptr, is_ptr

  +CMP_S_PTR_TO_IMM "fishy$"
  bcc +
  +FAIL_REASON "SCEN1: string var not found in var table"
  rts
+:

  ; SCEN2: add an int var
  ; ----------------------
  +ASSIGN_STRING_PTR_TO_IMM var_name, "dishy%"
  +ASSIGN_U8V_EQ_IMM ty, TYP_INT

  jsr add_varname_to_vartable

  +SET_IS_PTR_TO_VARTABLE_AT_TY_IDX
  +UPDATE_IS_PTR_TO_LATEST_ELEMENT_COUNT_IDX
  
  ldy #$00
  lda (is_ptr),y
  sta s_ptr
  iny
  lda (is_ptr),y
  sta s_ptr+1

  +CMP_S_PTR_TO_IMM "dishy%"
  bcc +
  +FAIL_REASON "SCEN2: int var not found in var table"
  rts
+:
  clc
  rts


;---------------------
test__generate_varname:
;---------------------
  ; SCEN1: 1 = 'b'
  ; --------------
  +ASSIGN_U8V_EQ_IMM elidx, $01

  jsr generate_varname

  +CMP_GENVNAME_TO 'b', $00
  bcc +
  +FAIL_REASON "SCEN1: elidx=1 expected var='b'"
  rts
+:

  ; SCEN2: 26 = 'aa'
  ; --------------
  +ASSIGN_U8V_EQ_IMM elidx, 26

  jsr generate_varname

  +CMP_GENVNAME_TO 'a', 'a'
  bcc +
  +FAIL_REASON "SCEN2: elidx=26 expected var='aa'"
  rts
+:

  clc
  rts

;-----------------------
test__parse_declared_var:
;-----------------------
  +ASSIGN_U8V_EQ_IMM define_flag, $01
  +SET_STRING f_str, "FISHY = 1"
  +ASSIGN_U16V_EQ_ADDR var_name, f_str

  jsr parse_declared_var

  ; SCEN1: check define var added
  ; - - - - - -
  ; +ASSIGN_U8V_EQ_IMM define_flag, $00
  +SET_IS_PTR_TO_LAST_VARTABLE_ELEMENT_OF_TYPE TYP_DEF
  +ASSIGN_U16V_EQ_DEREF_U16V s_ptr, is_ptr

  +CMP_S_PTR_TO_IMM "FISHY"
  bcc +
  +FAIL_REASON "SCEN1: define var not found in var table"
  rts
+:

  ; SCEN2: check define value added
  ; - - - - - -
  lda #$00  ; I'm expecting it to be the 1st define element
  +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_A define_val

  +CMP_PPSTR_TO_IMM tmp_ptr, "1"
  bcc +
  +FAIL_REASON "SCEN2: define value not found in value table"
  rts
+:
  clc
  rts


;----------------------------
test__replace_vars_and_labels:
;----------------------------
  // NOTE: This test relies on the prior "test__parse_declared_var:" adding the
  // define "FISHY=1" into the define-val table already

  +SET_STRING cur_src_line, "a$(FISHY)"
  ; this will set s_ptr to point to it too

  jsr replace_vars_and_labels

  bra +
@expected:
!pet "a$(1)",$00
+:

  +ASSIGN_U16V_EQ_ADDR s_ptr, cur_src_line
  +STR_MATCH_TO_SPTR @expected
  bcc +
  +FAIL_REASON "s_ptr != 'a$(1)'"
  rts
+:
  
  clc
  rts


;----------------------------
test__check_token_for_subbing:
;----------------------------
  +FORCE_ADD_VAR_TO_VARTABLE "moo$", TYP_STR
  +SET_LSTRING cur_tok, "moo$"

  jsr check_token_for_subbing

  +CMP_S_PTR_TO_IMM "a$"
  bcc +
  +FAIL_REASON "SCEN2: expected cur_tok='a$'"
  rts
+:

  clc
  rts



;-----------------------
test__add_curtok_to_astr:
;-----------------------
  +SET_LSTRING a_str, "hello "
  +SET_LSTRING cur_tok, "world"

  jsr add_curtok_to_astr

  ; SCEN1: check new astr length
  +CMP_LSTR_LEN_TO_IMM a_str, 11
  beq +
  +FAIL_REASON "SCEN1: a_str has wrong length"
  rts

@expected:
!pet "hello world",$00

+:
  +STR_MATCH a_str+1, @expected
  bcc +

  +FAIL_REASON "SCEN2: a_str has wrong contents"
  rts

+:
  clc
  rts


;------------------------
test__add_curchar_to_astr:
;------------------------
  +SET_LSTRING a_str, "hell"
  +ASSIGN_U8V_EQ_IMM cur_char, 'o'

  jsr add_curchar_to_astr

  bra +
@expected:
!pet $05, "hello", $00

+:
  +STR_MATCH a_str, @expected

  rts


;------------------------
test__add_curchar_to_curtok:
;------------------------
  +SET_LSTRING cur_tok, "hell"

  lda #'o'
  sta cur_char

  jsr add_curchar_to_curtok

  bra +
@expected:
!pet $05, "hello", $00

+:
  +STR_MATCH cur_tok, @expected

  rts


;--------------------
test__dbl_quote_check:
;--------------------
  ; SCEN1: char is not a quote, so do nothing to a_str
  +ASSIGN_U8V_EQ_IMM cur_char, 'z'
  +SET_LSTRING a_str, "xy"
  +SET_LSTRING cur_tok, "token"
  +ASSIGN_U8V_EQ_IMM quote_flag, $00

  jsr dbl_quote_check

  ; check C=0 (not a char within (and including) pair of quotes)
  bcc +:
  +FAIL_REASON "SCEN1: expected C=0, but got C=1"
  rts

+:
  bra +
@expected:
!pet $02, "xy", $00
+:
  +STR_MATCH a_str, @expected
  bcc +:
  +FAIL_REASON "SCEN1: a_str should not have changed"
  rts
+:

  ; - - - - - -
  ; SCEN2: char is a starting quote, so a_str += cur_tok + ", cur_tok="", C=0
  +ASSIGN_U8V_EQ_IMM cur_char, '"'

  jsr dbl_quote_check

  bcs +
  +FAIL_REASON "SCEN2: expected C=1, but got C=0"
  rts

@expected2:
!pet $08, "xytoken\"", $00
+:

  +STR_MATCH a_str, @expected2
  bcc +:
  +FAIL_REASON "SCEN2: fail on a_str += cur_tok"
  rts
+:

  lda cur_tok
  beq +
  +FAIL_REASON "SCEN2: cur_tok is not empty"
  rts
+:

  ; - - - - - -
  ; SCEN3: char is not quote, but we are in quote mode
  ;        ...so a_str += cur_char, C=1
  +ASSIGN_U8V_EQ_IMM cur_char, 's'

  jsr dbl_quote_check

  bcs +
  +FAIL_REASON "SCEN3: expected C=1, but got C=0"
  rts

@expected3:
!pet $09, "xytoken\"s", $00
+:

  +STR_MATCH a_str, @expected3
  bcc +:
  +FAIL_REASON "SCEN3: fail on a_str += cur_char"
  rts
+:

  ; - - - - - -
  ; SCEN4: char is ending quote
  ;        ...so a_str += cur_char, C=1
  +ASSIGN_U8V_EQ_IMM cur_char, '"'

  jsr dbl_quote_check

  bcs +
  +FAIL_REASON "SCEN4: expected C=1, but got C=0"
  rts

@expected4:
!pet $0a, "xytoken\"s\"", $00
+:

  +STR_MATCH a_str, @expected4
  bcc +:
  +FAIL_REASON "SCEN4: fail on a_str += cur_char"
  rts
+:

  clc
  rts


;------------------------------
test__add_subbed_curtok_to_astr:
;------------------------------
  +SET_LSTRING a_str, "hello "
  +SET_LSTRING cur_tok, "moocow$"

  jsr add_subbed_curtok_to_astr

  ; SCEN1: no sub, check new astr length
  ; - - - - - - -
  +CMP_LSTR_LEN_TO_IMM a_str, 13
  beq +
  +FAIL_REASON "SCEN1: a_str has wrong length"
  rts
+:

  ; SCEN2: no sub, check astr contents
  ; - - - - - - -
  +ASSIGN_U16V_EQ_ADDR s_ptr, a_str
  inw s_ptr ; slip length byte
  +CMP_S_PTR_TO_IMM "hello moocow$"
  bcc +
  +FAIL_REASON "SCEN2: a_str has wrong contents"
  rts
+:

  ; SCEN3:
  +SET_LSTRING a_str, "hello "
  +FORCE_ADD_VAR_TO_VARTABLE "moocow$", TYP_STR
  +SET_LSTRING cur_tok, "moocow$"

  jsr add_subbed_curtok_to_astr
  
  ; SCEN3: subbed, check new astr length
  ; - - - - - - -
  +CMP_LSTR_LEN_TO_IMM a_str, $08
  beq +
  +FAIL_REASON "SCEN3: a_str has wrong length"
  rts
+:

  ; SCEN4: subbed, check astr contents
  ; - - - - - - -
  +ASSIGN_U16V_EQ_ADDR s_ptr, a_str
  inw s_ptr ; slip length byte
  +CMP_S_PTR_TO_IMM "hello b$"
  bcc +
  +FAIL_REASON "SCEN4: a_str has wrong contents"
  rts
+:
  clc
  rts


;--------------------
test__parse_arguments:
;--------------------
  +SET_STRING f_str, "  a=1  ,b=2  ,c = 3"
  ; this will set s_ptr to point to it too

  jsr parse_arguments

  bra +
@expected1:
!pet "a=1",$00
@expected2:
!pet "b=2",$00
@expected3:
!pet "c = 3",$00
+:

  +STR_MATCH_PTR_TO_STR args, @expected1
  bcc +:
  +FAIL_REASON "args[0] not as expected"
  rts
+:

  +STR_MATCH_PTR_TO_STR args+2, @expected2
  bcc +:
  +FAIL_REASON "args[1] not as expected"
  rts
+:

  +STR_MATCH_PTR_TO_STR args+4, @expected3
  bcc +:
  +FAIL_REASON "args[2] not as expected"
  rts
+:

  clc
  rts

;---------------------
test__add_trimmed_args:
;---------------------
  +ASSIGN_U16V_EQ_ADDR args, tempheap
  +ASSIGN_U8V_EQ_IMM arg_idx, $00
  +ASSIGN_U8V_EQ_IMM arg_cnt, $00

  +ASSIGN_U16V_EQ_ADDR s_ptr, tempheap
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
!pet "  a=1  ", $00

  bra +
@expected:
!pet "a=1",$00

+:
  
  jsr add_trimmed_arg

  ; +STR_MATCH args, @expected

  +STR_MATCH_PTR_TO_STR args, @expected

  bcc +:
  +FAIL_REASON "args[0] failed to trim as expected"
  rts
+:
  clc
  rts


;-----------------------------
test__declare_assignment_check:
;-----------------------------
  +ASSIGN_U8V_EQ_IMM equals_idx, $02

  +SET_STRING f_str, "a = 1"
  ; this will set s_ptr to point to it too

  +ASSIGN_U16V_EQ_ADDR var_name, f_str

  jsr declare_assignment_check
  
  bra +
@expected1:
!pet "a", $00

@expected2:
!pet "1", $00
+:

  +ASSIGN_U16V_EQ_U16V s_ptr, var_name
  +STR_MATCH_TO_SPTR @expected1
  bcc +
  +FAIL_REASON "var_name != 'a'"
  rts
+:

  +ASSIGN_U16V_EQ_U16V s_ptr, value
  +STR_MATCH_TO_SPTR @expected2
  bcc +
  +FAIL_REASON "value != '1'"
  rts
+:

  clc
  rts


;----------------------------
test__declare_dimension_check:
;----------------------------
  +SET_STRING f_str, "fish$(10)"
  +ASSIGN_U16V_EQ_ADDR var_name, f_str

  +ASSIGN_U8V_EQ_IMM bkt_open_idx, $05
  +ASSIGN_U8V_EQ_IMM bkt_close_idx, $08

  jsr declare_dimension_check

  bra +
@expected1:
!pet "fish$",$00
@expected2:
!pet "10",$00
+:

  +ASSIGN_U16V_EQ_U16V s_ptr, var_name
  +STR_MATCH_TO_SPTR @expected1
  bcc +
  +FAIL_REASON "var_name != 'fish$'"
  rts
+:

  +ASSIGN_U16V_EQ_U16V s_ptr, dimension
  +STR_MATCH_TO_SPTR @expected2
  bcc +
  +FAIL_REASON "dimension != '10'"
  rts
+:
  
  clc
  rts


;-------------------------
test__decimal_number_check:
;-------------------------
  +ASSIGN_U16V_EQ_ADDR s_ptr, cur_tok

  ; SCEN1: cur_tok = "5"
  ; - - - - - - - - -
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
@expected1:
!pet $01, "5", $00  ; length-encoded in first byte

  jsr decimal_number_check

  +CMP_U8V_TO_IMM expecting_label, $00
  beq +
  +FAIL_REASON "SCEN1: expecting_label != 0"
  rts
+:

  +STR_MATCH_TO_SPTR @expected1
  bcc +
  +FAIL_REASON "SCEN1: s_ptr has not expected value"
  rts
+:

  ; SCEN2: cur_tok = "0"
  ; - - - - - - - - -
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
!pet $01, "0", $00  ; length-encoded in first byte

  jsr decimal_number_check

  +CMP_U8V_TO_IMM expecting_label, $00
  beq +
  +FAIL_REASON "SCEN2: expecting_label != 0"
  rts
@expected2:
!pet $01, ".", $00
+:

  +STR_MATCH_TO_SPTR @expected2
  bcc +
  +FAIL_REASON "SCEN2: s_ptr has not expected value"
  rts
+:

  clc
  rts


;------------------------------
test__check_mark_expected_label:
;------------------------------
  ; SCEN1: expecting label, but don't mark label (cur_tok should stay the same)
  ; - - - - - - -
  +ASSIGN_U8V_EQ_IMM expecting_label, $01
  +ASSIGN_U8V_EQ_IMM dont_mark_label, $01

  +ASSIGN_U16V_EQ_ADDR s_ptr, cur_tok
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
@expected1:
!pet $05, "dummy", $00  ; length-encoded in first byte

  jsr check_mark_expected_label

  +STR_MATCH_TO_SPTR @expected1
  bcc +
  +FAIL_REASON "SCEN1: s_ptr has not expected value"
  rts
@expected2:
!pet $09, "@", $7e, "dummy", "@", $7e, $00
+:

  ; SCEN2: expecting label, and we can mark label (cur_tok should add mk$ markers)
  ; - - - - - - -
  +ASSIGN_U8V_EQ_IMM expecting_label, $01
  +ASSIGN_U8V_EQ_IMM dont_mark_label, $00

  jsr check_mark_expected_label

  +STR_MATCH_TO_SPTR @expected2
  bcc +
  +FAIL_REASON "SCEN2: s_ptr has not expected value"
  rts
+:

  clc
  rts


;-----------------------
test__mark_cur_tok_label:
;-----------------------
  +ASSIGN_U16V_EQ_ADDR s_ptr, cur_tok

  ; SCEN1: cur_tok = "dummy", expecting markers added on both ends
  ; - - - - - - - - -
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
!pet $05, "dummy", $00  ; length-encoded in first byte

  jsr mark_cur_tok_label
  
  bra +
@expected:
!pet $09, "@", $7e, "dummy", "@", $7e, $00
+:

  +STR_MATCH_TO_SPTR @expected
  bcc +
  +FAIL_REASON "s_ptr has not expected value"
  rts
+:

  clc
  rts


;----------------------------
test__check_expect_label_next:
;----------------------------
  ; SCEN1: cur_tok = "gosub", so expect_label_next flag should be set
  ; - - - - - - - - -
  +ASSIGN_U8V_EQ_IMM expecting_label, $00
  +SET_LSTRING cur_tok, "gosub"

  jsr check_expect_label_next

  +CMP_U8V_TO_IMM expecting_label, $01
  beq +
  +FAIL_REASON "SCEN1: expecting_label == 1"
  rts
+:

  ; SCEN2: cur_tok = "blah", so expect_label_next flag should = 0
  ; - - - - - - - - -
  +ASSIGN_U8V_EQ_IMM expecting_label, $00
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
!pet $04, "blah", $00  ; length-encoded in first byte

  jsr check_expect_label_next

  +CMP_U8V_TO_IMM expecting_label, $00
  beq +
  +FAIL_REASON "SCEN2: expecting_label == 0"
  rts
+:

  clc
  rts


;-------------------------------
test__check_hex_and_binary_value:
;-------------------------------
  ; SCEN1: Valid hex
  ; - - - - - - - -
  +ASSIGN_U8V_EQ_IMM parser_error, $00
  +SET_LSTRING cur_tok, "$ABCD"

  jsr check_hex_and_binary_value

  ; confirm parser_error is still "" (input was valid hex)
  lda parser_error 
  beq + ; null-terminator?
  +FAIL_REASON "SCEN1: parser error found"
  rts
+:

  ; SCEN2: Valid hex (in lowercase)
  ; - - - - - - - -
  +SET_LSTRING cur_tok, "$abcd"

  jsr check_hex_and_binary_value

  lda parser_error 
  beq + ; null-terminator?
  +FAIL_REASON "SCEN2: parser error found"
  rts
+:

  ; SCEN3: Invalid hex
  ; - - - - - - - -
  +SET_LSTRING cur_tok, "$EFGH"

  jsr check_hex_and_binary_value

  lda parser_error 
  bne + ; null-terminator?
  +FAIL_REASON "SCEN3: expected parser error"
  rts
+:

  ; SCEN4: valid binary
  ; - - - - - - - -
  +ASSIGN_U8V_EQ_IMM parser_error, $00
  +SET_LSTRING cur_tok, "%0101"

  jsr check_hex_and_binary_value

  lda parser_error 
  beq + ; null-terminator?
  +FAIL_REASON "SCEN4: parser error found"
  rts
+:

  ; SCEN5: invalid binary
  ; - - - - - - - -
  +SET_LSTRING cur_tok, "%0101234"

  jsr check_hex_and_binary_value

  lda parser_error 
  bne + ; null-terminator?
  +FAIL_REASON "SCEN5: expected parser error"
  rts
+:

  clc
  rts


;--------------------------------
test__check_ignore_existing_vocab:
;--------------------------------
  ; SCEN1: not in vocab
  ; - - - - - - - -
  +SET_LSTRING cur_tok, "fishy"

  jsr check_ignore_existing_vocab

  bcc +
  +FAIL_REASON "SCEN1: wasn't expecting token in vocab"
  rts
+:

  ; SCEN2: in vocab
  ; - - - - - - - -
  +SET_LSTRING cur_tok, "tron"

  jsr check_ignore_existing_vocab

  bcs +
  +FAIL_REASON "SCEN2: was expecting token in vocab"
  rts
+:

  clc
  rts


;-------------------------------------
test__check_swap_vars_with_short_names:
;-------------------------------------
; NOTE: This test is presently relying on the prior
; test__add_varname_to_vartable: routine to add some
; vars in first.
;
; This isn't ideal, as I need to increment element counts in here (very dodgy)
; I can do things more cleanly later

  ; dodgy increment of element_count for integer
  ldx #TYP_INT
  inc element_cnt,x

  ; SCEN1:  Check 'ty' returns correctly
  ; -----
  +ASSIGN_U8V_EQ_IMM ty, $ff  ; dummy value (to assure it changes)
  +SET_LSTRING cur_tok, "dishy%"

  jsr check_swap_vars_with_short_names

  +CMP_U8V_TO_IMM ty, TYP_INT
  beq +
  +FAIL_REASON "SCEN1: expected ty=TYP_INT"
  rts
+:

  ; SCEN2: Check 'cur_tok' replaced
  ; -----
  +CMP_S_PTR_TO_IMM "a%"
  bcc +
  +FAIL_REASON "SCEN2: expected cur_tok='a%'"
  rts
+:

  clc
  rts


;------------------------------
test__add_define_value_to_table:
;------------------------------
  +ASSIGN_U8V_EQ_IMM define_flag, $01
  +ASSIGN_U8V_EQ_IMM ty, TYP_DEF
  +SET_STRING f_str, "1"
  +ASSIGN_U16V_EQ_ADDR value, f_str

  jsr add_define_value_to_table

  +SET_TMP_PTR_TO_DEFVALS_AT_ELCNT_IDX
  +ASSIGN_U16V_EQ_DEREF_U16V s_ptr, tmp_ptr

  +CMP_S_PTR_TO_IMM "1"
  bcc +
  +FAIL_REASON "string var not found in def-values table"
  rts
+:

  clc
  rts


;------------------------
test__check_defines_table:
;------------------------
  ; NOTE: This test presently expects a prior test to
  ; define "FISHY = 1"

  +SET_LSTRING cur_tok, "FISHY"

  jsr check_defines_table

  +CMP_S_PTR_TO_IMM "1"
  bcc +
  +FAIL_REASON "token 'FISHY' wasn't replaced with '1'"
  rts
+:

  clc
  rts


;----------------------
test__declare_s_ptr_var:
;----------------------
  ; SCEN1: check real var added
  ; - - - - - -
  +ASSIGN_U8V_EQ_IMM define_flag, $00
  +SET_STRING cur_src_line, "#declare TEST=123"

  jsr declare_s_ptr_var

  +SET_IS_PTR_TO_LAST_VARTABLE_ELEMENT_OF_TYPE TYP_REAL

  +CMP_PPSTR_TO_IMM is_ptr, "TEST"
  bcc +
  +FAIL_REASON "SCEN1: real var not found in var table"
  rts
+:

  ; SCEN2: check real value is in next line
  ; - - - - - -
  +CMP_STR_TO_IMM next_line, "a=123:"
  bcc +
  +FAIL_REASON "SCEN2: next_line string not correct"
  rts
+:

  clc
  rts


;-------------------------------------------
test__generate_dest_line_for_dimensioned_var:
;-------------------------------------------
  +FORCE_ADD_VAR_TO_VARTABLE_NO_INC "moo%", TYP_INT
  +ASSIGN_STRING_PTR_TO_IMM dimension, "10"
  +ASSIGN_U8V_EQ_IMM ty, TYP_INT
  +ASSIGN_U8V_EQ_IMM define_flag, $01
  +ASSIGN_U8V_EQ_IMM next_line, $00  ; null-term

  ; SCEN1: do nothing if define_flag is on
  ; - - - - - - -
  jsr generate_dest_line_for_dimensioned_var

  +CMP_U8V_TO_IMM next_line, $00
  beq +
    +FAIL_REASON "next_line wasn't empty"
    rts
+:

  ; SCEN2: prepare next_line if define_flag is off
  ; - - - - - - -
  +ASSIGN_U8V_EQ_IMM define_flag, $00

  jsr generate_dest_line_for_dimensioned_var

  +CMP_STR_TO_IMM next_line, "dim b%(10)"
  bcc +
    +FAIL_REASON "next_line not as expected"
    rts
+:

  clc
  rts


;----------------------------------------
test__generate_dest_line_for_assigned_var:
;----------------------------------------
  +FORCE_ADD_VAR_TO_VARTABLE_NO_INC "mybyte&", TYP_BYTE
  +ASSIGN_STRING_PTR_TO_IMM value, "10"
  +ASSIGN_U8V_EQ_IMM ty, TYP_BYTE
  +ASSIGN_U8V_EQ_IMM define_flag, $01
  +ASSIGN_U8V_EQ_IMM next_line, $00  ; null-term

  jsr generate_dest_line_for_assigned_var

  +ASSIGN_U8V_EQ_IMM define_flag, $00   ; so it doesn't surprise following tests
  +CMP_STR_TO_IMM next_line, "a&=10:"
  bcc +
    +FAIL_REASON "next_line not as expected"
    rts
+:

  clc
  rts


;---------------------------------
test__parse_preprocessor_directive:
;---------------------------------
  sec
  rts


;--------------------
test__set_output_file:
;--------------------
  +SET_STRING cur_src_line, "#output \"myfile\""

  jsr set_output_file

  +CMP_STR_TO_IMM outfile, "myfile"
  bcc +
    +FAIL_REASON "outfile not as expected"
    rts
+:

  clc
  rts

s_idx:
!byte 00

;---------------------------
test__read_in_struct_details:
;---------------------------
  +SET_STRING f_str, "#struct ENVTYPE name$, attack, decay, sustain"
  +ASSIGN_U16V_EQ_ADDR s_ptr, f_str

  jsr read_in_struct_details

  ; s_idx = struct_cnt - 1
  ldy struct_cnt
  dey
  sty s_idx

  +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_U8V struct_name, s_idx
  +CMP_PPSTR_TO_IMM tmp_ptr, "ENVTYPE"
  bcc +
    +FAIL_REASON "SCEN1: struct_name(idx) not as expected"
    rts
+:

  +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_U8V struct_vars, s_idx
  +CMP_PPSTR_TO_IMM tmp_ptr, "name$, attack, decay, sustain"
  bcc +
    +FAIL_REASON "SCEN2: struct_vars(idx) not as expected"
    rts
+:

  clc
  rts


;--------------------
test__read_next_token:
;--------------------
  +SET_STRING f_str, "ENVTYPE name$, attack, decay, sustain"
  +ASSIGN_U16V_EQ_ADDR s_ptr, f_str

  jsr read_next_token

  +ASSIGN_U16V_EQ_U16V orig_sptr, s_ptr ; back it up
  +CMP_PSTR_TO_IMM a_ptr, "ENVTYPE"
  bcc +
    +FAIL_REASON "SCEN1: struct name not as expected"
    rts
+:

  +ASSIGN_U16V_EQ_U16V s_ptr, orig_sptr ; back it up
  +CMP_PSTR_TO_IMM s_ptr, "name$, attack, decay, sustain"
  bcc +
    +FAIL_REASON "SCEN2: struct vars not as expected"
    rts
+:

  clc
  rts


;---------------------
test__is_s_ptr_defined:
;---------------------
  ; SCEN1: successfully find already defined token
  ; - - - - - - -
  +SET_STRING f_str, "FISHY"
  +ASSIGN_U16V_EQ_ADDR s_ptr, f_str

  jsr is_s_ptr_defined

  +CMP_U8V_TO_IMM inside_ifdef, $00   ; 0 = exists
  beq +
    +FAIL_REASON "SCEN1: expected 'FISHY' to be defined already"
    rts
+:

  ; SCEN2: assure we can't find a token not defined yet
  ; - - - - - - -
  +SET_STRING f_str, "NOT_DEFINED_YET_THINGIE"

  jsr is_s_ptr_defined

  +CMP_U8V_TO_IMM inside_ifdef, $01   ; 1 = doesn't exit
  beq +
    +FAIL_REASON "SCEN2: didn't expect undefined token to be found"
    rts
+:

  clc
  rts


;-------------------------------------
test__check_compulsory_next_line_cases:
;-------------------------------------
  sec
  rts


;---------------------------
test__parse_no_brackets_case:
;---------------------------
  +ASSIGN_U8V_EQ_IMM bkt_open_idx, $ff
  +SET_STRING f_str, "env"
  +ASSIGN_U16V_EQ_ADDR struct_obj_name, f_str
  +ASSIGN_U8V_EQ_IMM ridx, $00
  jsr assign_dummy_args

  jsr parse_no_brackets_case

  +CMP_PSTR_TO_IMM struct_obj_name, "env_name$"
  bcc +
  +FAIL_REASON "SCEN1: struct field name not correct"
  rts
+:

  +ASSIGN_ZPV_TO_DEREF_LATEST_VARTABLE_ELEMENT_OF_TYPE s_ptr, TYP_STR

  +CMP_S_PTR_TO_IMM "env_name$"
  bcc +
  +FAIL_REASON "SCEN2: struct field not found in var table"
  rts
+:

  clc
  rts


;--------------------------------------
test__gen_dimensioned_struct_field_name:
;--------------------------------------
  +ASSIGN_U8V_EQ_IMM bkt_open_idx, $04
  +ASSIGN_STRING_PTR_TO_IMM struct_obj_name, "envs(9)"
  +ASSIGN_U8V_EQ_IMM ridx, $00
  jsr assign_dummy_args

  jsr gen_dimensioned_struct_field_name

  +CMP_STR_TO_IMM f_str, "envs_name$(9)"
  bcc +
  +FAIL_REASON "generated field name not right"
  rts
+:

  clc
  rts


;------------------------
test__parse_brackets_case:
;------------------------
  +ASSIGN_U8V_EQ_IMM bkt_open_idx, $04
  +ASSIGN_STRING_PTR_TO_IMM struct_obj_name, "envs(9)"
  +ASSIGN_U8V_EQ_IMM ridx, $00
  jsr assign_dummy_args

  jsr parse_brackets_case

  +ASSIGN_ZPV_TO_DEREF_LATEST_VARTABLE_ELEMENT_OF_TYPE s_ptr, TYP_STR
  +CMP_S_PTR_TO_IMM "envs_name$"
  bcc +
  +FAIL_REASON "SCEN1: struct field not found in var table"
  rts
+:

  ; s_ptr = struct_fields[0]
  +ASSIGN_ZPV_TO_DEREF_WORDARRAY_ELEMENT_AT_IDX_IMM s_ptr, struct_fields, $00
  +CMP_S_PTR_TO_IMM "envs_name$"
  bcc +
  +FAIL_REASON "SCEN2: struct field not found in struct_fields"
  rts
+:

  clc
  rts


assign_dummy_args:
;----------------
  bra +
@a1: !pet "name$", $0
@a2: !pet "attack", $0
+:
  +ASSIGN_U16V_EQ_ADDR args + 0, @a1
  +ASSIGN_U16V_EQ_ADDR args + 2, @a2
  +ASSIGN_U16V_EQ_IMM  args + 4, $0000  ; null terminator?
  +ASSIGN_U8V_EQ_IMM arg_cnt, $02
rts

;-------------------------
test__parse_args_of_struct:
;-------------------------
  +ASSIGN_U8V_EQ_IMM bkt_open_idx, $04
  +ASSIGN_STRING_PTR_TO_IMM struct_obj_name, "envs(9)"
  jsr assign_dummy_args

  jsr parse_args_of_struct

  +ASSIGN_ZPV_TO_DEREF_VARTABLE_ELEMENT_AT_BACKIDX_IMM s_ptr, TYP_STR, 1

  +CMP_S_PTR_TO_IMM "envs_name$"
  bcc +
  +FAIL_REASON "SCEN1: envs_name$ not found in var_table"
  rts
+:

  +ASSIGN_ZPV_TO_DEREF_VARTABLE_ELEMENT_AT_BACKIDX_IMM s_ptr, TYP_REAL, 1

  +CMP_S_PTR_TO_IMM "envs_attack"
  bcc +
  +FAIL_REASON "SCEN2: envs_attack not found in var_table"
  rts
+:

  clc
  rts


;-----------------------------------
test__tally_up_next_dest_line_length:
;-----------------------------------
  +ASSIGN_U16V_EQ_IMM dest_lineno, 1240
  +SET_LSTRING cur_dest_line, "this is a short line :"
  +SET_STRING sf_str, "next token to add"
  +ASSIGN_U16V_EQ_U16V a_ptr, s_ptr

  jsr tally_up_next_dest_line_length

  cmp #43
  beq +
    +FAIL_REASON "did not tally to correct amount"
    rts
+:
  clc
  rts


;----------------------
test__add_cur_dest_line:
;----------------------
  +SET_LSTRING cur_dest_line, "testing a line"

  jsr add_cur_dest_line

  +ASSIGN_U32V_EQ_IMM DESTPTR, $0005, $0000
  +CMP_S_PTR_TO_PSTR32 DESTPTR
  bcc +
    +FAIL_REASON "dest line not added as expected"
    rts
+:
  clc
  rts


;------------------------
test__handle_on_next_line:
;------------------------
  +SET_LSTRING cur_dest_line, "testing a line"
  +SET_STRING cur_src_line, "next bit to add"  ; will set s_ptr also
  +ASSIGN_U16V_EQ_U16V a_ptr, s_ptr
  +ASSIGN_U16V_EQ_IMM dest_lineno, 123
  +ASSIGN_U16V_EQ_IMM cur_src_lineno, 456

  jsr handle_on_next_line
  
  ; I won't bother checking DESTPTR memory, as prior test does that already
  +CMP_STR_TO_IMM cur_dest_line + 1, "next bit to add"
  bcc +
    +FAIL_REASON "SCEN1: cur_dest_line not updated as expected"
    rts
+:

  +CMP_U16V_TO_IMM dest_lineno, 124
  beq +
    +FAIL_REASON "SCEN2: dest_lineno not incremented"
    rts
+:
  
  +ASSIGN_U16V_EQ_IMM dest_lineno, 123  ; bring it back for next check
  +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_U16V map_dest_to_src_lineno, dest_lineno
  +ASSIGN_U16V_EQ_DEREF_U16V temp16, tmp_ptr
  +CMP_U16V_TO_IMM temp16, 456
  beq +
    +FAIL_REASON "SCEN3: map_dest_to_src_lineno[] not updated as expected"
    rts
+:

  clc
  rts


;------------------------
test__handle_on_this_line:
;------------------------
  +SET_LSTRING cur_dest_line, "testing a line"
  +SET_STRING cur_src_line, "next bit to add"  ; will set s_ptr also

  jsr handle_on_this_line
  
  +CMP_STR_TO_IMM cur_dest_line + 1, "testing a line:next bit to add"
  bcc +
    +FAIL_REASON "cur_dest_line not updated as expected"
    rts
+:

  clc
  rts


;-------------------------------------
test__safe_add_to_current_or_next_line:
;-------------------------------------
  +ASSIGN_U32V_EQ_IMM DESTPTR, $0005, $0000
  +SET_STRING cur_src_line, "next bit to add"  ; will set s_ptr also
  +ASSIGN_U16V_EQ_U16V a_ptr, s_ptr

  +SET_LSTRING cur_dest_line, "012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789"  ; 150 chars long
  +ASSIGN_U16V_EQ_U16V is_ptr, s_ptr
  +COPY_STR_FROM_PSTR sf_str, is_ptr ; preserve original contents in sf_str
  +ASSIGN_U16V_EQ_IMM dest_lineno, 123

  jsr safe_add_to_current_or_next_line

  ; check current line was added to DESTPTR
  +ASSIGN_U32V_EQ_IMM DESTPTR, $0005, $0000
  +ASSIGN_U16V_EQ_ADDR s_ptr, sf_str
  +CMP_S_PTR_TO_PSTR32 DESTPTR
  bcc +
    +FAIL_REASON "SCEN1: DESTPTR data not added as expected"
    rts
+:

  +CMP_STR_TO_IMM cur_dest_line + 1, "next bit to add"
  bcc +
    +FAIL_REASON "SCEN2: cur_dest_line not as expected"
    rts
+:

  +ASSIGN_U32V_EQ_IMM DESTPTR, $0005, $0000
  +SET_LSTRING cur_dest_line, "short line"
;
  jsr safe_add_to_current_or_next_line
;
  +CMP_STR_TO_IMM cur_dest_line + 1, "short line:next bit to add"
  bcc +
    +FAIL_REASON "SCEN3: cur_dest_line not as expected"
    rts
+:

  clc
  rts


;---------------------------------------------------
test__find_struct_obj_name_and_gen_struct_field_vars:
;---------------------------------------------------
; NOTE: We're relying on a prior test adding ENVTYPE into struct_vars
  +SET_STRING cur_src_line, "envs(9) = [ [ \"Piano\", 0, 9, 0 ] ]"
  +ASSIGN_U16V_EQ_ADDR s_ptr, cur_src_line
  +ASSIGN_U8V_EQ_IMM found_idx, $00

  jsr find_struct_obj_name_and_gen_struct_field_vars

  +CMP_PSTR_TO_IMM struct_obj_name, "envs(9)"
  bcc +
    +FAIL_REASON "SCEN1: struct name not as expected"
    rts
+:

  +CMP_U8V_TO_IMM bkt_open_idx, $04
  beq +
    +FAIL_REASON "SCEN2: bkt_open_idx != 4"
    rts
+:

  +ASSIGN_ZPV_TO_DEREF_VARTABLE_ELEMENT_AT_BACKIDX_IMM s_ptr, TYP_STR, 1
  +CMP_S_PTR_TO_IMM "envs_name$"
  bcc +
  +FAIL_REASON "SCEN3: envs_name$ not found in var_table"
  rts
+:

  +ASSIGN_ZPV_TO_DEREF_VARTABLE_ELEMENT_AT_BACKIDX_IMM s_ptr, TYP_REAL, 3

  +CMP_S_PTR_TO_IMM "envs_attack"
  bcc +
  +FAIL_REASON "SCEN4: envs_attack not found in var_table"
  rts
+:

  +ASSIGN_ZPV_TO_DEREF_VARTABLE_ELEMENT_AT_BACKIDX_IMM s_ptr, TYP_REAL, 2

  +CMP_S_PTR_TO_IMM "envs_decay"
  bcc +
  +FAIL_REASON "SCEN4: envs_decay not found in var_table"
  rts
+:

  +ASSIGN_ZPV_TO_DEREF_VARTABLE_ELEMENT_AT_BACKIDX_IMM s_ptr, TYP_REAL, 1

  +CMP_S_PTR_TO_IMM "envs_sustain"
  bcc +
  +FAIL_REASON "SCEN4: envs_sustain not found in var_table"
  rts
+:

  clc
  rts


;----------------------------------------
test__check_for_creation_of_struct_object:
;----------------------------------------
  +SET_STRING cur_src_line, "ENVTYPE envs(9) = [ [ \"Piano\", 0, 9, 0 ] ]"
  +ASSIGN_U16V_EQ_ADDR s_ptr, cur_src_line

  jsr check_for_creation_of_struct_object

  sec
  rts


;--------------------------------------
test__check_for_continue_onto_next_line:
;--------------------------------------
  +SET_LSTRING cur_dest_line, "testing a line with left-arrow marker "
  +APPEND_IMM_CHR_TO_S_PTR $5f  ; right-arrow char
  inc cur_dest_line ; update length-byte

  jsr check_for_continue_onto_next_line

  +CMP_U8V_TO_IMM cont_next_line_flag, $01
  beq +
    +FAIL_REASON "SCEN1: flag not set"
    rts
+:

  +CMP_STR_TO_IMM cur_dest_line+1, "testing a line with left-arrow marker "
  bcc +
    +FAIL_REASON "SCEN2: dest-line not valid"
    rts
+:

  +SET_LSTRING cur_dest_line, "line with no cont marker"

  jsr check_for_continue_onto_next_line

  +CMP_U8V_TO_IMM cont_next_line_flag, $00
  beq +
    +FAIL_REASON "SCEN3: flag not clear"
    rts
+:

  +CMP_STR_TO_IMM cur_dest_line+1, "line with no cont marker"
  bcc +
    +FAIL_REASON "SCEN4: dest-line not valid"
    rts
+:

  clc
  rts


;--------------------------------
test__check_continue_on_next_line:
;--------------------------------
  bra +
cont_marker_token:
!pet $5f,$00
+:
  +COPY_STR_FROM_STR cur_src_line, cont_marker_token

  jsr check_continue_on_next_line
  bcc +
    +FAIL_REASON "expected C=0"
    rts
+:

  clc
  rts


;--------------
test__check_sm0:
;--------------
  +ASSIGN_U8V_EQ_IMM sm, $00
  +SET_STRING cur_src_line, "z"
  +ASSIGN_U16V_EQ_IMM cur_src_lineno, 123

  jsr check_sm0

  +ASSIGN_U16V_EQ_ADDR s_ptr, parser_error
  +CMP_S_PTR_TO_IMM "?expected '[' on line 123"
  beq + ; null-terminator?
  +FAIL_REASON "SCEN1: parser error not as expected"
  rts
+:

  +SET_STRING cur_src_line, "["

  jsr check_sm0

  bcc +
    +FAIL_REASON "SCEN2: c != 0"
    rts
+:

  +CMP_U8V_TO_IMM sm, $01
  beq +
    +FAIL_REASON "SCEN3: sm != 1"
    rts
+:

  clc
  rts


;-------------------------
test__check_sm1_before_sm2:
;-------------------------
  +ASSIGN_U8V_EQ_IMM sm, $00
  +ASSIGN_U8V_EQ_IMM parser_error, $00  ; null-term
  +ASSIGN_U16V_EQ_IMM cur_src_lineno, 123

  jsr check_sm1_before_sm2

  +CMP_U8V_TO_IMM parser_error, $00
  beq +
    +FAIL_REASON "SCEN1: parser error found, but not expected"
    rts
+:

  +ASSIGN_U8V_EQ_IMM sm, $01
  +SET_STRING cur_src_line, "]"

  jsr check_sm1_before_sm2

  +CMP_U8V_TO_IMM parser_error, $00
  beq +
    +FAIL_REASON "SCEN2: parser error found, but not expected"
    rts
+:

  +SET_STRING cur_src_line, "z"

  jsr check_sm1_before_sm2

  +ASSIGN_U16V_EQ_ADDR s_ptr, parser_error
  +CMP_S_PTR_TO_IMM "?expected '[' or ']' on line 123"
  bcc +
  +FAIL_REASON "SCEN3: parser error not as expected"
  rts
+:

  clc
  rts


;---------------------
test__find_struct_type:
;---------------------
  +SET_STRING f_str, "ENVTYPE"
  +ASSIGN_U16V_EQ_ADDR s_ptr, f_str

  jsr find_struct_type

  bcc +
    +FAIL_REASON "SCEN1: expected carry=0 (found)"
    rts
+:

  +CMP_U8V_TO_IMM found_idx, $00
  beq +
    +FAIL_REASON "SCEN2: expected found_idx=0"
    rts
+:

  +SET_STRING f_str, "BOOGIE"
  
  jsr find_struct_type

  bcs +
    +FAIL_REASON "SCEN3: expected carry=1 (not found)"
    rts
+:
  
  +CMP_U8V_TO_IMM found_idx, $FF
  beq +
    +FAIL_REASON "SCEN4: expected found_idx=$ff"
    rts
+:

  clc
  rts


;------------------------
test__parse_standard_line:
;------------------------
  +ASSIGN_U8V_EQ_IMM delete_line_flag, $00

  +SET_STRING cur_src_line, "ENVTYPE envs(9) = [ [ \"Piano\", 0, 9, 0 ] ]"

  jsr parse_standard_line

  +CMP_PSTR_TO_IMM struct_obj_name, "env_name$"


  ; needed to assess replace_vars_and_labels
  +SET_STRING cur_src_line, "a$(FISHY)"
  ; this will set s_ptr to point to it too

  jsr parse_standard_line

  +ASSIGN_U16V_EQ_ADDR s_ptr, cur_src_line ; f_str is really s$
  +CMP_S_PTR_TO_IMM "a$(1)"
  bcc +
    +FAIL_REASON "SCEN1: s_ptr != 'a$(1)'"
  rts
+:

  +CMP_STR_TO_IMM cur_dest_line+1, "a$(1)"
  bcc +
    +FAIL_REASON "SCEN1: cur_dest_line != 'a$(1)'"
  rts
+:

  sec
  rts


; -------
; HELPERS
; -------

get_label_name_item_at_idx:
  ldx #$00
  lda label_name,x
  sta tmp_ptr
  inx
  lda label_name,x
  sta tmp_ptr+1

@loop:
  cpz #$00
  beq +
  inw tmp_ptr
  inw tmp_ptr
  dez
  bra @loop
+:

  ldy #$00
  lda (tmp_ptr),y
  sta s_ptr
  iny
  lda (tmp_ptr),y
  sta s_ptr+1

  rts


get_label_lineno_at_idx:
  ; input: z = index
  ; output: yx = lineno at index (uint value)
  ldx #$00
  lda label_lineno,x
  sta tmp_ptr
  inx
  lda label_lineno,x
  sta tmp_ptr+1
  
@loop:
  cpz #$00
  beq +
  inw tmp_ptr
  inw tmp_ptr
  dez
  bra @loop
+:

  ldy #$00
  lda (tmp_ptr),y
  tax
  iny
  lda (tmp_ptr),y
  tay

  rts


prepare_SRCPTR:
  lda #$02
  sta SRCPTR
  lda #$00
  sta SRCPTR+1
  lda #$03
  sta SRCPTR+2
  lda #$08
  sta SRCPTR+3
  rts

feed_dummy_string:
  pla
  clc
  adc #$01
  sta ret_ptr_lo
  pla
  adc #$00
  sta ret_ptr_hi

  lda #$01
  sta cur_line_len  ; skip the initial length byte

  jsr print_to_dummy_string

  ; add length byte to start of string
  ldz #$00
  dec cur_line_len
  lda cur_line_len
  sta [SRCPTR],z

  lda ret_ptr_hi
  pha
  lda ret_ptr_lo
  pha
  rts

print_to_dummy_string:
  ldx #$00
  lda (ret_ptr_lo,x)
  beq @found_null

  ldz cur_line_len
  sta [SRCPTR],z
  inc cur_line_len

  inc ret_ptr_lo
  bne print_to_dummy_string
  inc ret_ptr_hi
  bne print_to_dummy_string

@found_null:
  rts


set_font_a:
  +ASSIGN_U32V_EQ_IMM FOURPTR, $0002, $9000  ; source is FONT A
  +ASSIGN_U32V_EQ_IMM FOURPTR+4,  $0ff7, $e000  ; dest FONT

  ; copy across
    ldz  #0
    ldx  #16        ; copy 4K
-:
    lda  [FOURPTR],z
    sta  [FOURPTR+4],z
    inz
    bne -
    inc FOURPTR+1
    inc FOURPTR+5
    dex
    bne -
    rts

!source "tests_autogen.asm"

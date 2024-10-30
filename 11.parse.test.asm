; ------
; MACROS
; ------
!macro FORCE_ADD_VAR_TO_VARTABLE .name, .type {
  +ASSIGN_STRING_PTR_TO_IMM var_name, .name
  +ASSIGN_U8V_EQ_IMM ty, .type
  jsr add_varname_to_vartable
  +INCREMENT_ELCNT_OF_TY
}

!macro STR_MATCH .str1, .str2 {
  +assign_u16v_eq_addr tmp_ptr, .str1
  +assign_u16v_eq_addr s_ptr, .str2
  jsr cmp_tmp_ptr_to_s_str
}

!macro STR_MATCH_PTR_TO_STR .ptr, .str {
  +assign_u16v_eq_u16v tmp_ptr, .ptr
  +assign_u16v_eq_addr s_ptr, .str
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

!macro STR_MATCH_TO_SPTR .str1 {
  +assign_u16v_eq_addr tmp_ptr, .str1
  jsr cmp_tmp_ptr_to_s_str
}

!macro FAIL_REASON .txt {
    lda #$00
    sta parser_error
    sta cur_line_len
    +assign_u16v_eq_addr s_ptr, parser_error
    jsr append_inline_text_to_str
!pet $0d,150,$09,.txt,$0d,$05,$00

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
fail_reason_flag:
!byte $00

run_tests:
  lda #$00
  sta passed_cnt
  sta failed_cnt
  sta total_cnt

  +assign_u16v_eq_addr TESTPTR, test_array

  ; lower-case
  lda #$0e
  jsr CHROUT

  ; 80x50
  lda #27
  jsr CHROUT
  lda #'5'
  jsr CHROUT

  lda #$00
  sta $d020
  sta $d021

  jsr set_font_a

  jsr print_inline_text
!pet 147, $05, "^\"11.parse\"", $0d,$00
  jsr print_inline_text
!pet "---------------------",$0d,$00
  jsr print_inline_text
!pet "'11.parse' test suite",$0d,$00
  jsr print_inline_text
!pet "---------------------",$0d,$00

@loop_next_test:
  lda #$00
  sta fail_reason_flag

  ldy #$00
  lda (TESTPTR),y
  iny
  ora (TESTPTR),y
  beq @bail_out  ; if word pointer is zero, then bail out

  inc total_cnt

  jsr print_inline_text
!pet $05, "[    ] ",$00

  ldy #$00
  lda (TESTPTR),y
  sta ret_ptr_lo
  iny
  lda (TESTPTR),y
  sta ret_ptr_hi
  jsr print_text

  ; go back to start of line
  lda #27
  jsr CHROUT
  lda #'J'
  jsr CHROUT

  inw TESTPTR
  inw TESTPTR

  ldy #$00
  lda (TESTPTR),y
  sta tmp_ptr
  iny
  lda (TESTPTR),y
  sta tmp_ptr+1

  jsr (tmp_ptr)  ; test__read_next_line
  bcc +

  inc failed_cnt
  jsr print_inline_text
!pet $05, "[", $1c, "FAIL", $05, "] ",$00

  lda fail_reason_flag
  beq @skip_fail_reason

    +assign_u16v_eq_addr ret_ptr_lo, parser_error

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

  lda #$0d
  jsr CHROUT

  jmp @loop_next_test

@bail_out:
  jsr print_inline_text
!pet "---------------------",$0d,$00
  jsr print_inline_text
!pet "TOTAL TESTS: ",$00
  ldx total_cnt
  ldy #$00
  jsr print_uint
  lda #$0d
  jsr CHROUT

  jsr print_inline_text
!pet $1e, "TOTAL PASSED: ",$00
  ldx passed_cnt
  ldy #$00
  jsr print_uint
  lda #$0d
  jsr CHROUT

  jsr print_inline_text
!pet $1c, "TOTAL FAILED: ",$00
  ldx failed_cnt
  ldy #$00
  jsr print_uint
  lda #$0d
  jsr CHROUT
  lda #$05
  jsr CHROUT

  jsr print_inline_text
!pet "---------------------",$0d,$00
  
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
  +ASSIGN_STRING_PTR_TO_IMM var_name, "fishy$"
  +ASSIGN_U8V_EQ_IMM ty, TYP_STR

  jsr add_varname_to_vartable

  +SET_IS_PTR_TO_VARTABLE_AT_TY_IDX
  +UPDATE_IS_STR_TO_LATEST_ELEMENT_COUNT_IDX
  
  ldy #$00
  lda (is_ptr),y
  sta s_ptr
  iny
  lda (is_ptr),y
  sta s_ptr+1

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
  +UPDATE_IS_STR_TO_LATEST_ELEMENT_COUNT_IDX
  
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
  sec
  rts


;----------------------------
test__replace_vars_and_labels:
;----------------------------
  // NOTE: This test relies on the prior "test__add_to_label_table:" adding the
  // label ".testlabel1" into the label table already

  +SET_STRING f_str, "a$(testlabel1)"
  ; this will set s_ptr to point to it too

  jsr replace_vars_and_labels

  bra +
@expected:
!pet "a$(5)",$00
+:

  +assign_u16v_eq_u16v s_ptr, var_name
  +STR_MATCH_TO_SPTR @expected
  bcc +
  +FAIL_REASON "s_ptr != 'a$(5)'"
  rts
+:
  
  clc
  rts


;----------------------------
test__check_token_for_subbing:
;----------------------------
  +FORCE_ADD_VAR_TO_VARTABLE "moo$", TYP_STR
  +SET_LSTRING cur_tok, $04, "moo$"

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
  +assign_u16v_eq_addr s_ptr, a_str
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
!pet $06, "hello ", $00  ; length-encoded in first byte

  +SET_LSTRING cur_tok, $05, "world"

  jsr add_curtok_to_astr

  ; SCEN1: check new astr length
  lda a_str
  cmp #11
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
  +assign_u16v_eq_addr s_ptr, a_str
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
!pet $04, "hell", $00  ; length-encoded in first byte

  lda #'o'
  sta cur_char

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
  +SET_LSTRING cur_tok, $04, "hell"

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
  +SET_LSTRING a_str, $02, "xy"
  +SET_LSTRING cur_tok, $05, "token"
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
  sec
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
  +assign_u16v_eq_addr args, tempheap
  +ASSIGN_U8V_EQ_IMM arg_idx, $00
  +ASSIGN_U8V_EQ_IMM arg_cnt, $00

  +assign_u16v_eq_addr s_ptr, tempheap
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

  +assign_u16v_eq_addr var_name, f_str

  jsr declare_assignment_check
  
  bra +
@expected1:
!pet "a", $00

@expected2:
!pet "1", $00
+:

  +assign_u16v_eq_u16v s_ptr, var_name
  +STR_MATCH_TO_SPTR @expected1
  bcc +
  +FAIL_REASON "var_name != 'a'"
  rts
+:

  +assign_u16v_eq_u16v s_ptr, value
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
  +assign_u16v_eq_addr var_name, f_str

  +ASSIGN_U8V_EQ_IMM bkt_open_idx, $05
  +ASSIGN_U8V_EQ_IMM bkt_close_idx, $08

  jsr declare_dimension_check

  bra +
@expected1:
!pet "fish$",$00
@expected2:
!pet "10",$00
+:

  +assign_u16v_eq_u16v s_ptr, var_name
  +STR_MATCH_TO_SPTR @expected1
  bcc +
  +FAIL_REASON "var_name != 'fish$'"
  rts
+:

  +assign_u16v_eq_u16v s_ptr, dimension
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
  +assign_u16v_eq_addr s_ptr, cur_tok

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

  +assign_u16v_eq_addr s_ptr, cur_tok
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
  +assign_u16v_eq_addr s_ptr, cur_tok

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
  +SET_LSTRING cur_tok, $05, "gosub"

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
  +SET_LSTRING cur_tok, $05, "$ABCD"

  jsr check_hex_and_binary_value

  ; confirm parser_error is still "" (input was valid hex)
  lda parser_error 
  beq + ; null-terminator?
  +FAIL_REASON "SCEN1: parser error found"
  rts
+:

  ; SCEN2: Valid hex (in lowercase)
  ; - - - - - - - -
  +SET_LSTRING cur_tok, $05, "$abcd"

  jsr check_hex_and_binary_value

  lda parser_error 
  beq + ; null-terminator?
  +FAIL_REASON "SCEN2: parser error found"
  rts
+:

  ; SCEN3: Invalid hex
  ; - - - - - - - -
  +SET_LSTRING cur_tok, $05, "$EFGH"

  jsr check_hex_and_binary_value

  lda parser_error 
  bne + ; null-terminator?
  +FAIL_REASON "SCEN3: expected parser error"
  rts
+:

  ; SCEN4: valid binary
  ; - - - - - - - -
  +ASSIGN_U8V_EQ_IMM parser_error, $00
  +SET_LSTRING cur_tok, $05, "%0101"

  jsr check_hex_and_binary_value

  lda parser_error 
  beq + ; null-terminator?
  +FAIL_REASON "SCEN4: parser error found"
  rts
+:

  ; SCEN5: invalid binary
  ; - - - - - - - -
  +SET_LSTRING cur_tok, $08, "%0101234"

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
  +SET_LSTRING cur_tok, $08, "fishy"

  jsr check_ignore_existing_vocab

  bcc +
  +FAIL_REASON "SCEN1: wasn't expecting token in vocab"
  rts
+:

  ; SCEN2: in vocab
  ; - - - - - - - -
  +SET_LSTRING cur_tok, $08, "tron"

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
  +SET_LSTRING cur_tok, $06, "dishy%"

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
  +assign_u32v_eq_addr FOURPTR, $0002, $9000  ; source is FONT A
  +assign_u32v_eq_addr FOURPTR+4,  $0ff7, $e000  ; dest FONT

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

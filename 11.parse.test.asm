; ------
; MACROS
; ------
!macro STR_MATCH .str1, .str2 {
  +assign_u16v_eq_addr tmp_ptr, .str1
  +assign_u16v_eq_addr s_ptr, .str2
  jsr cmp_tmp_ptr_to_s_str
}

; -----
; TESTS
; -----

run_tests:
  +assign_u16v_eq_addr TESTPTR, test_array

  ; lower-case
  lda #$0e
  jsr CHROUT

  lda #$00
  sta $d020
  sta $d021

  jsr set_font_a

  jsr print_inline_text
!pet 147, "---------------------",$0d,$00
  jsr print_inline_text
!pet "'11.parse' test suite",$0d,$00
  jsr print_inline_text
!pet "---------------------",$0d,$00

@loop_next_test:
  clc
  ldy #$00
  lda (TESTPTR),y
  iny
  adc (TESTPTR),y
  beq @bail_out  ; if word pointer is zero, then bail out

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

  jsr print_inline_text
!pet $05, "[", $1c, "FAIL", $05, "] ",$00
  bra ++

+:
  jsr print_inline_text
!pet $05, "[", $1e, "PASS", $05, "] ",$00

++:
  inw TESTPTR
  inw TESTPTR

  lda #$0d
  jsr CHROUT

  bra @loop_next_test

@bail_out:
  rts


test__read_fish:
  sec
  rts

test__read_next_line:
  jsr prepare_SRCPTR

; feed a dummy string into #8030000
  jsr feed_dummy_string
_test_str:
!pet "#output \"fishy\"", $00

  jsr read_next_line

  +STR_MATCH _test_str, cur_src_line
  rts

; -------
; HELPERS
; -------

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

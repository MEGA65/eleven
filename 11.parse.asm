#output "11.parse-new"

* = $2001

// BASIC Stub
  !word basic_end   // pointer to next basic line
  !word $0a         // line 10
  !byte $fe,$02     // 'BANK' command
  !text "128:"       // '128:'
  !byte $9e         // 'SYS' command
  !text "8211"      // '8211'
  !byte $00         // end of current basic line

basic_end:
  !word $00         // end of basic program

; BANK4 Memory usage  (aka 'Mailbox RAM')
; ------------------
; 4.FF00  "sk" magic signature (Stephan Kleinert / ubik's initials)
; 4.FF02  border colour
; 4.FF03  background colour
; 4.FF04  foreground colour
; 4.FF05  highlight colour
; 4.FF06  status bar colour
; 4.FF07  misc. flags
;         - bit0: autoload source enable flag
;         - bit1: autojump to line flag
;         - bit2: autobackup flag
;         - bit3: text-mode (0=80x25, 1=80x50)
;         - bit4: verbose flag
;         - bit5-7: reserved
; 4.FF08  error number to be displayed (>128 are preprocessor errors)
;   - 131 = illegal hex value
;   - 132 = illegal binary value
;     (but editor doesn't make use of this number as yet)
; 4.FF09  (2 bytes) line number for autojump
;
; 4.FF10  current file name
; 4.FF20  output file name
;
; 4.FF30- parser error message string

; 4.FF7F
;
; Attic RAM cache usage
; =====================
; $800,0000 +---------------+
;           ! 11.EDIT       ! (max 53kb)
; $801,0000 +---------------+
;           ! 11.PARSE      ! (max 53kb)
; $802,0000 +---------------+
;           ! 11.SETTINGS   ! (8kb)
; $802,2000 +---------------+
;           ! 11.TOKENIZE   ! (8kb)
; $802,4000 +---------------+
;           ! 11.POST       ! (8kb)
; $802,6000 +---------------+
; 
; $803,0000 +---------------+
;           ! ELEVEN SOURCE !
;       ... +---------------+

; ELEVEN SOURCE STORAGE FORMAT:
; ----------------------------
; $803,0000  WORD       Total Lines
; $803,0002  BYTE       Line 1 length
; $803,0003  BYTE[LEN]  Chars for Line 1
; $803,00xx  BYTE       Line 2 length
; $803,00yy  BYTE[LEN]  Chars for Line 2
; ...

!convtab pet  ; assure string/chars have lower(65-90) and upper (97-122)

; ------
; MACROS
; ------
!macro PUSH_U8V .var {
  lda .var
  pha
}

!macro PULL_U8V .var {
  pla
  sta .var
}

!macro ASSIGN_ZPV_TO_DEREF_VARTABLE_ELEMENT_AT_BACKIDX_IMM .zpvar, .type, .idx {
  +ASSIGN_U8V_EQ_IMM ty, .type
  +SET_IS_PTR_TO_VARTABLE_AT_TY_IDX
  +ASSIGN_A_EQ_ELCNT_OF_TYPE_MINUS_IMM .type, .idx
  +UPDATE_IS_PTR_TO_DESIRED_ELEMENT_IDX_OF_A
  +ASSIGN_U16V_EQ_DEREF_U16V .zpvar, is_ptr
}

!macro ASSIGN_ZPV_TO_DEREF_WORDARRAY_ELEMENT_AT_BACKIDX_IMM .zpvar, .array, .idx {
  +ASSIGN_U16V_EQ_ADDR is_ptr, .array
  +ASSIGN_A_EQ_ELCNT_OF_TYPE_MINUS_IMM .type, .imm
  +UPDATE_IS_PTR_TO_DESIRED_ELEMENT_IDX_OF_A
  +ASSIGN_U16V_EQ_DEREF_U16V .zpvar, is_ptr
}

!macro ASSIGN_ZPV_TO_DEREF_WORDARRAY_ELEMENT_AT_IDX_IMM .zpvar, .array, .idx {
  +ASSIGN_U16V_EQ_ADDR is_ptr, .array
  lda #.idx
  +UPDATE_IS_PTR_TO_DESIRED_ELEMENT_IDX_OF_A
  +ASSIGN_U16V_EQ_DEREF_U16V .zpvar, is_ptr
}

!macro ASSIGN_ZPV_TO_DEREF_LATEST_VARTABLE_ELEMENT_OF_TYPE .zpvar, .type {
  +ASSIGN_U8V_EQ_IMM ty, .type
  +SET_IS_PTR_TO_VARTABLE_AT_TY_IDX
  +ASSIGN_A_EQ_ELCNT_OF_TYPE_MINUS_1 .type
  +UPDATE_IS_PTR_TO_DESIRED_ELEMENT_IDX_OF_A
  +ASSIGN_U16V_EQ_DEREF_U16V .zpvar, is_ptr
}

!macro SUB_U8V_WITH_IMM .var, .val {
  sec
  lda .var
  sbc #.val
  sta .var
}

!macro SET_PARSER_ERROR_ON_LINE .text {
;     parser_error$ = "?declare parameter missing in line " + str$(cur_src_lineno)
    +SET_STRING parser_error, "?declare parameter missing in line "
    +APPEND_UINT_TO_S_PTR cur_src_lineno
}

!macro SET_IS_PTR_TO_LAST_VARTABLE_ELEMENT_OF_TYPE .type {
  +SET_IS_PTR_TO_VARTABLE_AT_TY_IDX
  +ASSIGN_A_EQ_ELCNT_OF_TYPE_MINUS_1 .type
  +UPDATE_IS_PTR_TO_DESIRED_ELEMENT_IDX_OF_A
}

!macro ASSIGN_A_EQ_ELCNT_OF_TYPE_MINUS_1 .type {
  ldx #.type
  lda element_cnt,x
  tax
  dex
  txa
}

!macro ASSIGN_A_EQ_ELCNT_OF_TYPE_MINUS_IMM .type, .imm {
  ldx #.type
  lda element_cnt,x
  sec
  sbc #.imm
}

!macro ASSIGN_U8V_EQ_ELCNT_IDX_OF_TY .var {
  ldx ty
  lda element_cnt,x
  sta .var
}

!macro STR_MATCH_TO_SPTR .str1 {
  +ASSIGN_U16V_EQ_ADDR tmp_ptr, .str1
  jsr cmp_tmp_ptr_to_s_str
}

!macro UPDATE_S_PTR_TO_SKIP_DECL_OR_DEF {
    +ADD_TO_POINTER_U8 s_ptr, 9

    +SUB_U8V_WITH_IMM cur_line_len, 9

    +CMP_U8V_TO_IMM define_flag, $00
    beq +
    ; the (- define_flag) part
    lda define_flag
    beq +
    dew s_ptr
    clc
    inc cur_line_len
+:
}

!macro SET_VARNAME_EQ_ARGS_I {
      lda arg_idx
      clc
      rol
      tay
      
      lda args,y
      sta var_name
      iny
      lda args,y
      sta var_name+1
}

!macro HEAP_COPY_PSTR_EQ_PSTR .dest, .src {
  ; figure out length of value
  +ASSIGN_U16V_EQ_U16V s_ptr, .src
  jsr get_s_ptr_length

  ; todo: allocate a string from the heap
    ldz #$00
    ldx cur_line_len
    inx ; add one extra for null terminator
    txa
    +DYN_ALLOC_BYTES_SIZE_ZA temp16
    ldy #$00
    lda temp16
    sta (<.dest),y
    iny
    lda temp16+1
    sta (<.dest),y ; put new string ptr into defines_val table

    +COPY_PSTR_FROM_PSTR temp16, .src
}


!macro SET_TMP_PTR_TO_DEFVALS_AT_ELCNT_IDX {
  +ASSIGN_U16V_EQ_ADDR tmp_ptr, define_val
  ldx ty
  lda element_cnt,x
  +UPDATE_TMP_PTR_TO_WORD_IDX_OF_A
}

!macro SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_U8V .wrd_array, .var {
  lda .var
  +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_A .wrd_array
}

!macro SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_A .wrd_array {
  pha
  +ASSIGN_U16V_EQ_ADDR tmp_ptr, .wrd_array
  pla
  +UPDATE_TMP_PTR_TO_WORD_IDX_OF_A
}

!macro UPDATE_TMP_PTR_TO_WORD_IDX_OF_A {
    clc
    rol
    adc tmp_ptr  ; todo: is there a chance the rol could set the carry and +1?
    sta tmp_ptr
    bcc +
    ; increment high-byte of pointer
    inc tmp_ptr+1
+:
}

!macro SET_PTR_TO_ARRAY_AT_OFFSET .ptr, .array, .offset {
    +ASSIGN_U16V_EQ_ADDR .ptr, .array
    clc
    lda .ptr
    adc #<.offset
    sta .ptr
    lda .ptr+1
    adc #>.offset
    sta .ptr+1
}

!macro CMP_LSTR_LEN_TO_IMM .var, .len {
  lda .var
  cmp #.len
}

!macro INCREMENT_ELCNT_OF_TY {
  ldx ty
  inc element_cnt,x
}

!macro CMP_GENVNAME_TO .c1, .c2 {
  lda gen_varname
  cmp #.c1
  bne +
  lda gen_varname+1
  cmp #.c2
  bne +
  clc  ; found
  bra ++
+:
  sec ; not found
++:
}

!macro SET_MULTINA_TO_U8V .var {
  ldz #$00
  ldy #$00
  ldx #$00
  lda .var
  stq MULTINA
}

!macro SET_MULTINB_TO_U8 .val {
  ldz #$00
  ldy #$00
  ldx #$00
  lda #.val
  stq MULTINB
}

!macro WAIT_DIV_FINISH {
@wait_for_divide_to_finish:
    bit MULDIVBUSY  ; bit6 (mulbusy) = overflow flag
                    ; bit7 (divbusy) = negative flag
    bmi @wait_for_divide_to_finish
}

!macro WAIT_MUL_FINISH {
@wait_for_mult_to_finish:
    bit MULDIVBUSY  ; bit6 (mulbusy) = overflow flag
                    ; bit7 (divbusy) = negative flag
    bvs @wait_for_mult_to_finish
}

!macro UPDATE_IS_PTR_TO_LATEST_ELEMENT_COUNT_IDX {
    lda element_cnt,x
    +UPDATE_IS_PTR_TO_DESIRED_ELEMENT_IDX_OF_A
}

!macro UPDATE_IS_PTR_TO_DESIRED_ELEMENT_IDX_OF_A {
    clc
    rol
    bcc +
    ; increment high-byte of pointer
    inc is_ptr+1
+:
    clc
    adc is_ptr
    sta is_ptr
    lda #$00
    adc is_ptr+1
    sta is_ptr+1
}
    
!macro SET_IS_PTR_TO_VARTABLE_AT_TY_IDX {
    +ASSIGN_U16V_EQ_ADDR is_ptr, var_table
    ldx #$00
@loop_next_type:
    cpx ty
    beq @bail_type
    clc

    +add_to_pointer_u16 is_ptr, 200*2
    
    inx
    bra @loop_next_type
@bail_type:
}
   
!macro APPEND_INLINE_TO_S_PTR .txt {
  jsr append_inline_text_to_str
!pet .txt, $00
}

!macro PRINT_U8V .var {
  ldx .var
  ldy #$00
  jsr print_uint
}

!macro PRINT_U16V .var {
  ldx .var
  ldy .var+1
  jsr print_uint
}

!macro PRINT_INLINE_CR .txt {
  +PRINT_INLINE .txt
  +PRINT_CHR $0d
}

!macro PRINT_INLINE .txt {
  jsr print_inline_text
!pet .txt, $00
}

!macro PRINT_CHR .val {
  lda #.val
  jsr CHROUT
}

!macro PRINT_PSTR .ptr {
  +ASSIGN_U16V_EQ_U16V ret_ptr_lo, .ptr
  jsr print_text
}

!macro PRINT_PPSTR .ptr {
  +ASSIGN_U16V_EQ_DEREF_U16V ret_ptr_lo, .ptr
  jsr print_text
}

!macro CMP_U8V_IN_IMM_RANGE .var, .val1, .val2 {
  lda .var
  cmp #.val2
  bcc +   ; valid (less than max)
  beq +   ; valid (equal to max)
  bra ++  ; invalid
+:
  cmp #.val1
  bcc ++   ; invalid

  clc
  bra +++   ; move on

++:  ; invalid
  sec
+++:
}

!macro APPEND_UINT_TO_S_PTR .uint {
    lda #$01
    sta to_str_flag
    ldx .uint
    ldy .uint+1
    jsr print_uint
    lda #$00
    sta to_str_flag
    ldy cur_line_len  ; append null-terminator
    sta (s_ptr),y
}

!macro APPEND_IMM_CHR_TO_S_PTR .val {
  lda #.val
  jsr append_char_to_str
}

!macro APPEND_TYPE_CHAR_TO_S_PTR {
  ldx ty
  lda type_ident,x
  jsr append_char_to_str  ; type char (e.g. '$')
}

!macro APPEND_STR_TO_S_PTR .var {
    lda #<.var
    sta ret_ptr_lo
    lda #>.var
    sta ret_ptr_hi
    jsr append_text_to_str
}

!macro APPEND_PSTR_TO_S_PTR .ptr {
    +ASSIGN_U16V_EQ_U16V ret_ptr_lo, .ptr
    jsr append_text_to_str
}

!macro APPEND_MIDZ_PSTR_TO_S_PTR .var, .midposvar {
    clc
    lda .var
    adc .midposvar
    sta ret_ptr_lo
    lda .var+1
    adc #$00
    sta ret_ptr_hi
    jsr append_text_to_str
}

!macro APPEND_LEFTZ_PSTR_TO_S_PTR .var, .leftlengthvar {
    +ASSIGN_U8V_EQ_IMM cur_line_len, $00
    lda .var
    sta ret_ptr_lo
    lda .var+1
    sta ret_ptr_hi
    ldz .leftlengthvar
    jsr append_left_text_to_str
}

!macro COPY_STR .var1, .var2 {
    +ASSIGN_U16V_EQ_ADDR s_ptr, .var1
    +ASSIGN_U8V_EQ_IMM cur_line_len, $00
    lda #<.var2
    sta ret_ptr_lo
    lda #>.var2
    sta ret_ptr_hi
    jsr append_text_to_str
}

!macro COPY_TO_STR_FROM_S_PTR .var {
    ldy #$00
-:
    lda (s_ptr),y
    sta .var,y
    cmp #$00  ; null term?
    beq +
    iny
    bra -
+:
}

!macro COPY_STR_FROM_PSTR .var1, .var2 {
    +ASSIGN_U16V_EQ_ADDR s_ptr, .var1
    +ASSIGN_U8V_EQ_IMM cur_line_len, $00
    lda .var2
    sta ret_ptr_lo
    lda .var2+1
    sta ret_ptr_hi
    jsr append_text_to_str
}

!macro COPY_PSTR_FROM_PSTR .var1, .var2 {
    +ASSIGN_U16V_EQ_U16V s_ptr, .var1
    +ASSIGN_U8V_EQ_IMM cur_line_len, $00
    lda .var2
    sta ret_ptr_lo
    lda .var2+1
    sta ret_ptr_hi
    jsr append_text_to_str
}

!macro COPY_PSTR_FROM_STR .var1, .var2 {
    +ASSIGN_U16V_EQ_U16V s_ptr, .var1
    +ASSIGN_U8V_EQ_IMM cur_line_len, $00
    lda #<.var2
    sta ret_ptr_lo
    lda #>.var2+1
    sta ret_ptr_hi
    jsr append_text_to_str
}

!macro POKEB4 .addr, .val {
    lda #<.addr
    sta FOURPTR
    lda #>.addr
    sta FOURPTR+1
    ldz #$00
    lda #.val
    sta [FOURPTR],z
}

!macro check_09_range {
    cmp #'0'
    bcc +
    cmp #'9'+1
    bcc ++ ; branch to passed
+:  ; fail check
      ; if not, sec
      sec
      rts
++:  ; passed
}

!macro add_multout_to_u16result {
    clc
    lda u16result
    adc MULTOUT
    sta u16result
    lda u16result+1
    adc MULTOUT+1
    sta u16result+1
}

!macro multiply_a_by_column_value {
    ldx #$00
    ldy #$00
    ldz #$00
    stq MULTINA

    ldx column_val+1
    lda column_val
    stq MULTINB

@wait_for_mult_to_finish:
    bit MULDIVBUSY  ; bit6 (mulbusy) = overflow flag
                    ; bit7 (divbusy) = negative flag
    bvs @wait_for_mult_to_finish
}

!macro CMP_U8V_TO_U8V .var, .cmpvar {
  lda .var
  cmp .cmpvar
}

!macro CMP_U8V_TO_IMM .var, .val {
  lda .var
  cmp #.val
}

!macro SET_LSTRING .var, .len, .val {
  +ASSIGN_U16V_EQ_ADDR s_ptr, .var
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
!pet .len, .val, $00  ; length-encoded in first byte
}

!macro SET_STRING .var, .val {
  +ASSIGN_U16V_EQ_ADDR s_ptr, .var
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
!pet .val, $00
}

!macro SET_STRING_PTR .var, .val {
  +ASSIGN_U16V_EQ_U16V s_ptr, .var
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  jsr append_inline_text_to_str
!pet .val, $00
}

!macro ASSIGN_STRING_PTR_TO_IMM .var, .val {
  bra +
-:
!pet .val, $00
+:
  +ASSIGN_U16V_EQ_ADDR .var, -
}

!macro ASSIGN_U8V_EQ_IMM .dest, .val {
  lda #.val
  sta .dest
}

!macro ASSIGN_U8V_EQ_U8V .dest, .src {
  lda .src
  sta .dest
}

!macro ASSIGN_U32V_EQ_ADDR .dest, .addr1, .addr2 {
  lda #<.addr2
  sta .dest
  lda #>.addr2
  sta .dest+1
  lda #<.addr1
  sta .dest+2
  lda #>.addr1
  sta .dest+3
}

!macro ASSIGN_U16V_EQ_IMM .var, .val16 {
    lda #<.val16
    sta .var
    lda #>.val16
    sta .var+1
}

!macro ASSIGN_U16V_EQ_ADDR .dest, .addr {
  lda #<.addr
  sta .dest
  lda #>.addr
  sta .dest+1
}

!macro ASSIGN_U16V_EQ_U16V .dest, .src {
  lda .src
  sta .dest
  lda .src+1
  sta .dest+1
}

!macro ASSIGN_U16V_EQ_DEREF_U16V .dest, .src {
  ldy #$00
  lda (.src),y
  sta .dest
  iny
  lda (.src),y
  sta .dest+1
}

!macro ASSIGN_U16V_EQ_DEREF_U16V_WORDARRAY_AT_WORDIDX_OF_U8V .dest, .wordarray, .idx {
; .dest = .wordarray[.idx]
  +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_U8V .wordarray, .idx
  lda tmp_ptr
  sta .dest
  lda tmp_ptr+1
  sta .dest+1
}

!macro ALLOC .inptr, size {
  lda HEAPPTR
  sta .inptr
  lda HEAPPTR+1
  sta .inptr+1

  clc
  lda HEAPPTR
  adc #<size
  sta HEAPPTR
  lda HEAPPTR+1
  adc #>size
  sta HEAPPTR+1
}

!macro DYN_ALLOC_BYTES .var, .size {
  lda #<.size
  ldz #>.size
  +DYN_ALLOC_BYTES_SIZE_ZA .var
}

!macro DYN_ALLOC_BYTES_SIZE_ZA .var {
  jsr alloc_bytes
  stx .var
  sty .var+1
}


!macro CMP_S_PTR_TO_PSTR .ptr {
    ldx .ptr
    ldy .ptr+1
    lda s_ptr
    ldz s_ptr+1
    jsr instr
}

!macro CMP_S_PTR_TO_STR .str {
    ldx #<.str
    ldy #>.str
    lda s_ptr
    ldz s_ptr+1
    jsr instr
}

!macro CMP_STR_TO_IMM .var, .val {
  +ASSIGN_U16V_EQ_ADDR s_ptr, .var
  +CMP_S_PTR_TO_IMM .val
}

!macro CMP_PSTR_TO_IMM .ptr, .val {
  +ASSIGN_U16V_EQ_U16V s_ptr, .ptr
  +CMP_S_PTR_TO_IMM .val
}

!macro CMP_PPSTR_TO_PSTR .pptr, .ptr {
  +ASSIGN_U16V_EQ_DEREF_U16V is_ptr, .pptr
  ldx is_ptr
  ldy is_ptr+1
  lda .ptr
  ldz .ptr+1
  jsr streq
}

!macro CMP_PPSTR_TO_IMM .pptr, .val {
  +ASSIGN_U16V_EQ_DEREF_U16V s_ptr, .pptr
  +CMP_S_PTR_TO_IMM .val
}

!macro INSTR_CHR_U8V_IN_STR .var, .chr {
  ldx #<.var
  ldy #>.var
  lda .chr
  jsr instr_chr
}

!macro INSTR_PSTR_TO_IMM_CHAR .ptr, .immchr {
  ldx .ptr
  ldy .ptr+1
  lda #.immchr
  jsr instr_chr
}

!macro INSTR_S_PTR_TO_STR .var {
  ldx #<.var
  ldy #>.var
  lda s_ptr
  ldz s_ptr+1
  jsr instr
}

!macro INSTR_S_PTR_TO_IMM .str {
  bra +
@imm_str:
!pet .str, $00
+:
;   if instr(cur_src_line$, "ifdef") = 2 then begin
    ldx #<@imm_str
    ldy #>@imm_str
    lda s_ptr
    ldz s_ptr+1
    jsr instr
}

; todo: consider how this streq: logic compares to cmp_tmp_ptr_to_s_str:
!macro CMP_S_PTR_TO_IMM .str {
  bra +
@imm_str:
!pet .str, $00
+:
;   if instr(cur_src_line$, "ifdef") = 2 then begin
    ldx #<@imm_str
    ldy #>@imm_str
    lda s_ptr
    ldz s_ptr+1
    jsr streq
}

!macro ADD_TO_POINTER_U8 .ptr, .val {
  clc
  lda .ptr
  adc #.val
  sta .ptr
  lda .ptr+1
  adc #$00
  sta .ptr+1
}


!macro add_to_pointer_u16 .ptr, .val {
  clc
  lda .ptr
  adc #<.val
  sta .ptr
  lda .ptr+1
  adc #>.val
  sta .ptr+1
}


!macro plw .ptr {
  pla
  sta .ptr+1
  pla
  sta .ptr
}

!macro toggle_flag .flag {
  lda .flag
  eor #$01
  sta .flag
}



// -------------------
// zero page variables
// -------------------
ret_ptr_lo = $06  // (low-byte) address to return to after call to print_inline_text
ret_ptr_hi = $07  // (high-byte) address to return to after call to print_inline_text
tr_ptr = $08  ; $08-$09
s_ptr = $0a   ; $0a-$0b
is_ptr = $0c  ; $0c-$0d
tmp_ptr = $0e ; $0e-$0f
needle_ptr = $10 ; $10-$11
haystack_ptr = $12 ; $12-13
a_ptr = $14 ; $14-$15
sr_ptr = $16 ; $16-17

FOURPTR = $18  // $18-$1B
SRCPTR = $1c   // $1C-$1F
verbose = $20  // byte
cur_src_lineno = $21  ; $21-$22
HEAPPTR = $23 ; $23-$24  (pointer to free location of heap)
             ; (I'll try start it at $a000 and grow it upwards)
TMPHEAPPTR = $25 ;  $25-$26
TESTPTR = $27 ; $27-$28

tmp2_ptr = $29 ; $29-$2a

;---------
initialise:
;---------
  sei

  lda #$00
  sta FOURPTR
  sta FOURPTR+1
  sta FOURPTR+3
  lda #$04
  sta FOURPTR+2

  +ASSIGN_U16V_EQ_IMM HEAPPTR, $a000

  ; allocate label_name$(200)
  +ALLOC label_name, 200*2

  lda #$00
  sta label_cnt

  ; allocate label_lineno(200)
  +ALLOC label_lineno, 200*2

  lda #$00
  sta element_cnt
  sta element_cnt+1
  sta element_cnt+2
  sta element_cnt+3

  sta struct_cnt

; #declare var_table$(4,200)             ' variable table per type
  ; ptr to memory for array (4*200*2 = 1600 bytes in size, $640)
  +ALLOC var_table, 4*200*2

  jsr init_tokens

  tsx
  stx bailout_stack_pos

!ifdef RUN_TESTS {
  jsr run_tests
} else {
  jsr main
}

bail_out_early:
  ldx bailout_stack_pos
  txs

  cli
  rts

bailout_stack_pos:
!byte $00

!ifdef RUN_TESTS {
!source "11.parse.test.asm"
}

; '-------
; DEFINES
; '-------
TYP_REAL = 0
TYP_INT = 1
TYP_STR = 2
TYP_BYTE = 3
TYP_DEF = 4

CHROUT = $ffd2
CLOSE_ALL = $ff50
GETLFS = $ff44

MULDIVBUSY = $d70f
DIVOUT  = $d768
DIVOUT2 = $d76c
MULTINA = $d770
MULTINB = $d774
MULTOUT = $d778

; 
;   clr
; 
; #declare dbl_quote$, parser_file$, t$, blank_line$, type_ident$
dbl_quote:
!pet '"', $00
!byte $00
type_ident:
!pet $00  ; type_ident$(TYP_REAL)=""
!pet '%'  ; type_ident$(TYP_INT)="%"
!pet '$'  ; type_ident$(TYP_STR)="$"
!pet '&'  ; type_ident$(TYP_BYTE)="&"

; #declare next_line$
cur_src_line:
!fill 256, $00
next_line_flag:
!byte $00
next_line:
!fill 256, $00
cur_char:
!byte $00

; #declare mk$, dbl_quote_char, sngl_quote_char
; #declare tokens$(10), dumb_cmds$, i
dumb_cmds:
!fill 256, $00
tokens:
TKN_STR_CNT = 7
!fill TKN_STR_CNT*256, $00   ; allocated strings
; #declare bin_conv(16)  ' bit shifter's fast binary conversion
; 
; #declare map_dest_to_src_lineno%(1000) ' dest file line nr --> src file line nr
; #declare dest_line$(1000)              ' post processed lines
; #declare element_cnt(4)                ' element count per type
element_cnt:
!byte $00, $00, $00, $00, $00

; #declare var_table$(4,200)             ' variable table per type
var_table = $1600   ; ptr to memory for array (4*200*2 = 1600 bytes in size, $640)

; #declare define_val$(200)              ' define values table
define_val:
!fill 200*2, $00  ; memory for array of string pointers

; #declare args$(32)                     ' argument list
args:
!fill 32*2    ; pointers to strings in tempheap
tempheap:
!fill 32*64   ; each string is 64bytes max

; next avail = $1e10

; 
; #declare label_name$(200)              ' label table
label_name:
!word $0000   ; pointer to where array is allocated in heap

; #declare label_lineno(200)
label_lineno:
!word $0000   ; pointer to array

; #declare label_cnt=0
label_cnt:
!byte $00

; 
; #declare struct_name$(30)
struct_name:
!fill 30*2, $00   ; array of pointers to strings

; #declare struct_fields$(30)
struct_fields:
!fill 30*2, $00   ; array of pointers to strings

; #declare struct_vars$(30)              ' struct vars (each entry has string of vars)
struct_vars:
!fill 30*2, $00   ; array of pointers to strings

; #declare struct_cnt=0                  ' index to next free struct definition
struct_cnt:
!byte $00

; 
; #declare cb
whitespace:
!byte 32, 160, $1d, $09, $00
space_only:
!pet ' ', $00

; #declare cur_attic_addr, total_lines, s$
total_lines:
!word $0000
delete_line_flag:
!byte $00

; #declare verbose, z$, cur_dest_line$
inside_ifdef:
!byte $00
dest_lineno:
!word $0000
; #declare cur_dest_lineno, s1, s2, cur_tok$, r, f$, delim$
cur_tok:
!fill 256, $00  ; length-encoded string of max 255 bytes
define_flag:
!byte 00

f_str:
!fill 256, $00  ; string of max 256 bytes

; #declare var_name$
var_name:
!word $0000 ; ptr to string
parser_error:
!fill 256, $00
arg_cnt:
!byte 00
ignore_brackets:
!byte $00
dimension:
!word $0000 ; ptr to string
value:
!word $0000 ; ptr to string
bkt_open_idx:
!byte $00
bkt_close_idx:
!byte $00
equals_idx:
!byte $00

; #declare tr$, hx$, bi$, ty, id, gen_varname$, of$
outfile:  ; old of$
!fill 32, $00
gen_varname:
!fill 4, $00
ty:
!byte $00
dont_mark_label:
!byte $00
; #declare args_list$, args_list_len, args_idx, cur_ch$
ignore_delim_flag:
!byte $00
did_replace_flag:
!byte $00
quote_flag:
!byte $00
; #declare expecting_label, default_delim$, rv_idx
rv_idx:
!byte $00
expecting_label:
!byte $00
a_str:
!fill 256, $00  ; length-encoded string of max 255 bytes
; #declare t, found_struct_idx, br, b, bc$, vl
lc:
!fill 16, $00
; #declare n1, n2, ba, a, ad, tf$, found_idx, k, sf$, sf
found_idx:
!byte $00
sf_str:
!fill 256, $00
sf:
!byte $00
n1:
!byte $00
n2:
!byte $00
; #declare src_line_ptr, src_linebuff_ptr
cur_line_len:
!byte $00
cut_tail_idx:
!byte $00

; #declare cur_line_len_minus_one, cur_linebuff_addr, chr
; #declare orig$, ridx, struct_obj_name$, bkt_open_idx, sz, sr, sm, zz$
field_count:   ; old sz
!byte 00
struct_obj_name:
!word $0000   ; ptr to string   ; old struct_obj_name$
orig_ptr:
!word $0000   ; orig$
ridx:
!byte $00
; #declare cont_next_line_flag, tok_name$, clean_varname$
tok_name:
!fill 32, $00
; #declare shitty_syntax_flag, zz
shitty_syntax_flag:
!byte $00
; 

dmatable:
!byte $80   ; source addr MB selector
!byte $80   ; $(80)3.0000
!byte $00   ; end of options
dmatable_cmd:
!byte $00 ; cmd
dmatable_len:
!word $0000
dmatable_src_addr:
!word $0000
dmatable_src_bank:
!byte $00
dmatable_dest_addr:
!word $0000
dmatable_dest_bank:
!byte $00
dmatable_cmd_msb:
!byte $00
dmatable_modulo:
!word $0000

;----------------
print_inline_text:
;----------------
  tsx
  inx
  lda $0100,x
  clc
  adc #$01
  sta ret_ptr_lo
  inx
  lda $0100,x
  adc #$00
  sta ret_ptr_hi

  jsr print_text

  tsx
  inx
  lda ret_ptr_lo
  sta $0100,x
  inx
  lda ret_ptr_hi
  sta $0100,x
  rts

;------------------------
append_inline_text_to_str:
;------------------------
  tsx
  inx
  lda $0100,x
  clc
  adc #$01
  sta ret_ptr_lo
  inx
  lda $0100,x
  adc #$00
  sta ret_ptr_hi

  jsr append_text_to_str

  tsx
  inx
  lda ret_ptr_lo
  sta $0100,x
  inx
  lda ret_ptr_hi
  sta $0100,x

   rts


; todo: quite similar to print_str, decide which one is better to keep
; (so far, I think print_str is shorter/faster)
;---------
print_text:
;---------
  ldx #$00
  lda (ret_ptr_lo,x)
  beq @found_null

  jsr CHROUT
  inc ret_ptr_lo
  bne print_text
  inc ret_ptr_hi
  bne print_text

@found_null:
  rts


;-------------------
append_char_to_str:
;-------------------
  cmp #$00
  bne +
  rts     ; don't add null-term

+:
;  input: regA = char to add
  ldy cur_line_len
  sta (s_ptr),y
  inc cur_line_len

  ldy cur_line_len ; null term
  lda #$00
  sta (s_ptr),y

  rts

;-----------------
append_text_to_str:
;-----------------
  ldx #$00
  lda (ret_ptr_lo,x)
  beq @found_null

  ldy cur_line_len
  sta (s_ptr),y
  inc cur_line_len

  inc ret_ptr_lo
  bne append_text_to_str
  inc ret_ptr_hi
  bne append_text_to_str

@found_null:
  ldy cur_line_len  ; add null terminator
  lda #$00
  sta (s_ptr),y
  rts


;----------------------
append_left_text_to_str:
;----------------------
; input: z = number of left chars to copy over
@loop:
  cpz #$00
  beq @bail_out
  dez

  ldx #$00
  lda (ret_ptr_lo,x)
  beq @bail_out

  ldy cur_line_len
  sta (s_ptr),y
  inc cur_line_len

  inc ret_ptr_lo
  bne @loop
  inc ret_ptr_hi
  bne @loop

@bail_out:
  ldy cur_line_len  ; add null terminator
  lda #$00
  sta (s_ptr),y
  rts


;-------------------
cmp_tmp_ptr_to_s_str:
;-------------------
  ; returns C=0 if equal, C=1 if unequal
  clc
  ldy #$0
@loop:
  lda (tmp_ptr),y
  beq @bail_out_on_null_term

  cmp (s_ptr),y
  bne @bail_out_fail

  lda (s_ptr),y
  beq @bail_out_fail

  iny
  bra @loop

@bail_out_fail:
  sec
  rts

@bail_out_on_null_term:
  lda (s_ptr),y
  bne @bail_out_fail  ; if other string doesn't have a null-terminator too, then fail
  clc
  rts

cmp16res:
!word $0000

cmp16val:
!word $0000


;----
cmp16:
;----
  stx cmp16res
  sty cmp16res+1

  sec
  txa
  sbc cmp16val
  sta cmp16res

  tya
  sbc cmp16val+1
  sta cmp16res

  rts


;----
log10:
;----
  ; input: x=lo-byte, y=hi-byte

  ; if val < 10 return 0
  lda #$0a
  sta cmp16val
  lda #$00
  sta cmp16val+1
  jsr cmp16
  bcs +
  lda #$00
  rts

+:
  ; if val < 100 return 1
  lda #$64
  sta cmp16val
  lda #$00
  sta cmp16val+1
  jsr cmp16
  bcs +
  lda #$01
  rts

+:
  ; if val < 1000 return 2
  lda #$e8
  sta cmp16val
  lda #$03
  sta cmp16val+1
  jsr cmp16
  bcs +
  lda #$02
  rts

+:
  ; if val < 10000 return 3
  lda #$10
  sta cmp16val
  lda #$27
  sta cmp16val+1
  jsr cmp16
  bcs +
  lda #$03
  rts

+:
  ; else return 4
  lda #$04
  rts

column_vals:
!word $0001   ; 1
!word $000a   ; 10
!word $0064   ; 100
!word $03e8   ; 1000
!word $2710   ; 10000

temp16:
!word $0000
column_val:
!word $0000
to_str_flag:
!byte $00

;---------------
get_column_value:
;---------------
; input: a = column digit
; output: column_val

  clc
  rol
  tax
  lda column_vals,x
  sta column_val
  lda column_vals+1,x
  sta column_val+1
  rts

;---------
print_uint:
;---------
; num_digits = abs( log10(val) + 1)
  jsr log10
  taz
  stx temp16
  sty temp16+1

@loop_next_digit:
; do
;   column_val = num_digits * 10
    phz
    tza
    jsr get_column_value

;   column_digit = abs(val / column_val)
    ldz #$00
    ldy #$00
    ldx temp16+1
    lda temp16
    stq MULTINA

    ldx column_val+1
    lda column_val
    stq MULTINB

    +WAIT_DIV_FINISH

    ; print this digit
    lda DIVOUT+4
    clc
    adc #'0'
    ldx to_str_flag
    bne @add_to_str
    jsr CHROUT
    bra @skip

@add_to_str:
    ldy cur_line_len
    sta (s_ptr),y
    inc cur_line_len

@skip:
;   value -= column_digit * column_val
    ldx #$00
    lda DIVOUT+4
    stq MULTINA

    +WAIT_MUL_FINISH

    sec
    lda temp16
    sbc MULTOUT
    sta temp16
    lda temp16+1
    sbc MULTOUT+1
    sta temp16+1

;   num_digits--
    plz
    dez
; while num_digits != 0
    bpl @loop_next_digit

  rts


;--------
print_str:
;--------
  ldy #$00
@loop_next_char:
  lda (s_ptr),y
  beq @bail_out
  jsr CHROUT
  iny
  jmp @loop_next_char

@bail_out:
  rts


;----------
init_tokens:
;----------
  +SET_STRING tokens + 0 * 256, " print input if then else do loop while until gosub goto open close dopen dclose for next getkey hex$ dim peek poke wait dec chr$ asc sgn sqr str$ graphic clr screen def begin bend len mid$ right$ left$ instr for next step trap border and foreground "

  +SET_STRING tokens + 1 * 256, " background set abs sin cos tan log fre cursor pixel window rwindow line box circle ellipse palette restore data err$ er el cursor on off val scratch return rnd stop bank ti do or st if el er on to pen get end int not ds run using dot "

  +SET_STRING tokens + 2 * 256, " append atn auto backup bload boot bsave bump bverify catalog change char cmd collision color concat cont copy wpoke wpeek setbit clrbit "

  +SET_STRING tokens + 3 * 256, " dclear deffn delete fn dir disk dload dma dmode dpat dsave dverify edma envelope erase exit exp fast filter find go64 header help highlight "

  +SET_STRING tokens + 4 * 256, " joy list load locate lpen mod monitor mouse movspr new paint play pointer polygon pos pot pudef "

  +SET_STRING tokens + 5 * 256, " rcolor rdot read record rem rename resume rgr rmouse rplay rreg rspcolor rsppos rsprite save scnclr sleep slow sound spc sprcolor "

  +SET_STRING tokens + 6 * 256, " sprite sprsav sys tab tempo troff tron type usr verify vol xor key vsync rcursor t@& c@& rgraphic fread pointer "

  +SET_STRING dumb_cmds, " bload bsave dload to save dir collect dopen dclose backup fread get "
  rts
 

;----
main:
;----
;   parser_file$ = "11.parse"
;   mk$ = "@{x7E}"  ' @ and pi
;   dbl_quote$ = chr$(34)
;   dbl_quote_char = 34
;   sngl_quote_char = 39
; 
;   key on
;   print "{x12}ELEVEN preprocessor v0.6.0{x92}"

  jsr print_inline_text
!pet $12, "ELEVEN preprocessor v0.6.1", $92, $0d, $00

;   print
  lda #$0d
  jsr CHROUT

;   key 7, "scratch" + dbl_quote$ + parser_file$ + dbl_quote$ + ":dsave" + dbl_quote$ + parser_file$ + dbl_quote$ + ":dverify" + dbl_quote$+parser_file$
; 
;   t$="                                                                               "
;   blank_line$ = t$ + t$ + t$
;   t$ = ""
; 

; 
;   gosub get_filename
  jsr get_filename

;   bank 128
; 
;   bin_conv(0) = 1
;   for i = 1 to 16
;     bin_conv(i) = bin_conv(i - 1) + bin_conv(i - 1)
;   next i
; 
; 
; ; ------------------------- pass 1 ------------------------------------
; 
;-----
pass_1:
;-----
;   next_line_flag = 0
  lda #$00
  sta next_line_flag

; 
;   print "pass 1 ";
  jsr print_inline_text
!pet "pass 1 ", $00

  lda #$0d
  jsr CHROUT
 
;   cur_src_lineno=0
  lda #$00
  sta cur_src_lineno
  sta cur_src_lineno+1

;   clr ti  ' keep start time for timing

;   cb = $8030000   ' eleven source to be compiled is located here in attic ram
    lda #$00
    sta SRCPTR
    sta SRCPTR+1
    lda #$03
    sta SRCPTR+2
    lda #$08
    sta SRCPTR+3

;   cur_attic_addr = cb
;   total_lines = wpeek(cur_attic_addr)
    ldz #$00
    lda [SRCPTR],z
    sta total_lines
    inz
    lda [SRCPTR],z
    sta total_lines+1

;   cur_attic_addr = cur_attic_addr + 2
    lda #$02
    sta SRCPTR
    ldz #$00

@loop_pass1:
;   do while cur_src_lineno <> total_lines  ' until target lines is reached
  lda cur_src_lineno
  cmp total_lines
  bne @condition_passed

  lda cur_src_lineno+1
  cmp total_lines+1
  lbeq @skip_pass1

@condition_passed:
;     gosub read_next_line
      jsr read_next_line

;     gosub single_quote_comment_trim
      jsr single_quote_comment_trim

;     ' strip whitespace from end
;     s$ = cur_src_line$
;     gosub strip_tr$_from_end
      jsr strip_tr_from_end

;     cur_src_line$ = s$

;     if cur_src_line$ <> "" then begin
      lda cur_line_len
      beq @skip_line_parse
;       delete_line_flag = 0
        +ASSIGN_U8V_EQ_IMM delete_line_flag, $00

        jsr verbose_dest_to_src_mapping

        jsr check_add_label

        jsr check_preproc_directive
; 
;       if inside_ifdef = 1 then goto parser_loop_skip
        +CMP_U8V_TO_IMM inside_ifdef, $01
        beq @parser_loop_skip

        jsr check_compulsory_next_line_cases

        jsr parse_standard_line

;     bend  ' endif cur_src_line$ <> ""
@skip_line_parse:
; 
@parser_loop_skip:
;     ' increase source code line (for error msgs...)
;     cur_src_lineno = cur_src_lineno + 1
      inw cur_src_lineno

      jsr verbose_src_line_info
;   loop
  jmp @loop_pass1

;   
;   if cur_dest_line$ <> "" then begin
;     dest_line$(dest_lineno) = cur_dest_line$
;     dest_lineno = dest_lineno + 1
;   bend
; 
;   close 1
;   gosub save_filename  ' set output filename
;   scratch "11temp"
;   scratch "11tokenized"
; 
@skip_pass1:

  rts

; ; ------------------------- pass 2 ------------------------------------
; 
; 
; '------
; .pass_2
; '------
;   open 1,1,5,"11temp,s,w"
;   print chr$(13) "{x11}pass 2 ";
; 
;   for cur_dest_lineno = 0 to dest_lineno - 1
;     s$ = dest_line$(cur_dest_lineno)
;     if verbose then print cur_dest_lineno; "{x9E}=> " s$ : else print ".";
; 
;     do while instr(s$, mk$) <> 0
;        s1 = instr(s$, mk$) : s2 = instr(s$, mk$, s1 + 2)
;        if s2 = 0 then begin
;          exit  ' exit loop for cases where a pi symbol was used in the source
;        bend:else begin
;          cur_tok$ = mid$(s$, s1 + 2, s2 - s1 - 2)
;          gosub swap_out_label_in_cur_tok$_for_lineno
;          s$ = left$(s$, s1 - 1) + cur_tok$ + mid$(s$, s2 + 2)
;        bend
;     loop
; 
;     if verbose then print "<= "; str$(cur_dest_lineno) + s$
;     print #1, str$(cur_dest_lineno) + " " + s$
;   next cur_dest_lineno
; 
;   gosub dump_vars
; 
;   for r = 0 to 10
;     print #1, str$(32000 + r)
;   next r  ' remove 11.tokenise.bas lines?
; 
;   f$ = "dC:dS" + dbl_quote$ + "11tokenized" + dbl_quote$
;   f$ = f$ + ":ifds<>0then?" + dbl_quote$ + "disc error: "
;   f$ = f$ + dbl_quote$ + ";ds$:else?" + dbl_quote$
;   f$ = f$ + "{x13}{x13}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}{x11}"
;   f$ = f$ + dbl_quote$ + ":dL" + dbl_quote$ + "11.post"
; 
;   print #1, f$
;   close 1
; 
;   print"{x11}"
;   print "elapsed time:"; ti; "ticks"
;   print"{x13}{x13}{x93}{x11}{x11}edma 0,$1fff,$8022000,$2001:new restore{x11}{x11}"
;   print"run{x13}";  ' load '11.tokenize' from cache
; 
;   bank 128
;   poke 208,2      ' no# of chars in keyboard buffer
;   poke 688,13,13  ' return chars
;   end


;----------------------
check_preproc_directive:
;----------------------
;       if left$(cur_src_line$, 1) = "#" then begin
        ldy #$00
        lda (s_ptr),y
        cmp #'#'
        bne +
;         gosub parse_preprocessor_directive
          jsr parse_preprocessor_directive
;       bend
+:
  rts


;--------------
check_add_label:
;--------------
;       if left$(cur_src_line$, 1) = "." then begin
      ldy #$00
      lda (s_ptr),y
      cmp #'.'
      bne @skip_add_label
;         next_line_flag = 1
        lda #$01
        sta next_line_flag
;         gosub add_to_label_table
        jsr add_to_label_table
;       bend
@skip_add_label:
  rts


;--------------------------
verbose_dest_to_src_mapping:
;--------------------------
;       if verbose then print ">> DEST:" dest_lineno;", SRC: "; cur_src_lineno;": "; cur_src_line$
        lda verbose
        beq @skip_verbose

          +PRINT_INLINE ">> DEST: "
          +PRINT_U16V dest_lineno
          +PRINT_INLINE ", SRC: "
          +PRINT_U16V cur_src_lineno
          +PRINT_CHR ':'
          +PRINT_CHR ' '
          +PRINT_PSTR s_ptr
          +PRINT_CHR $0d
@skip_verbose:
  rts


;--------------------
verbose_src_line_info:
;--------------------
;     if verbose then begin
      +CMP_U8V_TO_IMM verbose, $01
      bne @skip_verbose2
;       print "cur_src_lineno="; cur_src_lineno
        +PRINT_INLINE "cur_src_lineno="
        +PRINT_U16V cur_src_line
        +PRINT_CHR $0d
;       get key z$
;     bend
@skip_verbose2:
  rts


;-------------------------------
check_compulsory_next_line_cases:
;-------------------------------
;       if left$(cur_src_line$, 4) = "data" or right$(cur_src_line$, 5) = "begin" then begin
;         next_line_flag = 1
;       bend
  rts


;------------------
parse_standard_line:
;------------------
; input:
;  - delete_line_flag
; output:
;  - 

; if delete_line_flag = 0 then begin
  +CMP_U8V_TO_IMM delete_line_flag, $00
  bne @skip_df_zero
;   if verbose = 0 then print ".";
    +CMP_U8V_TO_IMM verbose, $00
    bne +
    +PRINT_CHR '.'
+:
;   s$ = cur_src_line$
;   gosub replace_vars_and_labels
    jsr replace_vars_and_labels
;   gosub check_for_creation_of_struct_object
    jsr check_for_creation_of_struct_object
;   gosub check_for_continue_onto_next_line
    jsr check_for_continue_onto_next_line
;   ' safe add cur_dest_line$+s$ to current or next dest_line$(dest_lineno)
;   gosub safe_add_to_current_or_next_line
    jsr safe_add_to_current_or_next_line
; 
;   if right$(s$, 4) = "bend" or right$(s$, 6) = "return" {x5F}
;       or left$(s$, 2) = "if" then begin
;     next_line_flag = 1
;   bend
; bend  ' endif delete_line_flag = 0
@skip_df_zero:
  rts


;----------------
declare_s_ptr_var:  ; declare_s$_var
;----------------
;   ' declare var(s) in s$
;  input:
;    - s$ (e.g. "#define FISHY=1" or "#declare a=1")
;         (if the former, define_flag=1. Latter is =0)
;  output:
;    - var_table gets populated
;    - define_vals get populated (if define_flag == 1)
;    - next_line (if variable is assigned or dimensioned)

;   s$ = mid$(cur_src_line$, 10 - define_flag)
    +UPDATE_S_PTR_TO_SKIP_DECL_OR_DEF

;   ignore_brackets = 1
    +ASSIGN_U8V_EQ_IMM ignore_brackets, $01

;   gosub parse_arguments
    jsr parse_arguments

;   ignore_brackets = 0  ' split parameters
    +ASSIGN_U8V_EQ_IMM ignore_brackets, $00
; 
;   next_line$ = ""  ' new line if dimensioning...
    sta next_line
; 
;   if arg_cnt < 0 then begin
    +CMP_U8V_TO_IMM arg_cnt, $ff
    bne +
    +SET_PARSER_ERROR_ON_LINE "?declare parameter missing in line "

;     goto return_to_editor_with_error
    jmp return_to_editor_with_error
;   bend
+:
; 
    +ASSIGN_U16V_EQ_U16V is_ptr, args

    lda #$00
    sta arg_idx
;   for i = 0 to arg_cnt
@loop_next_arg:
;     var_name$ = args$(i)
      +SET_VARNAME_EQ_ARGS_I
;     gosub parse_declared_var
      jsr parse_declared_var

;   next i
    inc arg_idx
    lda arg_idx
    cmp arg_cnt
    bne @loop_next_arg

;   if next_line$ <> "" then begin
    lda next_line
    beq @skip_to_else
;     delete_line_flag = 0
      +ASSIGN_U8V_EQ_IMM delete_line_flag, $00
;     cur_src_line$ = "^^" + next_line$
      +SET_STRING cur_src_line, "^^"
      +APPEND_STR_TO_S_PTR next_line
      bra @bail_out
;   bend : else begin
@skip_to_else:
;     delete_line_flag = 1
      +ASSIGN_U8V_EQ_IMM delete_line_flag, $01
;   bend
@bail_out:
;   return
    rts

arg_idx:
!byte $00

; 
; 
;-----------------
parse_declared_var:
;-----------------
; input: var_name$ (e.g. 'fishy = 1')
; output:
;   - var is added to var_table()
;   - any var declaration text is added to next_line$

;   dimension$ = ""
    lda #$00
    sta dimension
    sta dimension+1

;   value$ = ""
    sta value
    sta value+1

    jsr get_bkt_and_equals_indices

    jsr declare_assignment_check

    jsr declare_dimension_check

    jsr declare_type_check

;   var_table$(ty, element_cnt(ty)) = var_name$
    jsr add_varname_to_vartable

    jsr generate_dest_line_for_dimensioned_var

    jsr generate_dest_line_for_assigned_var

    jsr add_define_value_to_table

    jsr verbose_print_varname_and_el_cnt

;   element_cnt(ty) = element_cnt(ty) + 1
    +INCREMENT_ELCNT_OF_TY
;   return
    rts


;-------------------------------
verbose_print_varname_and_el_cnt:
;-------------------------------
;   if verbose then begin
    lda verbose
    beq +

;     print var_name$; "{x92}: "; element_cnt(ty)
      +ASSIGN_U16V_EQ_U16V s_ptr, var_name
      jsr print_str

      jsr print_inline_text
!pet $92, ": ", $00

      ldx ty
      lda element_cnt,x
      tax
      ldy #$00
      jsr print_uint

      lda #$0d
      jsr CHROUT
;   bend
+:
    rts

;------------------------
add_define_value_to_table:
;------------------------
; inputs:
;   - define_flag (only happens if it is set)
;   - ty = TYP_DEF
;   - value
; output:
;   - define_val[element_cnt(TYP_DEF)] = value

;   if define_flag = 1 then begin
    lda define_flag
    beq @skip_add_define_val
;     define_val$(element_cnt(ty)) = value$
      +SET_TMP_PTR_TO_DEFVALS_AT_ELCNT_IDX
      +HEAP_COPY_PSTR_EQ_PSTR tmp_ptr, value
;   bend
@skip_add_define_val:
    rts

;----------
alloc_bytes:
;----------
; input: ZA = 16-bit size to allocate
; output: YX = pointer to allocated bytes
  ldx HEAPPTR
  stx temp16
  ldy HEAPPTR+1
  sty temp16+1
  
  clc
  adc HEAPPTR
  sta HEAPPTR
  tza
  adc HEAPPTR+1
  sta HEAPPTR+1

  ldx temp16
  ldy temp16+1
  rts


;----------------------------------
generate_dest_line_for_assigned_var:
;----------------------------------
; input:
;    - value  (e.g. "10")
;    - ty     (type of var)
;    - define_flag (should be zero for any impact)
; output:
;    - next_line (e.g. "a=10:")

;   if value$ <> "" then begin
    lda value
    ora value+1
    beq @bail_out
;     id = element_cnt(ty)
      +ASSIGN_U8V_EQ_ELCNT_IDX_OF_TY elidx
;     gosub generate_varname 
      jsr generate_varname
;     if define_flag=0 then begin
      +CMP_U8V_TO_IMM define_flag, $00
;       next_line$ = next_line$ + gen_varname$ + t$ + "=" + value$ + ":"
        +ASSIGN_U16V_EQ_ADDR s_ptr, next_line
        jsr get_s_ptr_length
        +APPEND_STR_TO_S_PTR gen_varname
        +APPEND_TYPE_CHAR_TO_S_PTR
        +APPEND_IMM_CHR_TO_S_PTR '='
        +APPEND_PSTR_TO_S_PTR value
        +APPEND_IMM_CHR_TO_S_PTR ':'
;     bend
+:
;   bend
@bail_out:
    rts
; 

;-------------------------------------
generate_dest_line_for_dimensioned_var:
;-------------------------------------
;  input:
;    - dimension  (e.g., "10" of "fish$(10)")
;    - ty         (type of var)
;    - define_flag (should be zero for any impact)
;  output:
;    - next_line  (e.g. "dim ab$(10)")

;   if dimension$ <> "" then begin
    lda dimension
    ora dimension+1
    beq @bail_out
;     id = element_cnt(ty)
      +ASSIGN_U8V_EQ_ELCNT_IDX_OF_TY elidx
;     gosub generate_varname  ' fetch varname in gen_varname$
      jsr generate_varname

;     if define_flag = 0 then begin
      +CMP_U8V_TO_IMM define_flag, $00
      bne +
;       next_line$ = next_line$ + "dim " + gen_varname$ + t$ + "(" + dimension$ + "):"
        +ASSIGN_U16V_EQ_ADDR s_ptr, next_line
        jsr get_s_ptr_length
        +APPEND_INLINE_TO_S_PTR "dim "
        +APPEND_STR_TO_S_PTR gen_varname
        +APPEND_TYPE_CHAR_TO_S_PTR
        +APPEND_IMM_CHR_TO_S_PTR '('
        +APPEND_PSTR_TO_S_PTR dimension
        +APPEND_IMM_CHR_TO_S_PTR ')'
;     bend
+:
;   bend
@bail_out:
    rts

;-------------------------
get_bkt_and_equals_indices:
;-------------------------
;   bkt_open_idx = instr(var_name$, "(")
    lda #'('
    ldx var_name
    ldy var_name+1
    jsr instr_chr
    sta bkt_open_idx

;   bkt_close_idx = instr(var_name$, ")")
    lda #')'
    jsr instr_chr_quick
    sta bkt_close_idx

;   equals_idx = instr(var_name$, "=")
    lda #'='
    jsr instr_chr_quick
    sta equals_idx
    rts

;----------------------
add_varname_to_vartable:
;----------------------
; input: 
;  ty = the var type (string, int, byte, real, def)
;  var_name = "fishy$"
; output:
;  var_table(ty, element_count(ty) = var_name
;  

    ; var_table(,) = ptr to memory for array (4*200*2 = 1600 bytes in size, $640)
    +SET_IS_PTR_TO_VARTABLE_AT_TY_IDX
    +UPDATE_IS_PTR_TO_LATEST_ELEMENT_COUNT_IDX

    ; warning: i suspect that varname comes from temporary args
    ; (which will eventually be removed from the heap)
    ; it might be safer to copy the var_name contents to a new string
    ; (but then I need to reconsider my 'temp' use of the heap)
    +HEAP_COPY_PSTR_EQ_PSTR is_ptr, var_name
    rts


;-----------------
declare_type_check:
;-----------------
;   ty = TYP_REAL  ' var type
    +ASSIGN_U8V_EQ_IMM ty, TYP_REAL

;   t$ = right$(var_name$, 1)  ' type (if any) in t$
    +ASSIGN_U16V_EQ_U16V s_ptr, var_name
    jsr get_s_ptr_length
    
    ldy cur_line_len
    dey
    lda (s_ptr),y
    sta cur_char
; 
;   if verbose then begin
    lda verbose
    beq @skip_verbose
;     print "adding {x12}";
      jsr print_inline_text
!pet "adding ", $12, $00
;   bend
@skip_verbose:
; 
;   if instr("%&$", t$) = 0 then begin
    ldx #<vartype_delim
    ldy #>vartype_delim
    lda cur_char
    jsr instr_chr
    bcc @skip_check_real
;     t$ = ""
      +ASSIGN_U8V_EQ_IMM cur_char, $00
;     ty = TYP_REAL
      +ASSIGN_U8V_EQ_IMM ty, TYP_REAL
;   bend
@skip_check_real:

; 
;   if define_flag = 1 then begin
    lda define_flag
    beq +:
;     ty = TYP_DEF
      +ASSIGN_U8V_EQ_IMM ty, TYP_DEF
;   bend
+:
; 
;   if t$ = "%" then begin
    lda cur_char
    cmp #'%'
    bne +
;     ty = TYP_INT
      +ASSIGN_U8V_EQ_IMM ty, TYP_INT
;   bend
+:
; 
;   if t$ = "$" then begin
    cmp #'$'
    bne +
;     ty = TYP_STR
      +ASSIGN_U8V_EQ_IMM ty, TYP_STR
;   bend
+:
; 
;   if t$ = "&" then begin
    cmp #'&'
    bne +
;     ty = TYP_BYTE
      +ASSIGN_U8V_EQ_IMM ty, TYP_BYTE
;   bend
+:
    rts


;----------------------
declare_dimension_check:
;----------------------
; inputs:
;   - bkt_open_idx
;   - bkt_close_idx
;   - var_name   (e.g., "fish$(10)")
;
; outputs:
;   - var_name   (e.g. "fish$")
;   - dimension  (e.g. "10")  (if dimension was a declared (SIZE=10), then sub it)

;   if bkt_open_idx <> 0 and bkt_close_idx <> 0 then begin  ' --- dimension
    lda bkt_open_idx
    cmp #$ff
    beq @skip_found_brackets
    lda bkt_close_idx
    cmp #$ff
    beq @skip_found_brackets
;     dimension$ = mid$(var_name$, bkt_open_idx + 1, bkt_close_idx - bkt_open_idx - 1)
      clc
      lda var_name
      adc bkt_open_idx
      sta dimension
      lda var_name+1
      adc #$00
      sta dimension+1
      inc dimension
      bne +
      inc dimension+1
+:

      ; put a null-term on the close bracket
      +ASSIGN_U16V_EQ_U16V s_ptr, var_name
      lda #$00
      ldy bkt_close_idx
      sta (s_ptr),y

;     clean_varname$ = left$(var_name$, bkt_open_idx - 1)
      ldy bkt_open_idx
      sta (s_ptr),y

; 
;     s$ = dimension$
      +ASSIGN_U16V_EQ_U16V s_ptr, dimension

;     dont_mark_label = 1
      lda #$01
      sta dont_mark_label

;     gosub replace_vars_and_labels
      jsr replace_vars_and_labels

;     dont_mark_label = 0
      +ASSIGN_U8V_EQ_IMM dont_mark_label, $00
;     dimension$ = s$
; 
;     var_name$ = clean_varname$  ' check for define tokens
;     delete_line_flag = 0
      +ASSIGN_U8V_EQ_IMM delete_line_flag, $00
;   bend
@skip_found_brackets:
    rts


;-----------------------
declare_assignment_check:
;-----------------------
; inputs:
;   - equals_idx
;   - var_name   (e.g., "A = 1")
; outputs:
;   - var_name "A"
;   - value "1"

;   if equals_idx <> 0 then begin  ' --- assignment
    cmp #$ff
    beq @skip_found_equals
;     value$ = mid$(var_name$, equals_idx + 1)
      clc
      lda var_name
      adc equals_idx
      sta value
      lda var_name+1
      adc #$00
      sta value+1
      inc value
      bne +
      inc value+1
+:

;     var_name$ = left$(var_name$, equals_idx - 1)
      lda #$00
      ldy equals_idx
      sta (is_ptr),y
      sty cur_line_len
; 
;     tr$ = whitespace$
      +ASSIGN_U16V_EQ_ADDR tr_ptr, whitespace

;     s$ = var_name$
      +ASSIGN_U16V_EQ_U16V s_ptr, var_name

;     gosub strip_tr$_from_end
      jsr strip_tr_from_end

;     var_name$ = s$
; 
;     s$ = value$
      +ASSIGN_U16V_EQ_U16V s_ptr, value
      jsr get_s_ptr_length

;     gosub strip_tr$_from_beginning
      jsr strip_tr_from_beginning
;     gosub strip_tr$_from_end
      jsr strip_tr_from_end
;     value$ = s$
      +ASSIGN_U16V_EQ_U16V value, s_ptr
; 
; NOTE: Skip hex/binary checking for now, as latest rom permits such values
;     if left$(value$, 1) = "$" then begin
;       hx$ = mid$(value$, 2)
;       gosub check_hex
;     bend
; 
;     if left$(value$, 1) = "%" then begin
;       bi$ = mid$(value$, 2)
;       gosub check_binary
;     bend
; 
;   bend
@skip_found_equals:
    rts


vartype_delim:
!pet "%&$", $00

; 
;--------------
set_output_file:
;--------------
; input:
;   - cur_src_line  (e.g, '#output "myprog")
; output:
;   - outfile = "myprog"
;   
;   s$ = mid$(cur_src_line$, 8)
    +ADD_TO_POINTER_U8 s_ptr, 8
    +SUB_U8V_WITH_IMM cur_line_len, $08

;   gosub parse_arguments
    jsr parse_arguments
; 
;   if arg_cnt <> 0 then begin
    +CMP_U8V_TO_IMM arg_cnt, $ff
    bne +
;     print "?invalid parameters in line ";cur_src_lineno
;     end
    +SET_PARSER_ERROR_ON_LINE "?invalid parameters in line "

;     goto return_to_editor_with_error
    jmp return_to_editor_with_error
;   bend
+:
; 
;   s$ = args$(0)
    lda #$00
    sta arg_idx
    +SET_VARNAME_EQ_ARGS_I
;   tr$ = dbl_quote$
    +ASSIGN_U16V_EQ_ADDR tr_ptr, dbl_quote

;   gosub strip_tr$_from_beginning
    jsr strip_tr_from_beginning
;   gosub strip_tr$_from_end  ' quotes left & right
    jsr strip_tr_from_end
; 
;   if verbose then begin
    +CMP_U8V_TO_IMM verbose, $01
    bne @skip_verbose
;     print "setting output file to {x12}" + s$ + "{x92}"
    +PRINT_INLINE "setting output file to "
    +PRINT_CHR $12
    +PRINT_PSTR s_ptr
    +PRINT_CHR $92
    +PRINT_CHR $0D
;   bend
@skip_verbose:
; 
;   of$ = s$
    +COPY_TO_STR_FROM_S_PTR outfile

;   delete_line_flag = 1  ' disable passthrough
    +ASSIGN_U8V_EQ_IMM delete_line_flag, $01
;   return
    rts
; 
; 
;-----------------
add_to_label_table:
;-----------------
;   if verbose then begin
    lda verbose
    beq @skip_verbose
;     print "label "; cur_src_line$; " at line "; dest_lineno
      jsr print_inline_text
!pet "label ", $00

      jsr print_str

      jsr print_inline_text
!pet " at line ", $00

      ldx dest_lineno
      ldy dest_lineno+1
      jsr print_uint

      lda #$0d
      jsr CHROUT
;   bend
@skip_verbose:
; 
;   delete_line_flag = 1
    lda #$01
    sta delete_line_flag

;   label_name$(label_cnt) = mid$(cur_src_line$, 2)

    ; temp16 = label_cnt * 2  (as each entry in array is a word)
    lda label_cnt
    clc
    rol
    sta temp16
    lda #$00
    rol
    sta temp16+1

    ; tmp_ptr is pointer to indexed address within label_name$()
    clc
    lda label_name
    adc temp16
    sta tmp_ptr
    lda label_name+1
    adc temp16+1
    sta tmp_ptr+1

    ; set tmp_ptr word to equal the current heap pointer
    ldy #$00
    lda HEAPPTR
    sta (tmp_ptr),y
    iny
    lda HEAPPTR+1
    sta (tmp_ptr),y

    ; copy label name into heap
    ldx #$00
    ldy #$01
@loop_next_char:
    lda (s_ptr),y
    sta (HEAPPTR,x)
    pha
    inw HEAPPTR
    iny
    pla
    bne @loop_next_char

;   label_lineno(label_cnt) = dest_lineno + 1
    ; tmp_ptr is pointer to indexed address within label_lineno()
    clc
    lda label_lineno
    adc temp16
    sta tmp_ptr
    lda label_lineno+1
    adc temp16+1
    sta tmp_ptr+1

    ldy #$00
    lda dest_lineno
    clc
    adc #$01
    sta (tmp_ptr),y
    iny
    lda dest_lineno+1
    adc #$00
    sta (tmp_ptr),y

;   label_cnt = label_cnt + 1  ' increase label count
    inc label_cnt

;   return
    rts
; 
; 
;--------------------------
return_to_editor_with_error:
;--------------------------
;   bank 4  ' set error mailbox flag
    lda #$30
    sta FOURPTR
    lda #$ff
    sta FOURPTR+1
; 
    ldz #$ff
;   for r=1 to len(parser_error$)
@loop_next_char:
    inz
    lda (s_ptr),z
;     poke $4ff30 + r - 1, asc(mid$(parser_error$, r, 1))
    sta [FOURPTR],z

;   next r
;   poke $4ff30 + r - 1, 0
    bne @loop_next_char:

    ; decrement by one, as editor treats first line as line 0
    dew cur_src_lineno

    lda #$00
    sta FOURPTR
;   poke $ff09, mod(cur_src_lineno, 256)
    lda cur_src_lineno
    ldz #$09
    sta [FOURPTR],z

;   poke $ff0a, cur_src_lineno / 256
    lda cur_src_lineno+1
    inz
    sta [FOURPTR],z

;   poke $ff07, peek($ff07) or 2  ' set autojump flag
    ldz #$07
    lda [FOURPTR],z
    ora #$02
    sta [FOURPTR],z

;   dclose
    jsr GETLFS
    txa
    jsr CLOSE_ALL
;   goto chain_editor
    jmp chain_editor


;---------------
get_s_ptr_length:
;---------------
  ldy #$00
@loop_next_char:
  lda (s_ptr),y
  beq +
  iny
  bra @loop_next_char

+:
  sty cur_line_len
  rts
  

;-----------------------
strip_tr_from_beginning:
;-----------------------
;   ' -- strip tr$ from beginning of string in s$ --
  ldx #$00

@loop_next_char:
;   do while instr(tr$, (left$(s$, 1)))
  lda (s_ptr,x)
  sta cur_char
  ldy #$00
@loop_next_tr_char:
    lda (tr_ptr),y
    beq @bail_out

    cmp cur_char
    beq @found_tr_char
    iny
    jmp @loop_next_tr_char

@found_tr_char:
;     s$ = mid$(s$, 2)
    inw s_ptr
    dec cur_line_len
;   loop
  jmp @loop_next_char

@bail_out
;   return
  rts
; 
; 
;----------------
strip_tr_from_end:
;----------------
;   ' -- strip characters in tr$ from end of s$ --
  ldy cur_line_len
  beq @bail_out
;   do while instr(tr$, right$(s$, 1))
@loop_next_char:
  dey
  lda (s_ptr),y
  sta cur_char

  ldz #$00
@loop_next_tr_char:
  lda (tr_ptr),z
  beq @bail_out

  cmp cur_char
  beq @found_tr_char
  inz
  jmp @loop_next_tr_char

@found_tr_char:
  dec cur_line_len
  jmp @loop_next_char

;     s$ = left$(s$, len(s$) - 1)
;   loop

@bail_out:
  ldy cur_line_len
  lda #$00
  sta (s_ptr),y
  rts

delim:
!pet ",;", $00


;-------------------------
put_cur_arg_ptr_into_s_ptr:
;-------------------------
  lda arg_idx
  clc
  rol
  tax

  lda args,x
  sta s_ptr
  inx
  lda args,x
  sta s_ptr+1
  rts


;-------------------------
put_s_ptr_into_cur_arg_ptr:
;-------------------------
  lda arg_idx
  clc
  rol
  tax

  lda s_ptr
  sta args,x
  inx
  lda s_ptr+1
  sta args,x
  rts


;--------------
add_trimmed_arg:
;--------------
; input:
;   - arg_idx
;   - args[arg_idx] containing pointer to string like "  a = 1  "
; output:
;   - args[arg_idx] containing pointer to string like "a=1"
;
;       s$ = args$(arg_cnt)
;       tr$ = " "
        lda #<space_only
        sta tr_ptr
        lda #>space_only
        sta tr_ptr+1

        phw s_ptr   ; preserve original s_ptr
        lda cur_line_len
        pha

        lda arg_cnt
        sta arg_idx
        jsr put_cur_arg_ptr_into_s_ptr
        jsr get_s_ptr_length

;       gosub strip_tr$_from_beginning
        jsr strip_tr_from_beginning
;       gosub strip_tr$_from_end
        jsr strip_tr_from_end

;       args$(arg_cnt) = s$
        jsr put_s_ptr_into_cur_arg_ptr

        pla
        sta cur_line_len
        +plw s_ptr
        rts

chr_idx:
!byte $00
cur_arg_len:
!byte $00

;--------------
parse_arguments:
;--------------
;   ' -- parse arguments --
;   '     in: s$ = string   (e.g., "a=1, b=2")
;   '    out: args$(x) = argument list, arg_cnt = argument count
;   '         args$(0) = first arg, args$(1) = second arg...
;                        (e.g., arg$(0) = "a=1", arg$(1) = "b=2")
;   
;   delim$ = ",;"
; 
;   arg_cnt = 0
    +ASSIGN_U8V_EQ_IMM arg_cnt, $00

;   args_list$ = s$
;   args_list_len = len(s$)
;   ignore_delim_flag = 0
    +ASSIGN_U8V_EQ_IMM ignore_delim_flag, $00
; 
;   if args_list_len = 0 then begin
    lda cur_line_len
    bmi +
    bne ++
;     arg_cnt = -1
+:
      lda #$ff
      sta arg_cnt

;     return  ' no string
      rts
;   bend
++:
; 
    +ASSIGN_U16V_EQ_ADDR TMPHEAPPTR, tempheap

    ldy #$00
    lda #$00
@loop_clr_ptrs:
;   for arg_idx = 0 to 31
;     args$(arg_idx) = ""
      sta args,y
      iny
;   next arg_idx
    cpy #64
    bne @loop_clr_ptrs

    ; set args$(0) to point at latest TMPHEAPPTR
    +ASSIGN_U16V_EQ_U16V args, TMPHEAPPTR
    +ASSIGN_U16V_EQ_U16V tmp_ptr, args
    +ASSIGN_U8V_EQ_IMM cur_arg_len, $00

    ldy #$00
    sty chr_idx
@loop_next_char:
;   for chr_idx = 1 to args_list_len
;     cur_ch$ = mid$(args_list$, chr_idx, 1)
      ldy chr_idx
      lda (s_ptr),y
      sta cur_char
;     if cur_ch$ = "(" and ignore_brackets = 1 then ignore_delim_flag = 1
      cmp #'('
      bne +
      ldx ignore_brackets
      beq +

        ldx #$01
        stx ignore_delim_flag
+:

;     if cur_ch$ = ")" and ignore_brackets = 1 then ignore_delim_flag = 0
      cmp #')'
      bne +
      ldx ignore_brackets
      beq +
        
        ldx #$00
        stx ignore_delim_flag
+:
; 
;     if instr(delim$, cur_ch$) = 0 or ignore_delim_flag = 1 then begin
      ldz #$00  ; count of successful clauses
      +INSTR_CHR_U8V_IN_STR delim, cur_char
      bcc +
        inz
+:
      ldx ignore_delim_flag
      cpx #$01
      bne +
        inz

+:
      cpz #$00
      beq @skip_to_else
;       args$(arg_cnt) = args$(arg_cnt) + cur_ch$
        ldx #$00
        lda cur_char
        ldy cur_arg_len
        sta (tmp_ptr),y
        iny
        lda #$00
        sta (tmp_ptr),y
        sty cur_arg_len

        inw TMPHEAPPTR

        jmp @skip_after_else

;     bend : else begin
@skip_to_else:  ; we found an arg
    jsr add_trimmed_arg

; 
;       arg_cnt = arg_cnt + 1
        inc arg_cnt

        ; copy across next temp heap pointer to next arg
        lda arg_cnt
        clc
        rol
        tay

        ; +ASSIGN_U16V_EQ_U16V args, TMPHEAPPTR
        ; +ASSIGN_U16V_EQ_U16V tmp_ptr, args
        inw TMPHEAPPTR
        lda TMPHEAPPTR
        sta args,y
        sta tmp_ptr
        iny
        lda TMPHEAPPTR+1
        sta args,y
        sta tmp_ptr+1

        +ASSIGN_U8V_EQ_IMM cur_arg_len, $00
;     bend
@skip_after_else:

;   next chr_idx
    inc chr_idx
    ldy chr_idx
    cpy cur_line_len
    lbne @loop_next_char
; 
    jsr add_trimmed_arg
    ;   s$ = args$(arg_cnt)
    ;   tr$=" "
    ;   gosub strip_tr$_from_beginning
    ;   gosub strip_tr$_from_end
    ;   args$(arg_cnt) = s$

    ; increment arg_cnt
    inc arg_cnt

;   s$ = args_list$  ' restore s$

;   return
    rts

;   default_delim$ = "?<>=+-#*/^,.:;() "
default_delim:
!pet "?<>=+-#*/^,.:;() ",$00
default_plus_dpub_delim:
!pet "?<>=+-#*/^,.:;() dpub",$00

orig_sptr:
!word $000

;----------------------
replace_vars_and_labels:
;----------------------
;   ' -- replace vars & labels in source string --
;   '    in:   s$ = source string (which is really f_str)
;   '    out:  s$ = dest string with replaced items

    lda s_ptr
    sta orig_sptr
    lda s_ptr+1
    sta orig_sptr+1

    +ASSIGN_U16V_EQ_ADDR sr_ptr, f_str

;   if left$(s$, 2) = "^^" then begin
    ldy #$00
    lda (s_ptr),y
    cmp #'^'
    bne +
    iny
    lda (s_ptr),y
    cmp #'^'
    bne +
;     s$ = right$(s$,len(s$)-2)
      inc s_ptr
      inc s_ptr
;     return
      rts
;   bend
+:

;   quote_flag = 0
    lda #$00
    sta quote_flag

;   a$ = ""
    sta a_str

;   cur_tok$ = ""
    sta cur_tok
; 
;   shitty_syntax_flag = 0
    sta shitty_syntax_flag
; 
;   expecting_label = 0
    sta expecting_label

;   default_delim$ = "?<>=+-#*/^,.:;() "
;   delim$ = default_delim$
    +ASSIGN_U16V_EQ_ADDR is_ptr, default_delim

    jsr get_s_ptr_length
; 
    lda #$00
    sta rv_idx
;   for rv_idx = 1 to len(s$)
@loop_next_char:
      ldy rv_idx
      cpy cur_line_len
      lbeq @bail_for_loop

;     cur_ch$ = mid$(s$, rv_idx, 1)
      lda (s_ptr),y
      sta cur_char

      jsr shitty_syntax_checks

      ; assess if a starting double-quote appeared
      ; (this implies that the prior content is a token)

      jsr dbl_quote_check
      bcs @rval_for_continue

;     if instr(delim$, cur_ch$) <> 0 then begin
      lda cur_char
      jsr instr_chr_quick
      bcs @skip_to_delim_check_else

;       a$ = a$ + cur_tok$
;       cur_tok$ = ""
        jsr add_subbed_curtok_to_astr
;       if cur_ch$ = " " then cur_ch$ = ""
        lda cur_char
        cmp #' '
        bne +
          +ASSIGN_U8V_EQ_IMM cur_char, $00
+:
;       a$ = a$ + cur_ch$
        jsr add_curchar_to_astr
        bra @skip_delim_check
;     bend : else begin
@skip_to_delim_check_else:
;       cur_tok$ = cur_tok$ + cur_ch$
        jsr add_curchar_to_curtok
;     bend
@skip_delim_check:
; 
@rval_for_continue:
;   next
    inc rv_idx
    jmp @loop_next_char
    
@bail_for_loop:

    ; s$ = a$ + cur_tok$
    jsr add_subbed_curtok_to_astr

    +COPY_PSTR_FROM_STR orig_sptr, a_str+1
;   return
    rts


;-------------------
shitty_syntax_checks:
;-------------------
;     if cur_ch$ = ":" and quote_flag = 0 then begin
      cmp #':'
      bne +
      lda quote_flag
      bne +
;       shitty_syntax_flag = 0
        +ASSIGN_U8V_EQ_IMM shitty_syntax_flag, $00
;     bend
+:

;     if shitty_syntax_flag and cur_ch$ = "(" then begin
      lda shitty_syntax_flag
      beq +
      lda cur_char
      cmp #'('
      bne +
;       delim$ = default_delim$
        +ASSIGN_U16V_EQ_ADDR is_ptr, default_delim
;     bend
+:

;     if shitty_syntax_flag and cur_ch$ = ")" then begin
      lda shitty_syntax_flag
      beq +
      lda cur_char
      cmp #'('
      bne +
;       delim$ = default_delim$ + "dpub"
        +ASSIGN_U16V_EQ_ADDR is_ptr, default_plus_dpub_delim
;     bend
+:
      rts


;------------------------
add_subbed_curtok_to_astr:
;------------------------
; input: cur_tok
; output:
;   - astr += subbed cur_tok
;   - cur_tok = ""

  lda cur_char
  pha
  lda cur_line_len
  pha
  phw s_ptr
  phw is_ptr

; gosub check_token_for_subbing
  jsr check_token_for_subbing
; a$ = a$ + cur_tok$
  jsr add_curtok_to_astr
; cur_tok$ = ""
  +ASSIGN_U8V_EQ_IMM cur_tok, $00

  +plw is_ptr
  +plw s_ptr
  pla
  sta cur_line_len
  pla
  sta cur_char
  rts

;--------------
dbl_quote_check:
;--------------
; inputs:
;   - cur_char, a_str, cur_tok, quote_flag
; outputs:
;   - C=1 if this is a dbl-quote, or a char within a pair of quotes
;         (such chars are added to a_str immediately)
;   - C=0 if this is a char that accumulates in a token first
;         (such chars are later added to cur_tok, but not within this routine)

;   - if cur_char is a starting quote, then:
;       - a_str += (subbed)cur_tok  (token accumulating prior to the quote)
;       - a_str += cur_char (the ")
;       - cur_tok = ""
;       - C = 1

;   - else if ending quote, then:
;       - a_str += cur_char
;       - C = 1

;   - if we are within quotes, then:
;       - a_str += cur_char  (text within quotes does not need to be tokenised)
;       - C = 1

;     if cur_ch$ = dbl_quote$ then begin
      lda cur_char
      cmp #'"'
      bne @skip_dbl_quote_check
;       quote_flag = abs(quote_flag - 1)
        +toggle_flag quote_flag

        ; if this a starting quote?
;       if quote_flag = 1 then begin
        beq @skip_to_else
          jsr add_subbed_curtok_to_astr

          jsr add_curchar_to_astr
          sec  ; trigger for's continue
          rts
;       bend : else begin
@skip_to_else:
          ; else this is an ending quote
;         a$ = a$ + cur_ch$
          jsr add_curchar_to_astr
  ;       goto rval_for_continue
          sec  ; trigger for's continue
          rts
;       bend
;     bend
@skip_dbl_quote_check:

      ; this logic is to handle the chars within two double-quotes
;     if quote_flag = 1 then begin
      lda quote_flag
      beq +
;       a$ = a$ + cur_ch$
        jsr add_curchar_to_astr
;       goto rval_for_continue
        sec
        rts
;     bend
+:
    clc
    rts


;------------------
add_curchar_to_astr:
;------------------
  ldx a_str   ; string length
  inx

  lda cur_char
  cmp #$00    ; if cur_char=$00, then ignore it (consider it "don't add a char")
  bne +
  rts

+:
  sta a_str,x
  stx a_str   ; store new length

  ; add a null-term, just as a nicety
  inx
  lda #$00
  sta a_str,x

  rts

;--------------------
add_curchar_to_curtok:
;--------------------
  lda cur_char
  cmp #$00    ; if cur_char=$00, then ignore it (consider it "don't add a char")
  bne +
  rts
+:

  inc cur_tok   ; string length
  ldx cur_tok

  sta cur_tok,x
  stx cur_tok   ; store new length

  ; add a null-term, just as a nicety
  inx
  lda #$00
  sta cur_tok,x

  rts


;-----------------
add_curtok_to_astr:
;-----------------
  ldx a_str     ; X = current length of a_str
  ldy #$00

@loop_next_char:
  cpy cur_tok   ; compare to length of cur_tok
                ; assure we haven't gone over end of cur_tok string
  beq @bail_out

  iny
  lda cur_tok,y

  inx
  sta a_str,x
  bra @loop_next_char

@bail_out:
  stx a_str   ; store new length of a_str

  ; add a null-term, just as a nicety
  inx
  lda #$00
  sta a_str,x

  rts


;----------------------
check_token_for_subbing:
;----------------------
; input: cur_tok
; output:
;  - potentially modified cur_tok

;   if cur_tok$ = "" or cur_tok$ = "{x5F}" then return
    lda cur_tok  ; length of str is 0?
    bne +
    rts       ; if so, then bail out early
+:
    cmp #$01  ; is length is one?
    bne +
    lda cur_tok+1
    cmp #$5f  ; left-arrow char?
    bne +
    rts       ; if so, then bail out early
+:

    jsr decimal_number_check
    bcc +
    rts
+:
    
    jsr check_mark_expected_label

;   if cur_tok$ = "goto" then next_line_flag = 1
    phw s_ptr
    +ASSIGN_U16V_EQ_ADDR s_ptr, cur_tok+1
    +CMP_S_PTR_TO_IMM "goto"
    bcs +
      +ASSIGN_U8V_EQ_IMM next_line_flag, $01
+:
    +plw s_ptr

    jsr check_expect_label_next
    jsr check_hex_and_binary_value

    jsr check_ignore_existing_vocab
    bcc +
    rts
+:
    jsr check_swap_vars_with_short_names
;   if did_replace_flag = 1 then return
    +CMP_U8V_TO_IMM did_replace_flag, $01
    bne +
    rts
+:
    jsr check_defines_table
    bcc +
    rts

+:
    jsr check_struct_names
    bcc +
    rts
+:

;   ' PI token check
;   ' - - - - - - -
;   if asc(cur_tok$) = 222 then return

    jsr check_dumb_commands

unresolved_cur_tok:
;   parser_error$ = "?unresolved identifier: '" + cur_tok$ + "' in line " + str$(cur_src_lineno)
  +SET_STRING parser_error, "?unresolved identifier: '"
  +APPEND_STR_TO_S_PTR cur_tok
  +APPEND_INLINE_TO_S_PTR "' in line "
  +APPEND_UINT_TO_S_PTR cur_src_lineno

;   sleep 1
!ifdef RUN_TESTS {
    rts   ; bail back to test system
} else {
;   goto return_to_editor_with_error
    jmp return_to_editor_with_error
}


;---------------
prepare_tok_name:
;---------------
  +SET_STRING tok_name, " "
  +APPEND_STR_TO_S_PTR cur_tok+1
  +APPEND_INLINE_TO_S_PTR " "
  rts


;------------------
check_dumb_commands:
;------------------
;   ' check for dumb commands
;   ' - - - - - - - - - - - -
;   ' NOTE: I think lc$ below should be replaced with tok_name$
;   ' ----
;   if instr(dumb_cmds$, lc$) <> 0 and {x5F}
;     (cur_tok$ = "r" or cur_tok$ = "p" 
;      or cur_tok$ = "u8" or cur_tok$ = "w") then begin
; 
;     return
;   bend
    clc
    rts


;-----------------
check_struct_names:
;-----------------
;   ' check for struct names
;   ' ----------------------
;   found_struct_idx = -1
; 
;   for id = 0 to struct_cnt - 1  ' check struct names
;     if cur_tok$ = struct_name$(id) then begin
;       gosub check_for_creation_of_struct_object
;       found_struct_idx = id
;       id = struct_cnt - 1  ' create new struct object
;     bend
;   next id
; 
;   if found_struct_idx <> -1 then begin
;     return
;   bend
    clc
    rts


;------------------
check_defines_table:
;------------------
; input: cur_tok  (e.g., "TESTDEFVAL")
; output:
;   - cur_tok (changed to defined value)
;   - C=1 (if subbed)
;   - C=0 (if not subbed)
;
;   ' check defines table too
;   ' - - - - - - - - - - - -
    
    ldx #TYP_DEF
    stx ty
    +SET_IS_PTR_TO_VARTABLE_AT_TY_IDX
    lda element_cnt,x
    sta elcnt
    +ASSIGN_U8V_EQ_IMM elidx, $00
;   for id = 0 to element_cnt(TYP_DEF)
@loop_next_element:
    ldy elidx
    cpy elcnt
    beq @bail_out
;     if cur_tok$ = var_table$(TYP_DEF, id) then begin
      +ASSIGN_U16V_EQ_DEREF_U16V s_ptr, is_ptr
      +CMP_S_PTR_TO_STR cur_tok+1
      bcs +
;       cur_tok$ = define_val$(id)
        lda elidx
        +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_A define_val
        +ASSIGN_U16V_EQ_DEREF_U16V is_ptr, tmp_ptr
        +COPY_STR_FROM_PSTR cur_tok+1, is_ptr
        lda cur_line_len
        sta cur_tok  ; store current length
;       return
        sec
        rts
;     bend
+:
;   next id
    inc elidx
    inw is_ptr
    inw is_ptr
    bra @loop_next_element

@bail_out:
    clc
    rts

elcnt:
!byte $00
elidx:
!byte 00

;-------------------------------
check_swap_vars_with_short_names:
;-------------------------------
;  input: cur_tok (e.g., "fishy$")
;  output:
;    - ty
;    - cur_tok (if in var_table, then replace with auto-generated name "a$)
;    - did_replace_flag (=1 if subbed, =0 if not)

;   ' check to swap out variables with short names
;   ' - - - - - - - - - - - - - - - - - - - - - -
;   did_replace_flag = 0  ' did replace flag

    jsr check_type

  +SET_IS_PTR_TO_VARTABLE_AT_TY_IDX

    +ASSIGN_U8V_EQ_ELCNT_IDX_OF_TY elcnt
    +ASSIGN_U8V_EQ_IMM elidx, $00
;   for id = 0 to element_cnt(ty)
@loop_next_element:
    ldy elidx
    cpy elcnt
    beq @bail_out
;     if cur_tok$ = var_table$(ty, id) then begin
      +ASSIGN_U16V_EQ_DEREF_U16V s_ptr, is_ptr
      +CMP_S_PTR_TO_STR cur_tok+1
        bcs @skip
;       gosub generate_varname
        jsr generate_varname
;       cur_tok$ = gen_varname$ + type_ident$(ty)
        +COPY_STR cur_tok+1, gen_varname
        lda cur_line_len
        sta cur_tok   ; store current length
        ldx ty
        lda type_ident,x
        beq + ; skip for real (has no identifier)
        ldx cur_line_len
        sta cur_tok+1,x
        inc cur_line_len
        inc cur_tok  ; store current length
        lda #$00
        sta cur_tok+2,x
+:
;       did_replace_flag = 1
        +ASSIGN_U8V_EQ_IMM did_replace_flag, $01
;       id = element_cnt(ty)
        bra @bail_out
;     bend
@skip:
;   next id
    inc elidx
    inw is_ptr
    inw is_ptr
    bra @loop_next_element

@bail_out:
    rts


;---------
check_type:
;---------
;   t$ = right$(cur_tok$, 1)
    ldx cur_tok
    lda cur_tok,x
    sta cur_char

;   ty = TYP_REAL
    +ASSIGN_U8V_EQ_IMM ty, TYP_REAL
    
    lda cur_char
;   if t$ = "%" then ty = TYP_INT
    cmp #'%'
    bne +
      +ASSIGN_U8V_EQ_IMM ty, TYP_INT
      bra @skip
+:
;   if t$ = "$" then ty = TYP_STR
    cmp #'$'
    bne +
      +ASSIGN_U8V_EQ_IMM ty, TYP_STR
      bra @skip
+:
;   if t$ = "&" then ty = TYP_BYTE
    cmp #'&'
    bne @skip
      +ASSIGN_U8V_EQ_IMM ty, TYP_BYTE
@skip:
    rts


;--------------------------
check_ignore_existing_vocab:
;--------------------------
;  input: cur_tok
;  output:
;    - C=1 if token exists in vocab
;    - C=0 if token doesn't exist in vocab
;    - lc$ = cur_tok if found in vocab

;   ' ignore existing vocabulary of functions/commands
;   ' - - - - - - - - - - - - - - - - - - - - - - - - 
;   tok_name$ = " " + cur_tok$ + " "
    jsr prepare_tok_name

    lda #$00
    sta ty
;   for t = 0 to 6
@loop_next_ty:
    +CMP_U8V_TO_IMM ty, TKN_STR_CNT
    beq @bail_out

;     if instr(tokens$(t), tok_name$) <> 0 then begin
      +ASSIGN_U16V_EQ_ADDR s_ptr, tokens
      lda ty
      clc
      adc s_ptr+1
      sta s_ptr+1   ; increment s_ptr to tokens$(ty)

      +CMP_S_PTR_TO_STR tok_name
      bcs +
;       lc$ = cur_tok$
        +COPY_STR lc, cur_tok
;       gosub check_if_command_triggers_shitty_syntax
;       return
        sec
        rts
;     bend
+:
;   next
    inc ty
    bra @loop_next_ty

@bail_out:
    clc
    rts


;-------------------------
check_hex_and_binary_value:
;-------------------------
; input: cur_tok
; output:
;   - none (potentially trigger trap to illegal_hex_handler: if invalid number)

;   ' check hex value
;   ' - - - - - - - -
;   if left$(cur_tok$, 1) = "$" then begin
    +CMP_U8V_TO_IMM cur_tok+1, '$'
    bne +
      +ASSIGN_U16V_EQ_ADDR tmp_ptr, cur_tok
;     hx$ = mid$(cur_tok$, 2)
      ldx cur_tok
      dex
      stx cur_line_len
      inc tmp_ptr ; skip length-byte
      inc tmp_ptr ; skip first char
;     gosub check_hex
      jsr check_hex
;     return
      rts
;    bend
+:
; 
; 
;   ' check binary value
;   ' - - - - - - - - -
;   if left$(cur_tok$, 1) = "%" then begin
    +CMP_U8V_TO_IMM cur_tok+1, '%'
    bne +
;     bi$ = mid$(cur_tok$, 2)
      +ASSIGN_U16V_EQ_ADDR tmp_ptr, cur_tok
      ldx cur_tok
      dex
      stx cur_line_len
      inc tmp_ptr ; skip length-byte
      inc tmp_ptr ; skip first char
;     gosub check_binary
      jsr check_binary
;     return
      rts
;   bend
+:
    rts

;----------------------
check_expect_label_next:
;----------------------
;  input: cur_tok
;  output: expecting_label

;   ' are we expecting a label next?
;   ' - - - - - - - - - - - - - - -
    bra +
@exp1:
!pet $04, "goto", $00
@exp2:
!pet $05, "gosub", $00
@exp3:
!pet $04, "trap", $00
+:

;   if cur_tok$ = "goto" or cur_tok$ = "gosub" or cur_tok$ = "trap" then begin
    +STR_MATCH_TO_SPTR @exp1
    bcc +
    +STR_MATCH_TO_SPTR @exp2
    bcc +
    +STR_MATCH_TO_SPTR @exp3
    bcc +
    bra ++
+:
;     expecting_label = 1
      +ASSIGN_U8V_EQ_IMM expecting_label, $01
;   bend
++:
    rts


;------------------------
check_mark_expected_label:
;------------------------
; input:
;  - expecting_label
;  - dont_mark_label
;  - cur_tok
;
; output:
;  - expecting_label
;  - cur_tok (may have '@pi' markers added to front and end of it)
;      (the mk$ markers are used later in pass2 to sub out labels for line-no's)

;   ' check if we should mark this expected label
;   ' - - - - - - - - - - - - - - - - - - - - - -
;   if expecting_label and dont_mark_label = 0 then begin
    +CMP_U8V_TO_IMM expecting_label, $00
    beq +
    +CMP_U8V_TO_IMM dont_mark_label, $01
    beq +
;     gosub mark_cur_tok$_label
      jsr mark_cur_tok_label
;     expecting_label = 0
      +ASSIGN_U8V_EQ_IMM expecting_label, $00
;     return  ' replace label
      rts
;   bend
+:
    rts

u16result:
!word $0000

;---------------------
get_u16_val_of_cur_tok:
;---------------------
; input: cur_tok
; output:
;   - u16result
;   - C = 0 (token is a valid number)
;   - C = 1 (token appears not to be a number)

  ; initialise result to zero
  +ASSIGN_U16V_EQ_IMM u16result, $00

  ldy cur_tok   ; holds the length
  tya
  tax
  ; iterate over each column
@loop_next_char:
  cpy #$00
  beq @skip
    lda cur_tok,x
    sta cur_char
    ; check if char in '0' to '9' range
    +check_09_range

    phx

    dey
    tya
    jsr get_column_value
    iny

    phy
    ; multiply char-value - '0' by column_values
    lda cur_char
    sec
    sbc #'0'
    +multiply_a_by_column_value

    ; add to total
    +add_multout_to_u16result

    ply
    dey
    plx
    dex
  bne @loop_next_char
@skip

  clc
  rts

;-------------------
decimal_number_check:
;-------------------
;  input: cur_tok
;  output:
;    - expecting_label = 0 (if we assured this token is just a number)
;    - cur_tok = "." (if this token was just "0", optimize it to ".")

;   ' decimal number check
;   ' - - - - - - - - - -
;   if val(cur_tok$) <> 0 then begin
    jsr get_u16_val_of_cur_tok
    bcs +
    lda u16result
    ora u16result+1
    beq +
;     expecting_label = 0
      +ASSIGN_U8V_EQ_IMM expecting_label, $00
;     return  ' never change numbers
      sec
      rts
;   bend
+:
; 
;   if cur_tok$ = "0" then begin
    +CMP_U8V_TO_IMM cur_tok, $01
    bne @skip
    +CMP_U8V_TO_IMM cur_tok+1, '0'
    bne @skip
;     cur_tok$="."
    +ASSIGN_U8V_EQ_IMM cur_tok+1, '.'
;     return  ' stupid ms basic optimization
      sec
      rts
;   bend
@skip;
    clc
    rts


; '---------------------------------------
; .check_if_command_triggers_shitty_syntax
; '---------------------------------------
;   ' check if command triggers shitty syntax mode
;   ' todo: compare ubik's logic here versus my own 4079 lc$="dopen" logic
;   return


;-----------------
mark_cur_tok_label:  ; mark_cur_tok$_label
;-----------------
;  input: cur_tok
;  output: cur_tok = mk$ + cur_tok$ + mk$

;   cur_tok$ = mk$ + cur_tok$ + mk$
    ldy cur_tok   ; current length
    bne +
    rts   ; if length is non-zero, bail out
+:
    dey
    tya
    tax
    inx
    inx

@loop_next_char:
    lda cur_tok+1,y
    sta cur_tok+1,x
    dex
    dey
    bpl @loop_next_char

    ; add to start: mk$ = "@{x7E}"  ' @ and pi
    lda #'@'
    sta cur_tok+1
    lda #$7e  ; pi
    sta cur_tok+2

    ; add to end: mk$ = "@{x7E}"  ' @ and pi
    inc cur_tok
    inc cur_tok
    ldy cur_tok

    lda #'@'
    sta cur_tok+1,y
    lda #$7e
    sta cur_tok+2,y
    lda #$00
    sta cur_tok+3,y

    inc cur_tok
    inc cur_tok
;   return
    rts
; 
; 
; '-------------------------------
; .swap_out_label_in_cur_tok$_for_lineno
; '-------------------------------
;   did_replace_flag = 0
;   for id = 0 to label_cnt - 1
;     if cur_tok$ = label_name$(id) then begin
;       cur_tok$ = str$(label_lineno(id))
;       id = label_cnt
;       did_replace_flag = 1
;     bend
;   next id
;   if did_replace_flag then return
; 
;   parser_error$ = "?unresolved label: '" + cur_tok$ + "' in line" + str$(map_dest_to_src_lineno%(cur_dest_lineno - 1))
;   sleep 1
;   goto return_to_editor_with_error
; 
;   return
; 
; 
;-----------
check_binary:
;-----------
;   br = 0  ' result
; 
    ldy #$00
;   for b = 0 to len(bi$) - 1
@loop_next_char:
    cpy cur_line_len
    beq @bail_out

;     bc$ = mid$(bi$, len(bi$) - b, 1)
      lda (tmp_ptr),y

;     if bc$ <> "1" and bc$ <> "0" then begin
      cmp #'1'
      beq @skip
      cmp #'0'
      beq @skip
;       bank 4
;       poke $ff08, 132
        +POKEB4 $ff08, 132
;       goto unresolved_cur_tok$
        jmp unresolved_cur_tok
;     bend
@skip:

;     if bc$="1" then br=br+bin_conv(b)
;   next b
    iny
    bra @loop_next_char

@bail_out:
;   return
    rts
; 
; 
;--------
check_hex:
;--------
;   trap illegal_hex_handler
;   vl = dec(hx$)
;   trap
    ldy #$00
@loop_next_char:
    cpy cur_line_len
    beq @bail_out

    lda (tmp_ptr),y
    sta cur_char

    +CMP_U8V_IN_IMM_RANGE cur_char, '0', '9'
    bcc @valid
    +CMP_U8V_IN_IMM_RANGE cur_char, 'a', 'f'
    bcc @valid
    +CMP_U8V_IN_IMM_RANGE cur_char, 'A', 'F'
    bcc @valid
 
@invalid:
    jmp illegal_hex_handler

@valid:
    iny
    bra @loop_next_char

@bail_out:
;   return
    rts


;------------------
illegal_hex_handler:
;------------------
;   trap
;   bank 4
;   poke $ff08, 131  ' set illegal hex

    +POKEB4 $ff08, 131

;   goto unresolved_cur_tok$  ' jump into error handler
    jmp unresolved_cur_tok


;---------------
generate_varname:
;---------------
; input: elidx (e.g. = 1)
; output:
;  - gen_varname  (e.g., = 'B' for 1, or 'AA' for 26)

;   ' generate varname from index
; 
;   if id < 26 then begin
    lda elidx
    cmp #26
    bcs +
;     gen_varname$ = chr$(65 + id)
      lda #65   ; letter 'A'
      clc
      adc elidx
      sta gen_varname
      lda #$00
      sta gen_varname+1
;     return
      rts
;   bend
+:

    +SET_MULTINA_TO_U8V elidx
    +SET_MULTINB_TO_U8 26
    +WAIT_DIV_FINISH

;   n1 = int(id / 26) - 1
    ldx DIVOUT+4
    dex
    stx n1
;   n2 = mod(id, 26)
    lda DIVOUT+4
    sta MULTINA ; find the modulo value
    +WAIT_MUL_FINISH

    sec
    lda elidx
    sbc MULTOUT
    sta n2

;   gen_varname$ = chr$(65 + n1) + chr$(65 + n2)
      lda #65   ; letter 'A'
      clc
      adc n1
      sta gen_varname
      lda #65
      clc
      adc n2
      sta gen_varname+1
      lda #$00
      sta gen_varname+2

;   ' avoid any basic terms as var names
;   if gen_varname$="do" then gen_varname$="d1"
    +CMP_GENVNAME_TO 'd', 'o'
    bcs +
      +ASSIGN_U8V_EQ_IMM gen_varname+1, '1'
      rts
+:
;   if gen_varname$="go" then gen_varname$="g1"
    +CMP_GENVNAME_TO 'g', 'o'
    bcs +
      +ASSIGN_U8V_EQ_IMM gen_varname+1, '1'
      rts
+:
;   if gen_varname$="to" then gen_varname$="t1"
    +CMP_GENVNAME_TO 't', 'o'
    bcs +
      +ASSIGN_U8V_EQ_IMM gen_varname+1, '1'
      rts
+:
;   if gen_varname$="ds" then gen_varname$="d2"
    +CMP_GENVNAME_TO 'd', 's'
    bcs +
      +ASSIGN_U8V_EQ_IMM gen_varname+1, '2'
      rts
+:
;   if gen_varname$="dt" then gen_varname$="d3"
    +CMP_GENVNAME_TO 'd', 't'
    bcs +
      +ASSIGN_U8V_EQ_IMM gen_varname+1, '3'
      rts
+:
;   if gen_varname$="el" then gen_varname$="e1"
    +CMP_GENVNAME_TO 'e', 'l'
    bcs +
      +ASSIGN_U8V_EQ_IMM gen_varname+1, '1'
      rts
+:
;   if gen_varname$="er" then gen_varname$="e2"
    +CMP_GENVNAME_TO 'e', 'r'
    bcs +
      +ASSIGN_U8V_EQ_IMM gen_varname+1, '2'
      rts
+:
;   if gen_varname$="fn" then gen_varname$="f1"
    +CMP_GENVNAME_TO 'f', 'n'
    bcs +
      +ASSIGN_U8V_EQ_IMM gen_varname+1, '1'
      rts
+:
;   if gen_varname$="if" then gen_varname$="i1"
    +CMP_GENVNAME_TO 'i', 'f'
    bcs +
      +ASSIGN_U8V_EQ_IMM gen_varname+1, '1'
      rts
+:
;   return
    rts


;-----------
get_filename:
;-----------
;   bank 4
;   ba = $ff00
  lda #$00
  sta FOURPTR
  lda #$ff
  sta FOURPTR+1

;   if peek(ba+0) = asc("s") and peek(ba+1) = asc("k") then begin
  ldz #$00
  lda [FOURPTR],z
  cmp #'s'
  bne @skip_sig_check
  inz
  lda [FOURPTR],z
  cmp #'k'
  bne @skip_sig_check

;     verbose = peek($ff07) and 8
    ldz #$07
    lda [FOURPTR],z
    pha
    and #$10
    asr
    asr
    asr
    asr
    sta verbose
    pla
    and #$01
    tax ; x = peek($ff07) and 1

;     f$ = ""
    lda #$00
    sta f_str

;     a = ba + $10
    lda #$10
    sta FOURPTR
    ldz #$00
    ldy #$00

@loop_fname_parse:
;     do while peek(a) <> 0
    lda [FOURPTR],z
    beq @skip_fname_parse
;       f$ = f$ + chr$(peek(a))
    sta f_str,y
;       a = a + 1
    inz
    iny
;     loop
    bne @loop_fname_parse

@skip_fname_parse
    lda #$00
    sta f_str,y

;     if peek($ff07) and 1 then return
    cpx #$01
    bne @skip_print_fname
;     print "filename? " + f$
    jsr print_inline_text
!pet "filename? ", $00
    lda #<f_str
    sta ret_ptr_lo
    lda #>f_str
    sta ret_ptr_hi
    jsr print_text
    lda #$0d
    jsr CHROUT

;     print "{x91}"; (up arrow)
    ; lda #$91
    ; jsr CHROUT

;   bend
@skip_print_fname:

@skip_sig_check:
rts

; TODO: consider doing this manual filename input logic another time
; 
;   input "filename"; a$
;   if a$ = "" then begin
;     print "no filename set"
;     end
;   bend
; 
;   poke ba, asc("s")
;   poke ba + 1, asc("k")
;   for r = 1 to 16
;     poke ba + 8 + r - 1, asc(mid$(a$, r, 1))
;   next r
;   f$ = a$
;   return
; 
; 
; '-------------
; .save_filename
; '-------------
;   ad = $ff30
;   bank 4
; 
;   for r = 0 to 16
;     poke ad + r, 0
;   next r
; 
;   if of$ <> "" then begin
;     tf$ = of$
;   bend : else begin
;     tf$="eleven.out"
;   bend
; 
;   for r = 1 to len(tf$)
;     poke ad + r - 1, asc(mid$(tf$, r, 1))
;   next r
; 
;   return
; 
; 
;-----------
chain_editor:
;-----------
;   get a$
;   if a$ <> "" then begin
;     input zz
;     if zz=1 then stop
;   bend
; 
;   print "{x13}{x13}{x93}{x11}{x11}edma 0,$d400,$8000000,$2001:new restore{x11}{x11}"
    jsr print_inline_text
!pet $13, $13, $93, $11, $11, "edma 0,$d400,$8000000,$2001:new restore", $11, $11, $0d, $00

;   print "run{x13}";  ' load '11.edit' from cache
    jsr print_inline_text
!pet "run", $13, $00

;   bank 128
;   poke 208, 2      ' no# of chars in keyboard buffer
    lda #$02
    sta $d0

;   poke 688, 13, 13 ' return chars
    lda #$0d
    sta $2b0
    sta $2b1

    jmp bail_out_early
;   end

sd_idx:
!byte $00

;---------------
is_s_ptr_defined:  ; is_s$_defined
;---------------
; NOTE: Is used exclusively during parsing of "#ifdef FISHY" directives
;
; input:
;   - s_ptr  (the token to check if is defined in var_table(TYP_DEF,x) or not)
;            (e.g. "FISHY")
; output:
;   - inside_ifdef
;       =1 if define does not exist, so that compiler ignores lines within
;       =0 if define exists, so that compiler parses lines within

;   inside_ifdef = 1
  +ASSIGN_U8V_EQ_IMM inside_ifdef, $01

  +SET_PTR_TO_ARRAY_AT_OFFSET a_ptr, var_table, 200*2*TYP_DEF

  ldy #$00
  sta sd_idx
@loop:
;   for k = 0 to element_cnt(TYP_DEF)
  ldy sd_idx
  cpy element_cnt + TYP_DEF
  beq @bail_out
;     if var_table$(TYP_DEF, k) = s$ then begin
    ; tmp_ptr is pointer to indexed address within var_table$()

      +ASSIGN_U16V_EQ_DEREF_U16V tmp_ptr, a_ptr
      jsr cmp_tmp_ptr_to_s_str
      bcs + ; skip if string not foud
;       inside_ifdef = 0
        +ASSIGN_U8V_EQ_IMM inside_ifdef, $00
        rts
;     bend
+:
;   next k
  inc sd_idx
  inw a_ptr
  inw a_ptr
  bra @loop

@bail_out:
;   return
  rts
; 
; 
;---------------------
read_in_struct_details:
;---------------------
; input:
;   - s_ptr (the cur_src_line with the struct definition)
;       (e.g., "#struct ENVTYPE name$, attack, decay, sustain")
;
; output:
;   - struct_name(struct_cnt) = struct name
;       (e.g. "ENVTYPE")
;
;   - struct_vars(struct_cnt) = struct vars
;           (e.g. "name$, attack, decay, sustain")
;
;   - struct_cnt++

;   cur_src_line$ = mid$(cur_src_line$, 9)
    +ADD_TO_POINTER_U8 s_ptr, 8
    +SUB_U8V_WITH_IMM cur_line_len, $08

;   gosub read_next_token  ' get next token in s$
    jsr read_next_token
; 
;   if s$ = "" then begin
    ldy #$00
    lda (a_ptr),y
    bne +
;     print "error: no struct name found"
      +SET_PARSER_ERROR_ON_LINE "?no struct name found on line "
;     sleep 1
;     return
      jmp return_to_editor_with_error
;   bend
+:

    +ASSIGN_U16V_EQ_U16V sr_ptr, s_ptr  ; back up current s_ptr into sr_ptr

;   struct_name$(struct_cnt) = s$
    lda struct_cnt
    +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_A struct_name
    +HEAP_COPY_PSTR_EQ_PSTR tmp_ptr, a_ptr

;   struct_vars$(struct_cnt) = cur_src_line$
    +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_A struct_vars
    +HEAP_COPY_PSTR_EQ_PSTR tmp_ptr, sr_ptr

;   struct_cnt = struct_cnt + 1
    inc struct_cnt

;   return
    rts


;--------------
read_next_token
;--------------
; input:
;   - s_ptr (pointing to cur_src_line, or a portion of it due to a prior call)
;       (e.g., "ENVTYPE name$, attack, decay, sustain")
; output:
;   - a_ptr (the currently read token)
;           (e.g. "ENVTYPE")
;   - s_ptr (pointing to remainder of cur_src_line minus current token)
;           (e.g. "name$, attack, decay, sustain")

;   ' read next token from cur_src_line$ into s$
;   s$ = cur_src_line$
;   gosub strip_tr$_from_beginning
    jsr strip_tr_from_beginning
;   gosub strip_tr$_from_end
    jsr strip_tr_from_end
;   cur_src_line$ = s$
;   sf$ = " "
    +ASSIGN_U8V_EQ_IMM sf_str, ' '
    +ASSIGN_U8V_EQ_IMM sf_str+1, $00
;   sf = 0
    +ASSIGN_U8V_EQ_IMM sf, $00

;   if left$(s$, 1) = dbl_quote$ then begin
    ldy #$0
    lda (s_ptr),y
    cmp #'"'
    bne +
;     sf$ = dbl_quote$ + ", "
      phw s_ptr
      +SET_STRING sf_str, "\", "
      +plw s_ptr
;     sf = 2
      +ASSIGN_U8V_EQ_IMM sf, $02
;   bend
+:

;   a = instr(cur_src_line$, sf$)
    +INSTR_S_PTR_TO_STR sf_str
;   if a <> 0 then begin
    bcs @skip_to_else_not_found
;     s$ = mid$(cur_src_line$, 1, instr(cur_src_line$, sf$) + sf - 1)
      clc
      adc sf
      tay
      lda #$00
      sta (s_ptr),y ; add null term at end of token
      +ASSIGN_U16V_EQ_U16V a_ptr, s_ptr

;     cur_src_line$ = mid$(cur_src_line$, instr(cur_src_line$, sf$) + sf + 1)
      iny
      tya
      clc
      adc s_ptr
      sta s_ptr
      lda #$00
      adc s_ptr+1
      sta s_ptr+1
;   bend
    bra @skip_past_if_else
; 
;   if a = 0 then begin
@skip_to_else_not_found:
;     s$ = cur_src_line$
;     cur_src_line$ = ""
;   bend
@skip_past_if_else:
; 
;   return
    rts
; 
; 
;-------------
read_next_line:
;-------------
;   ' read next line into cur_src_line$
;   bank 0
;   cur_line_len = peek(cur_attic_addr)
  ldz #$00
  ldy #$00
  lda [SRCPTR],z
  sta cur_line_len

  sty cur_src_line  ; put null-terminator at start of line (to empty it)

;   cur_src_line$ = left$(blank_line$, cur_line_len)
;   src_line_ptr = pointer(cur_src_line$)
;   src_linebuff_ptr = $10000 + wpeek(src_line_ptr + 1)
;   cur_attic_addr = cur_attic_addr + 1
  inw SRCPTR
; 
;   if cur_line_len <> 0 then begin
  cmp #$00
  beq @skip_dma_copy
  
;     edma 0, cur_line_len, cur_attic_addr, src_linebuff_ptr
    lda #$00  ; cmd = copy
    sta dmatable_cmd
    lda cur_line_len
    sta dmatable_len
    lda #$00
    sta dmatable_len+1

    lda SRCPTR
    sta dmatable_src_addr
    lda SRCPTR+1
    sta dmatable_src_addr+1
    lda SRCPTR+2
    sta dmatable_src_bank

    lda #<cur_src_line
    sta dmatable_dest_addr
    lda #>cur_src_line
    sta dmatable_dest_addr+1
    lda #$00
    sta dmatable_dest_bank

    // poke $d702, 0 ' dma list in bank 0
    lda #$00
    sta $d702

    // poke $d701, $00 ' dma list msb
    lda #>dmatable
    sta $d701

    // poke $d705, $00 ' dma list lsb
    lda #<dmatable
    sta $d705

;     cur_attic_addr = cur_attic_addr + cur_line_len
    clc
    lda cur_line_len
    adc SRCPTR
    sta SRCPTR
    lda #$00
    adc SRCPTR+1
    sta SRCPTR+1

    ldy cur_line_len
    lda #$00
    sta cur_src_line,y  ; add null terminator

;   bend
@skip_dma_copy:

; 
;   cur_src_lineno = cur_src_lineno + 1
  inw cur_src_lineno

; 
;   tr$ = whitespace$
  lda #<whitespace
  sta tr_ptr
  lda #>whitespace
  sta tr_ptr+1

;   s$ = cur_src_line$
  lda #<cur_src_line
  sta s_ptr
  lda #>cur_src_line
  sta s_ptr+1

;   gosub strip_tr$_from_beginning
  jsr strip_tr_from_beginning

;   cur_src_line$ = s$
; 
;   quote_flag = 0    ' quotes on
  lda #$00
  sta quote_flag

;   cut_tail_idx = 0  ' cut chars from tail
  sta cut_tail_idx

;   bank 1
;   return
  rts


;------
instr_chr:
;------
  ; finds A in str at ptr YX
  ; returns:
  ; - C=0 if found, C=1 if not found (and A = $ff)
  ; - A=index of found char
  stx is_ptr
  sty is_ptr+1
instr_chr_quick:
  sta cur_char

  ldy #$00
@loop_next_char:
  lda (is_ptr),y
  beq @end_of_string

  cmp cur_char
  bne @not_found

  tya ; found it, let a = index of found pos
  clc ; indicates 'found'
  rts

@not_found:
  iny
  jmp @loop_next_char

@end_of_string:
  sec ; indicates 'not found'
  lda #$ff
  rts

cur_scan_idx:
!byte $00

;----
streq:
;----
; C=0 if equal, C=1 if unequal
  jsr instr
  bcc +  ; bail out if not instr
  rts
+:
  lda (haystack_ptr),y
  beq @bail_out_succeed   ; assure haystack has null-term
  sec
  rts

@bail_out_succeed:
  clc
  rts

;----
instr:
;----
  ; looks for needle str (at ptr YX) inside haystack str (at ptr ZA)
  ; returns:
  ; - C=0 if found, C=1 if not found
  ; - A=index of found str
  stx needle_ptr
  sty needle_ptr+1
  sta haystack_ptr
  stz haystack_ptr+1

  ldy #$00  ; pointer to haystack
  ldz #$00  ; pointer to needle

@loop_next_needle_char:
  cpz #$00
  bne +
  sty cur_scan_idx  ; store what index in haystack we are currently
                    ; checking for needle in

+:
  lda (needle_ptr),z
  beq @bail_out_succeed
  sta cur_char

@loop_next_haystack_char:
  lda (haystack_ptr),y
  beq @bail_out_fail
  cmp cur_char
  bne @not_found

  ; found-logic (go to next needle_char)
  inz
  iny
  bra @loop_next_needle_char

@not_found:
  ldz #$00
  ldy cur_scan_idx
  iny
  bra @loop_next_needle_char

@bail_out_fail:
  sec
  rts

@bail_out_succeed:
  lda cur_scan_idx
  clc
  rts


;------------------------
single_quote_comment_trim:
;------------------------

;       cut_tail_idx = 0
      lda #$00
      sta cut_tail_idx
      ldy #$00
@loop_next_char:
;       for r = 0 to cur_line_len_minus_one
;         chr = peek(cur_linebuff_addr + r)
        lda (s_ptr),y
        beq @bail_out   ; bail out if found null terminator
;         if chr = dbl_quote_char then begin
        cmp #'"'
        bne @char_not_a_dbl_quote
;           quote_flag = abs(quote_flag - 1)
          lda quote_flag
          eor #$01
          sta quote_flag
          jmp @skip_past_else

;         bend : else begin
@char_not_a_dbl_quote:
;           if chr = sngl_quote_char and quote_flag=0 then begin
          cmp #"'"
          bne @skip_past_else
          lda quote_flag
          bne @skip_past_else
;             cut_tail_idx = r + 1
            lda #$00
            sta (s_ptr),y
            sty cur_line_len
;             r = 999
            jmp @bail_out
;           bend
;         bend
@skip_past_else:
;       next
      iny
      jmp @loop_next_char

;     bend
@bail_out
  rts


; 
;---------------------------
parse_preprocessor_directive:
;---------------------------
;   if instr(cur_src_line$, "ifdef") = 2 then begin
    +INSTR_S_PTR_TO_IMM "ifdef"
    bcs +
    cmp #$01
    bne +

;     s$ = mid$(cur_src_line$, 8)
      +ADD_TO_POINTER_U8 s_ptr, $07
      +SUB_U8V_WITH_IMM cur_line_len, $07

;     gosub is_s$_defined
      jsr is_s_ptr_defined

;     delete_line_flag = 1
      lda #$01
      sta delete_line_flag

;   bend
+:
; 
;   if instr(cur_src_line$, "endif") = 2 then begin
    +INSTR_S_PTR_TO_IMM "endif"
    bcs +
    cmp #$01
    bne +

;     inside_ifdef = 0
      lda #$00
      sta inside_ifdef

;     delete_line_flag = 1
      lda #$01
      sta delete_line_flag

;   bend
+:

; 
;   if instr(cur_src_line$, "define") = 2 then begin
    +INSTR_S_PTR_TO_IMM "define"
    bcs +
    cmp #$01
    bne +
;     define_flag = 1
      +ASSIGN_U8V_EQ_IMM define_flag, $01

;     gosub declare_s$_var
      jsr declare_s_ptr_var

;     define_flag = 0
      lda #$00
      sta define_flag
;   bend
+:

; 
;   if instr(cur_src_line$, "declare") = 2 then begin
    +INSTR_S_PTR_TO_IMM "declare"
    bcs +
    cmp #$01
    bne +
;     define_flag = 0
      +ASSIGN_U8V_EQ_IMM define_flag, $00
;     gosub declare_s$_var 
      jsr declare_s_ptr_var
;   bend
+:
; 
;   if instr(cur_src_line$,"output")=2 then begin
    +INSTR_S_PTR_TO_IMM "output"
    bcs +
    cmp #$01
    bne +
;     gosub set_output_file
      jsr set_output_file
;   bend
+:
; 
;   if instr(cur_src_line$,"struct")=2 then begin
    +INSTR_S_PTR_TO_IMM "struct"
    bcs +
;     gosub read_in_struct_details
      jsr read_in_struct_details
;     delete_line_flag = 1
      +ASSIGN_U8V_EQ_IMM delete_line_flag, $01
;   bend
+:

;   return
    rts


; '--------------------------------
check_for_creation_of_struct_object:
; '--------------------------------
; input:
;   - s_ptr  e.g.:
;             ENVTYPE envs(9) = [ {x5F}
;             [ "Piano",       0,  9,  0,  0,  2, 1536 ], {x5F}
; output:
;   - ...

;   orig$ = s$
    +ASSIGN_U16V_EQ_U16V orig_ptr, s_ptr
;   cur_src_line$ = s$
;   found_idx = -1  ' preserve original string
    +ASSIGN_U8V_EQ_IMM found_idx, $ff

    jsr find_struct_type    ; e.g. "ENVTYPE"
    bcc +
      rts
+:
    ; e.g. "envs(9)" => envs_name$(9), envs_attack(9)
    jsr find_struct_obj_name_and_dimension  
    bcc +
      rts
+:

;   s$ = next_line$
    +ASSIGN_U16V_EQ_ADDR s_ptr, next_line
;   gosub safe_add_to_current_or_next_line
    jsr safe_add_to_current_or_next_line
;   next_line$ = ""  ' safe add cur_dest_line$+s$ to current dest_line$(dest_lineno)
    +ASSIGN_U8V_EQ_IMM next_line, $00
; 
;   gosub read_next_token
    jsr read_next_token
; 
;   if s$ <> "=" then begin
    +CMP_S_PTR_TO_IMM "="
    beq +
;     cur_src_line$=""
      +ASSIGN_U8V_EQ_IMM cur_src_line, $00
      ; todo: should this case be a parser error and return to editor?
      ; "?missing '=' in line "
;     return
      rts
;   bend
+:
;   gosub read_next_token
    jsr read_next_token
; 
;   field_count=0
    +ASSIGN_U8V_EQ_IMM field_count, $00
;   sr=0
;   sm=0  ' read next token from cur_src_line$ into s$
; 
;   do while s$ <> ""
@loop_while:
      +CMP_S_PTR_TO_IMM $00
      beq @bail_out
      
      jsr check_continue_on_next_line
; 
      jsr check_sm0
; 
      jsr check_sm1_before_sm2
; 
      jsr check_sm2
; 
      jsr check_sm1_after_sm2
; 
@cfcoso_skip:
;     gosub read_next_token  ' read next token from cur_src_line$ into s$
      jsr read_next_token
;   loop
    bra @loop_while
@bail_out:
; 
;   s$ = ""
;   cur_src_line$ = ""
;   next_line$ = ""
;   zz$ = "z"
;   return
    rts


;---------------------------------
find_struct_obj_name_and_dimension:
;---------------------------------
;   gosub read_next_token
    jsr read_next_token     

;   struct_obj_name$ = s$  ' get struct object name
    +ASSIGN_U16V_EQ_U16V struct_obj_name, a_ptr

;   bkt_open_idx = instr(struct_obj_name$, "(")
    +INSTR_PSTR_TO_IMM_CHAR struct_obj_name, '('
    sta bkt_open_idx
; 
;   ' *** found it, so make dim's for each member var
;   s$ = struct_vars$(found_idx)
    +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_U8V struct_vars, found_idx
    +ASSIGN_U16V_EQ_U16V tmp2_ptr, s_ptr
    +ASSIGN_U16V_EQ_DEREF_U16V  s_ptr, tmp_ptr

;   gosub parse_arguments  ' parse args into args$(), arg_cnt
    jsr parse_arguments   ; e.g. in: s_ptr="name$, attack, decay, sustain"
                          ;     out: args[0]="name$", args[1]="attack", ...
; 
;   found_idx = 1
    +ASSIGN_U8V_EQ_IMM found_idx, $01
;   field_count = 0
    +ASSIGN_U8V_EQ_IMM field_count, $00

    jsr parse_args_of_struct
    clc
    rts

;--------------------------
check_continue_on_next_line:
;--------------------------
;     if s$ = "{x5F}" then begin
;       gosub read_next_line
        jsr read_next_line
;       goto cfcoso_skip  ' read next line
;     bend
      rts


;--------
check_sm0:
;--------
;     if sm = 0 and s$ <> "[" then begin
;       print "error: expected ["
;       sleep 1
;       stop
;     bend
; 
;     if sm = 0 and s$ = "[" then begin
;       sm = 1
;       goto cfcoso_skip
;     bend
      rts


;-------------------
check_sm1_after_sm2:
;-------------------
;     if sm = 1 and s$ = "[" then begin
;       sm = 2
;       field_count = 0
;     bend
; 
;     if sm = 1 and s$ = "]" then begin
;       sm = 0
;     bend
      rts


;-------------------
check_sm1_before_sm2:
;-------------------
;     if sm = 1 and s$ <> "[" and s$ <> "]" then begin
;       print "error: expected [ or ]"
;       sleep 1
;       stop
;     bend
      rts


;--------
check_sm2:
;--------
;     if sm = 2 then begin
;       if left$(s$, 1) = "]" then begin
;         sr = sr + 1
;         field_count = 0
;         sm = 1
;         s$ = ""
;         goto cfcoso_nextrow  ' next row
;       bend
; 
;       ' if left$(s$, 1) = dbl_quote$ then begin
;       ' ss$ = s$
;       ' tr$ = ""
;       ' do while right$(ss$, 2) <> (dbl_quote$ + ",")
;       '   gosub read_next_token
;       '   ss$ = ss$ + s$
;       '   print ss$
;       ' loop
;       ' s$ = ss$
;       ' tr$ = whitespace$
;       ' stop
;       if right$(s$, 1) = "," then begin
;         s$ = left$(s$, len(s$) - 1)
;       bend
;       s$ = struct_fields$(field_count) + "(" + str$(sr) + ")=" + s$
;       gosub replace_vars_and_labels
        jsr replace_vars_and_labels
;       gosub safe_add_to_current_or_next_line
        jsr safe_add_to_current_or_next_line
;       s$ = ""
;       field_count = field_count + 1  ' safe add to dest_line$(dest_lineno)
; 
; .cfcoso_nextrow
;     bend
    rts


;-------------------
parse_args_of_struct:
;-------------------
; input:
;  - bkt_open_idx (will determine whether it is dimensioned or not)
;  - struct_obj_name  ( e.g. 'envs(9)' )
;  - args  (e.g. args[0]="name$", args[1]="attack", ...)
;  - arg_cnt (number of args in array)
;
; output:
;  - var_table  (struct fields will each get added here)
;  - struct_fields  (struct fields will also get added here)
; 
    +ASSIGN_U8V_EQ_IMM ridx, $00
;   for ridx = 0 to arg_cnt
@loop_next_arg:
;     if args$(ridx) <> "" then begin
      +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_U8V args, ridx
      
;       print "args$(";ridx;")=";args$(ridx)
      +CMP_U8V_TO_IMM verbose, $01
      bne +
        +PRINT_INLINE "args$("
        +PRINT_U8V ridx
        +PRINT_INLINE ")="
        +PRINT_PSTR tmp_ptr
+:

        jsr parse_no_brackets_case  ; add simple var to var_table

        jsr parse_brackets_case     ; add dimensioned var to var_table (and struct_fields)
;     bend
;   next ridx
    inc ridx
    +CMP_U8V_TO_U8V ridx, arg_cnt
    bne @loop_next_arg
    rts


;------------------
parse_brackets_case:
;------------------
; inputs:
;   - args[ridx]  (which struct field we're focused on, e.g. "name$")
;   - struct_obj_name  ( e.g. 'envs(9)' )
;   - bkt_open_idx     ( e.g. = 4 )
; output:
;  - var_table  (added new field, e.g. "envs_name$")
;  - struct_fields[field_count]  (new field name is added in here)

;       if bkt_open_idx <> 0 then begin
        +CMP_U8V_TO_IMM bkt_open_idx, $ff
        lbeq @skip_parse_brackets
          phw s_ptr
          jsr gen_dimensioned_struct_field_name
          ; returns f_str (e.g. "envs_name$(9)")
          +plw s_ptr

;         var_name$ = s$
          +ASSIGN_U16V_EQ_ADDR var_name, f_str
;         gosub parse_declared_var
          +PUSH_U8V bkt_open_idx
          jsr parse_declared_var
          +PULL_U8V bkt_open_idx
; 
;         struct_fields$(field_count) = var_name$
          +ASSIGN_U16V_EQ_DEREF_U16V_WORDARRAY_AT_WORDIDX_OF_U8V is_ptr, struct_fields, field_count
          +HEAP_COPY_PSTR_EQ_PSTR is_ptr, var_name

  ;         field_count = field_count + 1
          inc field_count
;       ' if next_line$<>"" then delete_line_flag=0:cur_src_line$="^^"+next_line$:else delete_line_flag=1
@skip_parse_brackets:
  rts


;--------------------------------
gen_dimensioned_struct_field_name:
;--------------------------------
; input:
;  - struct_obj_name  (e.g. "envs(9)"
;  - ridx (index of which field name to generate full name for)
;  - bkt_open_idx
; output:
;  - f_str = "envs_name$(9)"

;         s$ = left$(struct_obj_name$, bkt_open_idx - 1) + "_" + args$(ridx) + mid$(struct_obj_name$, bkt_open_idx)
  +ASSIGN_U16V_EQ_ADDR s_ptr, f_str
  ldz bkt_open_idx
  dez
  +ASSIGN_U8V_EQ_IMM cur_line_len, $00
  +APPEND_LEFTZ_PSTR_TO_S_PTR struct_obj_name, bkt_open_idx
  +APPEND_IMM_CHR_TO_S_PTR '_'
  +ASSIGN_U16V_EQ_DEREF_U16V_WORDARRAY_AT_WORDIDX_OF_U8V is_ptr, args, ridx
  +ASSIGN_U16V_EQ_DEREF_U16V tmp2_ptr, is_ptr
  +APPEND_PSTR_TO_S_PTR tmp2_ptr
  +APPEND_MIDZ_PSTR_TO_S_PTR struct_obj_name, bkt_open_idx

;         print "struct: ";s$
  +CMP_U8V_TO_IMM verbose, $01
  bne @skip_verbose
    +PRINT_INLINE "struct: "
    +PRINT_PSTR s_ptr
    +PRINT_CHR $0d
@skip_verbose:

  rts


;---------------------
parse_no_brackets_case:
;---------------------
; inputs:
;   - bkt_open_idx (needs to be $ff in order to do anything)
;   - struct_obj_name (e.g., = "envs(9)" for the given example)
;   - args (e.g. args[0] = 'name$', args[1] = 'attack')
;   - ridx (current arg being assessed)
;
;             #struct ENVTYPE name$, attack, decay, sustain
;
;             ENVTYPE envs(9) = [ {x5F}
;             [ "Piano",       0,  9,  0  ], {x5F}
;
; outputs:
;   - struct_obj_name + "_" + args[ridx]
;             e.g. "envs_name$'
;   - var_table  (add simple var to var_table)

;       if bkt_open_idx = 0 then begin
        +CMP_U8V_TO_IMM bkt_open_idx, $ff
        lbne @skip_parse_no_brackets
;         s$ = struct_obj_name$ + "_" + args$(ridx)
          +COPY_PSTR_FROM_PSTR s_ptr, struct_obj_name
          +APPEND_IMM_CHR_TO_S_PTR "_"
          +ASSIGN_U16V_EQ_DEREF_U16V_WORDARRAY_AT_WORDIDX_OF_U8V is_ptr, args, ridx
          +ASSIGN_U16V_EQ_DEREF_U16V sr_ptr, is_ptr
          +APPEND_PSTR_TO_S_PTR sr_ptr

;         print "struct: "; s$
          +CMP_U8V_TO_IMM verbose, $01
          bne @skip_verbose1
            +PRINT_INLINE "struct: "
            +PRINT_PSTR s_ptr
            +PRINT_CHR $0d
@skip_verbose1:

;         var_name$ = s$
          +ASSIGN_U16V_EQ_U16V var_name, s_ptr
;         gosub parse_declared_var 
          jsr parse_declared_var
;       bend
@skip_parse_no_brackets:
  rts


;---------------
find_struct_type:
;---------------
; input:
;   - s_ptr = string where 1st token is a struct type (e.g., "ENVS")
; output:
;   - C (=0 for successfully found)
;       (=1 for not found)
;   - found_idx
;     = $ff (if not found)
;     = all else (found at idx)
;   gosub read_next_token  ' read next token from cur_src_line$ into s$
    jsr read_next_token

    +ASSIGN_U8V_EQ_IMM ridx, $00
    +SET_TMP_PTR_TO_WORDARRAY_AT_WORDIDX_OF_U8V struct_name, ridx
;   for ridx = 0 to struct_cnt - 1
@loop_next_struct_name:
      +CMP_U8V_TO_U8V ridx, struct_cnt
      beq @bail_out

;     if s$=struct_name$(ridx) then begin
      +CMP_PPSTR_TO_PSTR tmp_ptr, a_ptr
      bcs +
;       found_idx = ridx
        +ASSIGN_U8V_EQ_U8V found_idx, ridx
;       ridx = struct_cnt - 1
        clc  ; successfully found
        rts
;     bend
+:
;   next ridx
    inc ridx
    inw tmp_ptr
    inw tmp_ptr
    bra @loop_next_struct_name

@bail_out:
  +ASSIGN_U8V_EQ_IMM found_idx, $ff
; cur_src_line$ = orig$
; s$ = orig$
  +ASSIGN_U16V_EQ_U16V s_ptr, orig_ptr
; return
  sec   ; indicate failed to find
  rts


;--------------------------------
check_for_continue_onto_next_line:
;--------------------------------
;   if right$(cur_dest_line$, 1) = "{x5F}" then begin
;     cur_dest_line$ = left$(cur_dest_line$, len(cur_dest_line$) - 1)
;     next_line_flag = 0
;     cont_next_line_flag = 1
;   bend: else begin
;     cont_next_line_flag = 0
;   bend
;   return
    rts


;-------------------------------
safe_add_to_current_or_next_line:
;-------------------------------
;   ' --- safe add cur_dest_line$+s$ to current or next dest_line$(dest_lineno)
; 
;   if len(cur_dest_line$) + len(s$) + len(str$(dest_lineno)) >= 159 then begin
;     next_line_flag=1
;   bend
; 
;   if zz$ <> "" then begin
;     s$ = ""
;     zz$ = ""
;     return  ' force s$ to empty
;   bend
; 
;   if next_line_flag = 1 then begin
;     dest_line$(dest_lineno) = cur_dest_line$
;     cur_dest_line$ = s$
;     map_dest_to_src_lineno%(dest_lineno) = cur_src_lineno
;     dest_lineno = dest_lineno + 1
;     next_line_flag = 0
;   bend : else begin  ' -- add to cur_dest_line$
;     if cur_dest_line$ <> "" and cont_next_line_flag = 0 and right$(cur_dest_line$,1) <> ":" then begin
;       cur_dest_line$ = cur_dest_line$ + ":"
;     bend
;     
;     cur_dest_line$ = cur_dest_line$ + s$
;   bend
; 
;   if verbose then print "<<" dest_lineno; s$
;   return
    rts


; '---------
; .dump_vars
; '---------
;   for ty = 0 to 3
;     for id = 0 to element_cnt(ty)
;       gosub generate_varname  ' get gen_varname$ (optimised var name)
; 
;       print#1, str$(cur_dest_lineno) + " rem " {x5F}
;         + var_table$(ty,id) + " = :" + gen_varname$ + type_ident$(ty) + ":"
; 
;       cur_dest_lineno = cur_dest_lineno + 1
;     next id
;   next ty
;   return

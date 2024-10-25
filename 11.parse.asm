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
; 4.FF09  (2 bytes) line number for autojump
;
; 4.FF10  current file name
; 4.FF20  output file name
;
; 4.FF30- reserved
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


; ------
; MACROS
; ------

!macro set_lstring .var, .len, .val {
  +assign_u16v_eq_addr s_ptr, .var
  +assign_u8v_eq_imm cur_line_len, $00
  jsr print_inline_text_to_str
!pet .len, .val, $00  ; length-encoded in first byte
}

!macro set_string .var, .val {
  +assign_u16v_eq_addr s_ptr, .var
  +assign_u8v_eq_imm cur_line_len, $00
  jsr print_inline_text_to_str
!pet .val, $00
}


!macro assign_u8v_eq_imm .dest, .val {
  lda #.val
  sta .dest
}

!macro assign_u32v_eq_addr .dest, .addr1, .addr2 {
  lda #<.addr2
  sta .dest
  lda #>.addr2
  sta .dest+1
  lda #<.addr1
  sta .dest+2
  lda #>.addr1
  sta .dest+3
}

!macro assign_u16v_eq_addr .dest, .addr {
  lda #<.addr
  sta .dest
  lda #>.addr
  sta .dest+1
}

!macro assign_u16v_eq_u16v .dest, .src {
  lda .src
  sta .dest
  lda .src+1
  sta .dest+1
}

!macro assign_u16v_eq_deref_ptr_u16 .dest, .src {
  +assign_u16v_eq_u16v tmp_ptr, .src
  ldy #$00
  lda (tmp_ptr),y
  sta .dest
  iny
  lda (tmp_ptr),y
  sta .dest+1
}

!macro alloc .inptr, size {
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


!macro cmp_preproc_str .str {
;   if instr(cur_src_line$, "ifdef") = 2 then begin
    ldx #<.str
    ldy #>.str
    lda s_ptr
    ldz s_ptr+1
    jsr instr
}

!macro add_to_pointer_u8 .ptr, .val {
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
             ; (I'll try start it at $8000 and grow it upwards)
TMPHEAPPTR = $25 ;  $25-$26
TESTPTR = $27 ; $27-$28

initialise:
  sei

  lda #$00
  sta FOURPTR
  sta FOURPTR+1
  sta FOURPTR+3
  lda #$04
  sta FOURPTR+2

  lda #$00
  sta HEAPPTR
  lda #$80
  sta HEAPPTR+1

  ; allocate label_name$(200)
  +alloc label_name, 200*2

  lda #$00
  sta label_cnt

  ; allocate label_lineno(200)
  +alloc label_lineno, 200*2

  lda #$00
  sta element_cnt
  sta element_cnt+1
  sta element_cnt+2
  sta element_cnt+3

; #declare var_table$(4,200)             ' variable table per type
  ; ptr to memory for array (4*200*2 = 1600 bytes in size, $640)
  +alloc var_table, 4*200*2

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
MULTINA = $d770
MULTINB = $d774
MULTOUT = $d778

; 
;   clr
; 
; #declare dbl_quote$, parser_file$, t$, blank_line$, type_ident$
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
define_val = $1c40  ; ptr to memory for array (200*2 = 400 bytes in size, $190)

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
; #declare struct_fields$(30)
; #declare struct_vars$(30)              ' struct vars (each entry has string of vars)
; #declare struct_cnt=0                  ' index to next free struct definition
; 
; #declare cb
whitespace:
!byte 32, 160, $1d, $09, $00
space_only:
!pet ' '

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
ty:
!byte $00
dont_mark_label:
!byte $00
; #declare args_list$, args_list_len, args_idx, cur_ch$
ignore_delim_flag:
!byte $00
; #declare did_replace_flag
quote_flag:
!byte $00
; #declare expecting_label, default_delim$, rv_idx
rv_idx:
!byte $00
expecting_label:
!byte $00
a_str:
!fill 256, $00  ; length-encoded string of max 255 bytes
; #declare t, lc$, found_struct_idx, br, b, bc$, vl
; #declare n1, n2, ba, a, ad, tf$, found_idx, k, sf$, sf
; #declare src_line_ptr, src_linebuff_ptr
cur_line_len:
!byte $00
cut_tail_idx:
!byte $00

; #declare cur_line_len_minus_one, cur_linebuff_addr, chr
; #declare co$, ridx, sk$, bl, sz, sr, sm, zz$
; #declare cont_next_line_flag, tok_name$, clean_varname$
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
  pla
  clc
  adc #$01
  sta ret_ptr_lo
  pla
  adc #$00
  sta ret_ptr_hi

  jsr print_text

  lda ret_ptr_hi
  pha
  lda ret_ptr_lo
  pha
  rts

;-----------------------
print_inline_text_to_str:
;-----------------------
  pla
  clc
  adc #$01
  sta ret_ptr_lo
  pla
  adc #$00
  sta ret_ptr_hi

  jsr print_text_to_str

  lda ret_ptr_hi
  pha
  lda ret_ptr_lo
  pha
  rts


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

;---------
print_text_to_str:
;---------
  ldx #$00
  lda (ret_ptr_lo,x)
  beq @found_null

  ldy cur_line_len
  sta (s_ptr),y
  inc cur_line_len

  inc ret_ptr_lo
  bne print_text_to_str
  inc ret_ptr_hi
  bne print_text_to_str

@found_null:
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
    clc
    rol
    tax
    lda column_vals,x
    sta column_val
    lda column_vals+1,x
    sta column_val+1

;   column_digit = abs(val / column_val)
    ldz #$00
    ldy #$00
    ldx temp16+1
    lda temp16
    stq MULTINA

    ldx column_val+1
    lda column_val
    stq MULTINB

@wait_for_divide_to_finish:
    bit MULDIVBUSY  ; bit6 (mulbusy) = overflow flag
                    ; bit7 (divbusy) = negative flag
    bmi @wait_for_divide_to_finish

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

@wait_for_mult_to_finish:
    bit MULDIVBUSY  ; bit6 (mulbusy) = overflow flag
                    ; bit7 (divbusy) = negative flag
    bvs @wait_for_mult_to_finish

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
; tokens$(0)=" print input if then else do loop while until gosub goto open close dopen dclose for next getkey hex$ dim peek poke wait dec chr$ asc sgn sqr str$"
;   tokens$(0) = tokens$(0) + " graphic clr screen def begin bend len mid$ right$ left$ instr for next step trap border and foreground "
;   tokens$(1) = " background set abs sin cos tan log fre cursor pixel window rwindow line box circle ellipse palette restore data err$ er el cursor on off"
;   tokens$(1) = tokens$(1) + " val scratch return rnd stop bank ti do or st if el er on to pen get end int not ds run using dot "
;   tokens$(2) = " append atn auto backup bload boot bsave bump bverify catalog change char cmd collision color concat cont copy wpoke wpeek setbit clrbit "
;   tokens$(3) = " dclear deffn delete fn dir disk dload dma dmode dpat dsave dverify edma envelope erase exit exp fast filter find go64 header help highlight "
;   tokens$(4) = " joy list load locate lpen mod monitor mouse movspr new paint play pointer polygon pos pot pudef "
;   tokens$(5) = " rcolor rdot read record rem rename resume rgr rmouse rplay rreg rspcolor rsppos rsprite save scnclr sleep slow sound spc sprcolor "
;   tokens$(6) = " sprite sprsav sys tab tempo troff tron type usr verify vol xor key vsync rcursor t@& c@& rgraphic fread pointer "
;   dumb_cmds$ = " bload bsave dload to save dir collect dopen dclose backup fread get "
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
  
; 
;     ' strip whitespace from end
;     s$ = cur_src_line$
;     gosub strip_tr$_from_end
    jsr strip_tr_from_end

;     cur_src_line$ = s$
; 
;     if cur_src_line$ <> "" then begin
      lda cur_line_len
      beq @skip_line_parse
;       delete_line_flag = 0
        lda #$00
        sta delete_line_flag
; 
;       if verbose then print ">> DEST:" dest_lineno;", SRC: "; cur_src_lineno;": "; cur_src_line$
        lda verbose
        beq @skip_verbose

          jsr print_inline_text
!pet ">> DEST: ", $00

          ldx dest_lineno
          ldy dest_lineno+1
          jsr print_uint

          jsr print_inline_text
!pet ", SRC: ", $00

          ldx cur_src_lineno
          ldy cur_src_lineno+1
          jsr print_uint

          lda #':'
          jsr CHROUT
          lda #' '
          jsr CHROUT

          ldx s_ptr
          ldy s_ptr+1
          jsr print_str

          lda #$0d
          jsr CHROUT

@skip_verbose:
; 
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
; 
;       if left$(cur_src_line$, 1) = "#" then begin
      ldy #$00
      lda (s_ptr),y
      cmp #'#'
      bne +
;         gosub parse_preprocessor_directive
        jsr parse_preprocessor_directive
;       bend
+:
; 
;       if inside_ifdef = 1 then goto parser_loop_skip
; 
;       if left$(cur_src_line$, 4) = "data" or right$(cur_src_line$, 5) = "begin" then begin
;         next_line_flag = 1
;       bend
; 
;       if delete_line_flag = 0 then begin
;         if verbose = 0 then print ".";
;         s$ = cur_src_line$
;         gosub replace_vars_and_labels
;         gosub check_for_creation_of_struct_object
;         gosub check_for_continue_onto_next_line
;         ' safe add cur_dest_line$+s$ to current or next dest_line$(dest_lineno)
;         gosub safe_add_to_current_or_next_line
; 
;         if right$(s$, 4) = "bend" or right$(s$, 6) = "return" {x5F}
;            or left$(s$, 2) = "if" then begin
;           next_line_flag = 1
;         bend
;       bend  ' endif delete_line_flag = 0
;     bend  ' endif cur_src_line$ <> ""
@skip_line_parse:
; 
; .parser_loop_skip
;     ' increase source code line (for error msgs...)
;     cur_src_lineno = cur_src_lineno + 1
; 
;     if verbose then begin
;       print "cur_src_lineno="; cur_src_lineno
;       get key z$
;     bend
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
; 
; 
;----------------
declare_s_ptr_var:  ; declare_s$_var
;----------------
;   ' declare var(s) in s$
;   s$ = mid$(cur_src_line$, 10 - define_flag)
    +add_to_pointer_u8 s_ptr, 9
    sec
    lda cur_line_len
    sbc #09
    sta cur_line_len

    ; the (- define_flag) part
    lda define_flag
    beq +
    dew s_ptr
    clc
    inc cur_line_len

+:
;   ignore_brackets = 1
    lda #$01
    sta ignore_brackets

;   gosub parse_arguments
    jsr parse_arguments

;   ignore_brackets = 0  ' split parameters
    lda #$00
    sta ignore_brackets
; 
;   next_line$ = ""  ' new line if dimensioning...
    sta next_line
; 
;   if arg_cnt < 0 then begin
    lda arg_cnt
    cmp #$ff
    bne +
;     parser_error$ = "?declare parameter missing in line " + str$(cur_src_lineno)
    lda #$00
    sta parser_error
    sta cur_line_len
    +assign_u16v_eq_addr s_ptr, parser_error
    jsr print_inline_text_to_str

!pet "?declare parameter missing in line ", $00

    lda #$01
    sta to_str_flag
    ldx cur_src_lineno
    ldy cur_src_lineno+1
    jsr print_uint
    lda #$00
    sta to_str_flag

    ldy cur_line_len
    lda #$00
    sta (s_ptr),y
    inc cur_line_len

;     goto return_to_editor_with_error
    jmp return_to_editor_with_error
;   bend
+:
; 
    +assign_u16v_eq_u16v is_ptr, args

check:
    lda #$00
    sta arg_idx
;   for i = 0 to arg_cnt
@loop_next_arg:
;     var_name$ = args$(i)
      lda arg_idx
      clc
      rol
      tay
      
      lda args,y
      sta var_name
      iny
      lda args,y
      sta var_name+1

;     gosub parse_declared_var
      jsr parse_declared_var

;   next i
    inc arg_idx
    cmp arg_cnt
    bne @loop_next_arg

; 
;   if next_line$ <> "" then begin
;     delete_line_flag = 0
;     cur_src_line$ = "^^" + next_line$
;   bend : else begin
;     delete_line_flag = 1
;   bend
;   return
    rts

arg_idx:
!byte $00

; 
; 
;-----------------
parse_declared_var:
;-----------------
; input: var_name$ (e.g. '#define fishy = 1')
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
; 
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
      +assign_u16v_eq_addr tr_ptr, whitespace

;     s$ = var_name$
      +assign_u16v_eq_u16v s_ptr, var_name

;     gosub strip_tr$_from_end
      jsr strip_tr_from_end

;     var_name$ = s$
; 
;     s$ = value$
      +assign_u16v_eq_u16v s_ptr, value
      jsr get_s_ptr_length

;     gosub strip_tr$_from_beginning
      jsr strip_tr_from_beginning
;     gosub strip_tr$_from_end
      jsr strip_tr_from_end
;     value$ = s$
      +assign_u16v_eq_u16v value, s_ptr
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
; 
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
      adc #$01
      sta dimension+1
      inc dimension
      beq +
      inc dimension+1
+:

      ; put a null-term on the close bracket
      +assign_u16v_eq_u16v s_ptr, var_name
      lda #$00
      ldy bkt_close_idx
      sta (s_ptr),y

;     clean_varname$ = left$(var_name$, bkt_open_idx - 1)
      ldy bkt_open_idx
      sta (s_ptr),y

; 
;     s$ = dimension$
      +assign_u16v_eq_u16v s_ptr, dimension

;     dont_mark_label = 1
      lda #$01
      sta dont_mark_label

;     gosub replace_vars_and_labels
      jsr replace_vars_and_labels

;     dont_mark_label = 0
      +assign_u8v_eq_imm dont_mark_label, $00
;     dimension$ = s$
      +assign_u16v_eq_u16v dimension, s_ptr
; 
;     var_name$ = clean_varname$  ' check for define tokens
;     delete_line_flag = 0
      +assign_u8v_eq_imm delete_line_flag, $00
;   bend
@skip_found_brackets:
; 
;   ty = TYP_REAL  ' var type
    +assign_u8v_eq_imm ty, TYP_REAL

;   t$ = right$(var_name$, 1)  ' type (if any) in t$
    +assign_u16v_eq_u16v s_ptr, var_name
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
    bcs @skip_check_real
;     t$ = ""
      +assign_u8v_eq_imm cur_char, $00
;     ty = TYP_REAL
      +assign_u8v_eq_imm ty, TYP_REAL
;   bend
@skip_check_real:

; 
;   if define_flag = 1 then begin
    lda define_flag
    beq +:
;     ty = TYP_DEF
      +assign_u8v_eq_imm ty, TYP_DEF
;   bend
+:
; 
;   if t$ = "%" then begin
    lda cur_char
    cmp #'%'
    bne +
;     ty = TYP_INT
      +assign_u8v_eq_imm ty, TYP_INT
;   bend
+:
; 
;   if t$ = "$" then begin
    cmp #'$'
    bne +
;     ty = TYP_STR
      +assign_u8v_eq_imm ty, TYP_STR
;   bend
+:
; 
;   if t$ = "&" then begin
    cmp #'&'
    bne +
;     ty = TYP_BYTE
      +assign_u8v_eq_imm ty, TYP_BYTE
;   bend
+:
; 
;   var_table$(ty, element_cnt(ty)) = var_name$
    ; var_table(,) = ptr to memory for array (4*200*2 = 1600 bytes in size, $640)
    
    +assign_u16v_eq_u16v is_ptr, var_table
    ldx #$00
@loop_next_type:
    cpx ty
    beq @bail_type
    clc

    +add_to_pointer_u16 is_ptr, 200*2
    
    inx
    bra @loop_next_type
@bail_type:
   
    lda element_cnt,x
    tay
    
    ; warning: i suspect that varname comes from temporary args
    ; (which will eventually be removed from the heap)
    ; it might be safer to copy the var_name contents to a new string
    ; (but then I need to reconsider my 'temp' use of the heap)
    lda var_name
    sta (is_ptr),y
    iny
    lda var_name+1
    sta (is_ptr),y
; 
;   if dimension$ <> "" then begin
;     id = element_cnt(ty)
;     gosub generate_varname  ' fetch varname in gen_varname$
;     if define_flag = 0 then begin
;       next_line$ = next_line$ + "dim " + gen_varname$ + t$ + "(" + dimension$ + "):"
;     bend
;   bend
; 
;   if value$ <> "" then begin
;     id = element_cnt(ty)
;     gosub generate_varname 
;     if define_flag=0 then begin
;       next_line$ = next_line$ + gen_varname$ + t$ + "=" + value$ + ":"
;     bend
;   bend
; 
;   if define_flag = 1 then begin
;     define_val$(element_cnt(ty)) = value$
;   bend
; 
;   if verbose then begin
;     print var_name$; "{x92}: "; element_cnt(ty)
;   bend
; 
;   element_cnt(ty) = element_cnt(ty) + 1
;   return
    rts


vartype_delim:
!pet "%&$", $00

; 
; '---------------
; .set_output_file
; '---------------
;   s$ = mid$(cur_src_line$, 8)
;   gosub parse_arguments
; 
;   if arg_cnt <> 0 then begin
;     print "?invalid parameters in line ";cur_src_lineno
;     end
;   bend
; 
;   s$ = args$(0)
;   tr$ = dbl_quote$
;   gosub strip_tr$_from_beginning
;   gosub strip_tr$_from_end  ' quotes left & right
; 
;   if verbose then begin
;     print "setting output file to {x12}" + s$ + "{x92}"
;   bend
; 
;   of$ = s$
;   delete_line_flag = 1  ' disable passthrough
;   return
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
    lda #$00
    sta arg_cnt

;   args_list$ = s$
;   args_list_len = len(s$)
;   ignore_delim_flag = 0
    lda #$00
    sta ignore_delim_flag
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
    +assign_u16v_eq_addr TMPHEAPPTR, tempheap

    ldy #$00
    lda #$0
@loop_clr_ptrs:
;   for arg_idx = 0 to 31
;     args$(arg_idx) = ""
      sta args,y
      iny
;   next arg_idx
    cpy #64
    bne @loop_clr_ptrs

    ; set args$(0) to point at latest TMPHEAPPTR
    +assign_u16v_eq_u16v args, TMPHEAPPTR
    +assign_u16v_eq_u16v tmp_ptr, args
    +assign_u8v_eq_imm cur_arg_len, $00

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
      ldx #<delim
      ldy #>delim
      lda cur_char
      jsr instr_chr ; A=cur_ch,
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

        ; +assign_u16v_eq_u16v args, TMPHEAPPTR
        ; +assign_u16v_eq_u16v tmp_ptr, args
        inw TMPHEAPPTR
        lda TMPHEAPPTR
        sta args,y
        sta tmp_ptr
        iny
        lda TMPHEAPPTR+1
        sta args,y
        sta tmp_ptr+1

        +assign_u8v_eq_imm cur_arg_len, $00
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


;----------------------
replace_vars_and_labels:
;----------------------
;   ' -- replace vars & labels in source string --
;   '    in:   s$ = source string
;   '    out:  s$ = dest string with replaced items
;   
    +assign_u16v_eq_addr sr_ptr, f_str

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
; 
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
    +assign_u16v_eq_addr is_ptr, default_delim

    jsr get_s_ptr_length
; 
    sta rv_idx
;   for rv_idx = 1 to len(s$)
@loop_next_char:
      ldy rv_idx
      cpy cur_line_len
      lbeq @bail_for_loop

;     cur_ch$ = mid$(s$, rv_idx, 1)
      lda (s_ptr),y
      sta cur_char
; 
;     if cur_ch$ = ":" and quote_flag = 0 then begin
      cmp #':'
      bne +
      lda quote_flag
      bne +
;       shitty_syntax_flag = 0
        +assign_u8v_eq_imm shitty_syntax_flag, $00
;     bend
+:

;     if shitty_syntax_flag and cur_ch$ = "(" then begin
      lda shitty_syntax_flag
      beq +
      lda cur_char
      cmp #'('
      bne +
;       delim$ = default_delim$
        +assign_u16v_eq_addr is_ptr, default_delim
;     bend
+:

;     if shitty_syntax_flag and cur_ch$ = ")" then begin
      lda shitty_syntax_flag
      beq +
      lda cur_char
      cmp #'('
      bne +
;       delim$ = default_delim$ + "dpub"
        +assign_u16v_eq_addr is_ptr, default_plus_dpub_delim
;     bend
+:

      ; assess if a starting double-quote appeared
      ; (this implies that the prior content is a token)

      jsr dbl_quote_check
      bcs @rval_for_continue

; 
;     if instr(delim$, cur_ch$) <> 0 then begin
      jsr instr_chr_quick
      bcs @skip_to_delim_check_else

        jsr add_subbed_curtok_to_astr
;       if cur_ch$ = " " then cur_ch$ = ""
        lda cur_char
        cmp #' '
        bne +
          +assign_u8v_eq_imm cur_char, $00
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

    jsr add_subbed_curtok_to_astr

    +assign_u16v_eq_addr s_ptr, a_str+1
;   return
    rts


;------------------------
add_subbed_curtok_to_astr:
;------------------------
; gosub check_token_for_subbing
  jsr check_token_for_subbing
; a$ = a$ + cur_tok$
  jsr add_curtok_to_astr
; cur_tok$ = ""
  +assign_u8v_eq_imm cur_tok, $00
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
  ldx cur_tok   ; string length
  inx

  lda cur_char
  cmp #$00    ; if cur_char=$00, then ignore it (consider it "don't add a char")
  bne +
  rts

+:
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
; inputs: cur_tok$
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
;   ' decimal number check
;   ' - - - - - - - - - -
;   if val(cur_tok$) <> 0 then begin
;     expecting_label = 0
;     return  ' never change numbers
;   bend
; 
;   if cur_tok$ = "0" then begin
;     cur_tok$="."
;     return  ' stupid ms basic optimization
;   bend
; 
; 
;   ' check if we should mark this expected label
;   ' - - - - - - - - - - - - - - - - - - - - - -
;   if expecting_label and dont_mark_label = 0 then begin
;     gosub mark_cur_tok$_label
;     expecting_label = 0
;     return  ' replace label
;   bend
; 
; 
;   if cur_tok$ = "goto" then next_line_flag = 1
; 
; 
;   ' are we expecting a label next?
;   ' - - - - - - - - - - - - - - -
;   if cur_tok$ = "goto" or cur_tok$ = "gosub" or cur_tok$ = "trap" then begin
;     expecting_label = 1
;   bend
; 
; 
;   ' check hex value
;   ' - - - - - - - -
;   if left$(cur_tok$, 1) = "$" then begin
;     hx$ = mid$(cur_tok$, 2)
;     gosub check_hex
;     return
;    bend
; 
; 
;   ' check binary value
;   ' - - - - - - - - -
;   if left$(cur_tok$, 1) = "%" then begin
;     bi$ = mid$(cur_tok$, 2)
;     gosub check_binary
;     return
;   bend
; 
;   tok_name$ = " " + cur_tok$ + " "
; 
; 
;   ' ignore existing vocabulary of functions/commands
;   ' - - - - - - - - - - - - - - - - - - - - - - - - 
;   for t = 0 to 6
;     if instr(tokens$(t), tok_name$) <> 0 then begin
;       lc$ = cur_tok$
;       gosub check_if_command_triggers_shitty_syntax
;       return
;     bend
;   next
; 
; 
;   ' check to swap out variables with short names
;   ' - - - - - - - - - - - - - - - - - - - - - -
;   did_replace_flag = 0  ' did replace flag
; 
;   t$ = right$(cur_tok$, 1)
; 
;   ty = TYP_REAL
;   if t$ = "%" then ty = TYP_INT
;   if t$ = "$" then ty = TYP_STR
;   if t$ = "&" then ty = TYP_BYTE
; 
;   for id = 0 to element_cnt(ty)
;     if cur_tok$ = var_table$(ty, id) then begin
;       gosub generate_varname
;       cur_tok$ = gen_varname$ + type_ident$(ty)
;       id = element_cnt(ty)
;       did_replace_flag = 1
;     bend
;   next id
; 
;   if did_replace_flag = 1 then return
; 
; 
;   ' check defines table too
;   ' - - - - - - - - - - - -
;   for id = 0 to element_cnt(TYP_DEF)
;     if cur_tok$ = var_table$(TYP_DEF, id) then begin
;       cur_tok$ = define_val$(id)
;       return
;     bend
;   next id
; 
; 
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
; 
; 
;   ' PI token check
;   ' - - - - - - -
;   if asc(cur_tok$) = 222 then return
; 
; 
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
; 
; 
; .unresolved_cur_tok$
;   parser_error$ = "?unresolved identifier: '" + cur_tok$ + "' in line " + str$(cur_src_lineno)
;   sleep 1
;   goto return_to_editor_with_error
;   return
    rts
; 
; 
; '---------------------------------------
; .check_if_command_triggers_shitty_syntax
; '---------------------------------------
;   ' check if command triggers shitty syntax mode
;   ' todo: compare ubik's logic here versus my own 4079 lc$="dopen" logic
;   return
; 
; 
; '-------------
; .mark_cur_tok$_label
; '-------------
;   cur_tok$ = mk$ + cur_tok$ + mk$
;   return
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
; '------------
; .check_binary
; '------------
;   br = 0  ' result
; 
;   for b = 0 to len(bi$) - 1
;     bc$ = mid$(bi$, len(bi$) - b, 1)
;     if bc$ <> "1" and bc$ <> "0" then begin
;       bank 4
;       poke $ff08, 132
;       goto unresolved_cur_tok$
;     bend
;     if bc$="1" then br=br+bin_conv(b)
;   next b
; 
;   return
; 
; 
; '---------
; .check_hex
; '---------
;   trap illegal_hex_handler
;   vl = dec(hx$)
;   trap
;   return
; 
; 
; '-------------------
; .illegal_hex_handler
; '-------------------
;   trap
;   bank 4
;   poke $ff08, 131  ' set illegal hex
;   goto unresolved_cur_tok$  ' jump into error handler
; 
; 
; '----------------
; .generate_varname
; '----------------
;   ' generate varname from index
; 
;   if id < 26 then begin
;     gen_varname$ = chr$(65 + id)
;     return
;   bend
; 
;   n2 = mod(id, 26)
;   n1 = int(id / 26) - 1
;   gen_varname$ = chr$(65 + n1) + chr$(65 + n2)
; 
;   ' avoid any basic terms as var names
;   if gen_varname$="do" then gen_varname$="d1"
;   if gen_varname$="go" then gen_varname$="g1"
;   if gen_varname$="to" then gen_varname$="t1"
;   if gen_varname$="ds" then gen_varname$="d2"
;   if gen_varname$="dt" then gen_varname$="d3"
;   if gen_varname$="el" then gen_varname$="e1"
;   if gen_varname$="er" then gen_varname$="e2"
;   if gen_varname$="fn" then gen_varname$="f1"
;   if gen_varname$="if" then gen_varname$="i1"
;   return
;   stop
; 
; 
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
  cmp #'S'
  bne @skip_sig_check
  inz
  lda [FOURPTR],z
  cmp #'K'
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
; 
; 
;---------------
is_s_ptr_defined  ; is_s$_defined
;---------------
;   inside_ifdef = 1
  lda #$01
  sta inside_ifdef

  ldy #$00
@loop:
;   for k = 0 to element_cnt(TYP_DEF)
  cpy element_cnt + TYP_DEF
  beq @bail_out
;     if var_table$(TYP_DEF, k) = s$ then begin

    ; temp16 = 200*2*TYP_DEF + k * 2  (as each entry in array is a word)
    clc
    lda #<200*2*TYP_DEF
    sta temp16
    lda #>200*2*TYP_DEF
    sta temp16+1

    tya
    clc
    rol
    adc temp16
    sta temp16
    lda #$00
    rol
    adc temp16+1
    sta temp16+1

    ; tmp_ptr is pointer to indexed address within var_table$()
    clc
    lda #<var_table
    adc temp16
    sta tmp_ptr
    lda #>var_table
    adc temp16+1
    sta tmp_ptr+1

      jsr cmp_tmp_ptr_to_s_str
      bcs + ; skip if string not foud
;       inside_ifdef = 0
        lda #$00
        sta inside_ifdef
;     bend
+:
;   next k
  iny
  bra @loop

@bail_out:
;   return
  rts
; 
; 
; '----------------------
; .read_in_struct_details
; '----------------------
;   cur_src_line$ = mid$(cur_src_line$, 9)
;   gosub read_next_token  ' get next token in s$
; 
;   if s$ = "" then begin
;     print "error: no struct name found"
;     sleep 1
;     return
;   bend
; 
;   struct_name$(struct_cnt) = s$
;   struct_vars$(struct_cnt) = cur_src_line$
;   struct_cnt = struct_cnt + 1
;   return
; 
; 
; '---------------
; .read_next_token
; '---------------
;   ' read next token from cur_src_line$ into s$
;   s$ = cur_src_line$
;   gosub strip_tr$_from_beginning
;   gosub strip_tr$_from_end
;   cur_src_line$ = s$
;   sf$ = " "
;   sf = 0
; 
;   if left$(s$, 1) = dbl_quote$ then begin
;     sf$ = dbl_quote$ + ", "
;     sf = 2
;   bend
; 
;   a = instr(cur_src_line$, sf$)
; 
;   if a <> 0 then begin
;     s$ = mid$(cur_src_line$, 1, instr(cur_src_line$, sf$) + sf - 1)
;     cur_src_line$ = mid$(cur_src_line$, instr(cur_src_line$, sf$) + sf + 1)
;   bend
; 
;   if a = 0 then begin
;     s$ = cur_src_line$
;     cur_src_line$ = ""
;   bend
; 
;   return
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


s_ifdef:
!pet "ifdef", $00
s_endif:
!pet "endif", $00
s_define:
!pet "define", $00

; 
;---------------------------
parse_preprocessor_directive
;---------------------------
;   if instr(cur_src_line$, "ifdef") = 2 then begin
    +cmp_preproc_str s_ifdef
    bcs +
    cmp #$01
    bne +

;     s$ = mid$(cur_src_line$, 8)
      +add_to_pointer_u8 s_ptr, $07
      sec
      lda cur_line_len
      sbc #$07
      sta cur_line_len

;     gosub is_s$_defined
      jsr is_s_ptr_defined

;     delete_line_flag = 1
      lda #$01
      sta delete_line_flag

;   bend
+:
; 
;   if instr(cur_src_line$, "endif") = 2 then begin
    +cmp_preproc_str s_endif
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
    +cmp_preproc_str s_define
    bcs +
    cmp #$01
    bne +
;     define_flag = 1
      lda #$01
      sta define_flag

;     gosub declare_s$_var
      jsr declare_s_ptr_var

;     define_flag = 0
      lda #$00
      sta define_flag
;   bend
+:

  rts
; 
;   if instr(cur_src_line$, "declare") = 2 then begin
;     define_flag = 0
;     gosub declare_s$_var 
;   bend
; 
;   if instr(cur_src_line$,"output")=2 then begin
;     gosub set_output_file
;   bend
; 
;   if instr(cur_src_line$,"struct")=2 then begin
;     gosub read_in_struct_details
;     delete_line_flag = 1
;   bend
;   return
; 
; 
; '-----------------------------------
; .check_for_creation_of_struct_object
; '-----------------------------------
;   co$ = s$
;   cur_src_line$ = s$
;   found_idx = -1  ' preserve original string
; 
;   gosub read_next_token  ' read next token from cur_src_line$ into s$
; 
;   for ridx = 0 to struct_cnt - 1
;     if s$=struct_name$(ridx) then begin
;       found_idx = ridx
;       ridx = struct_cnt - 1
;     bend
;   next ridx
; 
;   if found_idx = -1 then begin
;     cur_src_line$ = co$
;     s$ = co$
;     return
;   bend
; 
;   gosub read_next_token
;   sk$ = s$  ' get struct object name
;   bl = instr(sk$, "(")
; 
;   ' *** found it, so make dim's for each member var
;   s$ = struct_vars$(found_idx)
;   gosub parse_arguments  ' parse args into args$(), arg_cnt
; 
;   found_idx = 1
;   sz = 0
; 
;   for ridx = 0 to arg_cnt
;     if args$(ridx) <> "" then begin
;       print "args$(";ridx;")=";args$(ridx)
; 
;       if bl = 0 then begin
;         s$ = sk$ + "_" + args$(ridx)
;         print "struct: "; s$
; 
;         var_name$ = s$
;         gosub parse_declared_var 
;       bend
; 
;       if bl <> 0 then begin
;         s$ = left$(sk$, bl - 1) + "_" + args$(ridx) + mid$(sk$, bl)
;         print "struct: ";s$
; 
;         var_name$ = s$
;         gosub parse_declared_var
; 
;         struct_fields$(sz) = var_name$
;         sz = sz + 1
;       ' if next_line$<>"" then delete_line_flag=0:cur_src_line$="^^"+next_line$:else delete_line_flag=1
;     bend
;   next ridx
; 
;   s$ = next_line$
;   gosub safe_add_to_current_or_next_line
;   next_line$ = ""  ' safe add cur_dest_line$+s$ to current dest_line$(dest_lineno)
; 
;   gosub read_next_token
; 
;   if s$ <> "=" then begin
;     cur_src_line$=""
;     return
;   bend
; 
;   gosub read_next_token
; 
;   sz=0
;   sr=0
;   sm=0  ' read next token from cur_src_line$ into s$
; 
;   do while s$ <> ""
;     if s$ = "{x5F}" then begin
;       gosub read_next_line
;       goto cfcoso_skip  ' read next line
;     bend
; 
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
; 
;     if sm = 1 and s$ <> "[" and s$ <> "]" then begin
;       print "error: expected [ or ]"
;       sleep 1
;       stop
;     bend
; 
;     if sm = 2 then begin
;       if left$(s$, 1) = "]" then begin
;         sr = sr + 1
;         sz = 0
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
;       s$ = struct_fields$(sz) + "(" + str$(sr) + ")=" + s$
;       gosub replace_vars_and_labels
;       gosub safe_add_to_current_or_next_line
;       s$ = ""
;       sz = sz + 1  ' safe add to dest_line$(dest_lineno)
; 
; .cfcoso_nextrow
;     bend
; 
;     if sm = 1 and s$ = "[" then begin
;       sm = 2
;       sz = 0
;     bend
; 
;     if sm = 1 and s$ = "]" then begin
;       sm = 0
;     bend
; 
; .cfcoso_skip
;     gosub read_next_token  ' read next token from cur_src_line$ into s$
;   loop
; 
;   s$ = ""
;   cur_src_line$ = ""
;   next_line$ = ""
;   zz$ = "z"
;   return
; 
; 
; '---------------------------------
; .check_for_continue_onto_next_line
; '---------------------------------
;   if right$(cur_dest_line$, 1) = "{x5F}" then begin
;     cur_dest_line$ = left$(cur_dest_line$, len(cur_dest_line$) - 1)
;     next_line_flag = 0
;     cont_next_line_flag = 1
;   bend: else begin
;     cont_next_line_flag = 0
;   bend
;   return
; 
; 
; '--------------------------------
; .safe_add_to_current_or_next_line
; '--------------------------------
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
; 
; 
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

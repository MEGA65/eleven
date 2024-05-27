last_ypos=0
last_yoffs=0
last_xpos=0
goto general_init_and_main_loop  ' buffer of past line location


.draw_text_line
'--------------
  a$=mid$(a$,x_offs)
  b=len(a$)
  o=b>80
  print"{rvof}";
  if o then b=79
  p=pointer(a$)
  bank 0
  m=peek(p+1)+256*peek(p+2)
  c=m+b-1
  if b=0 then 6
  if iv=1 then print "{rvon}";
  bank 1
  for a = m to c
    za=peek(a)
    printtransl$(za);
    if iv=1 and za=34 then print "{rvon}";
  next
  bank 128
  if o then begin
    foreground hl
    printtab(79);"$";
    foreground fg
  bend
  return

.general_init_and_main_loop
'--------------------------
  trap error_handler
  palette restore
  poke 0,65             ' m65 fast mode
  gosub ml_helper ' install m/l
  pf$="11.edit"
  key 7,"scratch "+chr$(34)+pf$+chr$(34)+":dsave"+chr$(34)+pf$+chr$(34)+":dverify"+chr$(34)+pf$+chr$(34)
  if rwindow(2)<>80 then print chr$(27)+"x"; ' force 80cols mode
  gosub read_defaults_from_bank4
  background bg
  border bo
  foreground fg
  print"{home}{home}{clr}{swlc}"+chr$(27)+"m";
  gosub clear_status_bar
  print"{rvon} ee v0.5.0{rvof}";
  gosub enable_raw_fn_keys

  max_lines=2000                ' max number of lines
  esc_flag%=0                  ' escape flag
  t$="                                                                               "
  blank_line$=t$+t$+t$:t$=""
  quote$=chr$(34)            ' fast access to quote char
  format_off$=chr$(27)+chr$(27)  ' escape flash etc.
  ctrl_chars$="{home}{clr}{f2}{$84}{left}{up}{down}{rght}{f1}{f3}{f5}{f7}{f4}{f8}{CTRL-O}{CTRL-Z}{CTRL-P}{$83}{CTRL-W}{CTRL-U}{inst}{blu}"+chr$(22)+chr$(20)+chr$(148)+chr$(13)  ' ctrl chars
  horz_move_chars$="{left}{rght}"+chr$(20)      ' horizontal movement characters
  file_changed_ind$(0)=" ":file_changed_ind$(1)="*"  ' for file changed indicator
  control_mode_ind$(0)="    ":         ' for control char mode indicator
  control_mode_ind$(1)="{CTRL-O}ctrl"+format_off$
  disk_op_result$(1)="success: ":disk_op_result$(0)="error: "  ' disc operation result
  disk_op_result_clr(1)=sb:disk_op_result_clr(0)=hl                  ' disc operation result colors
  default_file$="11.defaults"                  ' defaults file name
  dim line_buff$(max_lines)            ' line buffer
  dim clipboard$(100)         ' clipboard of lines
  dim label_name$(72),label_lineno(72),filtered_name$(72),filtered_lineno(72)    ' marker table
  ee$(0)="variable not declared"

' --- build char translation table ---

dim transl$(255)            ' char translation table
for a=1 to 31:transl$(a)="{rvon}"+chr$(64+a)+"{rvof}":transl$(128+a)="{rvon}"+chr$(192+a)+"{rvof}":next a
for a=32 to 127:transl$(a)=chr$(a):transl$(a+128)=chr$(a+128):next a
transl$(34)=chr$(34)+format_off$
transl$(160)="{CBM-+}"  ' make shift+space visible


x_offs=1  ' horizontal scroll index in current line
bank 128
poke 248,peek(248)or2^6     ' disable screen editor line linking
x_pos=0:y_pos=0:status_line_y=rwindow(1)-2  ' cursor pos, status line position
if curr_file$<>"" then begin
  if peek($4ff70)=55 and peek($4ff71)=77 then begin
    gosub copy_attic_ram_to_line_buffer
  bend:else begin
    gosub load_file ' load file if set
    gosub copy_line_buffer_to_attic_ram ' keep copy of file in attic ram
    poke $4ff70,55,77
    if disk_errno<>0 then curr_file$=""  ' delete current filename if file not there
  bend
bend

' uncomment this for output performance test
' ts=ti:for te=0 to 10:gosub 2080:next te:print"{down}{down}{down}"ti-ts:stop

gosub redraw_screen ' redraw screen
gosub build_status_bar ' build status bar

' **********************
' ** main editor loop **
' **********************

bank 128:sys ml_v4  ' enable vic iv
if jump_line<>-1 then begin
  ly=jump_line:context_lines=5:gosub safe_ly ' bring line in context
  if peek($4ff30)<>0 then begin
    r=0:parser_error$="":do:p=peek($4ff30+r):parser_error$=parser_error$+chr$(p):r=r+1:loop while p<>0
    gosub clear_status_bar:print "{rvon}";parser_error$;:redraw_status=1
    poke $4ff30,0
  bend
  jump_line=-1:context_lines=0              ' clear jump line
bend

do
.main_loop
  if y_pos-y_offs<0 then y_offs=0:y_pos=0
  if redraw_status<>1 then po$="      {left}{left}{left}{left}{left}{left}"+mid$(str$(x_pos),2)+":"+mid$(str$(y_pos),2):gosub display_status
  curr_line$=line_buff$(y_pos)
  if (y_pos-y_offs)>rwindow(1)-1 then y_pos=y_offs+rwindow(1)-1
  cursor x_pos,y_pos-y_offs
  foreground sb
  cursor on
  getkey t$
  cursor off
  foreground fg

  if mf=1 then gosub mark_mode_handler : goto main_loop  ' mark-mode handling
  if redraw_status=1 then gosub build_status_bar: bank 128:sys ml_v4: redraw_status=0
  bank 128:alt_key = peek(54801) and 16  ' check alt key
  ' -- if t$=f11 (also ctrl+5/green for xemu?)
  if t$="{pur}" or (alt_key and t$="c") then control_mode=abs(control_mode-1) : goto main_loop
    ctrl_or_alt = (instr(ctrl_chars$,t$) and control_mode=0) or alt_key
   if ctrl_or_alt then begin
    if t$="g" then t$="{CTRL-P}"  ' workaround for challenged keyboards
    if t$="s" then t$="{CTRL-Z}"
    if t$=chr$(13) then changed=1: gosub insert_return  ' insert return
    if instr(horz_move_chars$,t$)=0 and x_offs>1 then begin
       x_offs=1:cursor 0,y_pos-y_offs:curr_line$=line_buff$(y_pos):a$=curr_line$:gosub draw_text_line
       if t$=chr$(13) then cursor 0,y_pos-y_offs-1:a$=line_buff$(y_pos-1):gosub draw_text_line
    bend
    if t$="{CTRL-W}" then gosub next_word  ' ctrl-w = next word
    if t$="{CTRL-U}" then gosub previous_word ' ctrl-u = previous word
    if t$="{inst}" then gosub delete_curr_char ' shift-del = delete current char
    if t$="{blu}" then poke $4ff07,peek($4ff07) xor 8:if peek($4ff07)and8 then play "t0o5sg"  ' toggle debug on/off
    if t$="p" then gosub post_file_to_pc
    if t$="z" then gosub navigate_to_label_under_cursor
    if t$="f" then fr%=0 : gosub find_replace  ' find
    if t$="r" then fr%=1 : gosub find_replace  ' find and replace
    if t$="{f3}" then gosub save_file  ' save
    if t$="{f4}" then sa=1:gosub save_file  ' save as
    if t$="{f1}" then gosub load_file_prompt  ' load
    if t$="{$83}" then gosub quit_ee  ' stop
    if t$="{f2}" then gosub new_file  ' new
    if t$="{f7}" then gosub show_labels  ' labels
    if t$="{CTRL-P}" then gosub jump_to_line  ' goto line
    if t$="{f8}" then gosub jump_to_prev_file  ' go to previous file (pv$)
    if t$="{$84}" then gosub show_help  ' help
    if t$="{CTRL-O}" then temp=y_pos:y_pos=last_ypos:last_ypos=temp: temp=y_offs:y_offs=last_yoffs:last_yoffs=temp: temp=x_pos:x_pos=last_xpos:last_xpos=temp:gosub  redraw_screen
    if t$="{home}" then begin            ' home/end
      if alt_key=0 then ly=0:x_pos=0:gosub safe_ly: else ly=lastline_y:x_pos=0:gosub safe_ly
    bend
    if t$="{rght}" then begin  ' -------- cursor right
      if alt_key then begin
         if 79+x_offs<len(curr_line$) then x_offs=x_offs+1:cursor0,y_pos-y_offs:a$=curr_line$:gosub draw_text_line
      bend : else x_pos=x_pos+1
      if x_pos>79 then begin
        x_pos=79:if x_pos+x_offs<len(curr_line$) then x_offs=x_offs+1
        cursor 0,y_pos-y_offs:a$=curr_line$:gosub draw_text_line
      bend
    bend
    if t$="{left}" then begin
      if x_pos>0 then x_pos=x_pos-1 : else if y_pos>0 then y_pos=y_pos-1:x_pos=len(line_buff$(y_pos)):if x_pos>79 then x_pos=79
    bend
    if t$="{down}" then begin
      if alt_key then ly=y_pos+status_line_y:gosub safe_ly:else y_pos=y_pos+1
      if y_pos-y_offs>status_line_y-3 then gosub scroll_up : if y_pos-y_offs>status_line_y then y_pos=y_pos-1
    bend
    if t$="{up}" then begin
      if alt_key then ly=y_pos-status_line_y:gosub safe_ly:else y_pos=y_pos-1
      if y_pos-y_offs<0 then gosub scroll_down : if y_pos-y_offs<0 then y_pos=y_pos+1
    bend
    if t$="{f5}" then gosub compile  ' --- f5 - compile
    if t$=chr$(20) then begin ' delete
      changed=1
      if x_pos=0 or alt_key then gosub merge_lines :else begin  ' merge with last line
      curr_line$=left$(curr_line$,x_pos+x_offs-2)+mid$(curr_line$,x_pos+x_offs)
      line_buff$(y_pos)=curr_line$
      cursor 0,y_pos-y_offs:a$=curr_line$:gosub draw_text_line:if len(curr_line$)<=79 then print " ";
      if x_pos>0 then x_pos=x_pos-1
    bend
  bend
  if t$="{CTRL-Z}" then begin  ' --- f14 - save defaults
    gosub clear_status_bar:print"{rvon}saving defaults...{rvof}";
    scratch (default_file$):bsave (default_file$),b(4),p(dec("ff00")) to p(dec("ff80"))
    disk_errno=ds : disk_error$=ds$
    foreground disk_op_result_clr(abs(disk_errno=0))
    gosub clear_status_bar:print "{rvon}";disk_op_result$(abs(disk_errno=0));disk_error$;" ";:foreground sb
    redraw_status=1
  bend
bend:else begin:if t$=chr$(27) then begin ' -- check escape flag
  t$="":if esc_flag%=0 then esc_flag%=1:else esc_flag%=0
bend:else begin:if esc_flag%=1 then begin  ' -- check escape characters
  if t$="k" then x_pos=len(curr_line$):if x_pos>78 then x_offs=x_pos-78:x_pos=78:gosub redraw_line
  if t$="j" then x_pos=0:if x_offs>1 then x_offs=1:gosub redraw_line
  if t$="d" then alt_key=1:gosub merge_lines :changed=1
  if t$="m" then mf=1:ms=y_pos:me=y_pos:gosub redraw_line
  if t$="p" then gosub paste
  esc_flag%=0
bend:else begin  ' -- insert char
    changed=1
    if x_pos=79 then x_offs=x_offs+1
    do while len(curr_line$)<x_pos:curr_line$=curr_line$+" ":loop ' fill up spaces if needed
    curr_line$=left$(curr_line$,x_pos+x_offs-1)+t$+mid$(curr_line$,x_pos+x_offs)
    line_buff$(y_pos)=curr_line$
    cursor 0,y_pos-y_offs:a$=curr_line$:gosub draw_text_line
    x_pos=x_pos+1 : if x_pos>79 then x_pos=79
  bend ' endif esc_flag%=1
  bend ' endif t$=chr$(27)
  bend ' endif ctrl_or_alt (ctrl-char)
loop until qu=1
stop

' ==================== subroutines =====================

.load_file
'---------
' --- read in f$
open 1,1,5,curr_file$+",s,r":disk_errno=ds:disk_error$=ds$
if disk_errno<>0 then curr_file$=of$:close 1:return
for a = 0 to max_lines:line_buff$(a)="":next a:lc=0
lastline_y=0:y_offs=0:y_pos=0:x_pos=0
do
  l$="":line input#1,l$
  line_buff$(lastline_y)=l$
  lastline_y=lastline_y+1
loop while st=0
lastline_y=lastline_y-1:changed=0
close 1
return


.redraw_screen
'-------------
foreground fg : print "{rvof}";
if y_offs<0 then y_offs=0
for i=0 to status_line_y
  cursor 0,i
  a$=line_buff$(y_offs+i)
  print chr$(27)+"q";:zy=y_pos:y_pos=y_offs+i:gosub invert_if_marked :y_pos=zy:gosub draw_text_line
next i
return


.scroll_up
'---------
if y_offs+status_line_y>lastline_y-1 then return
y_offs=y_offs+1  ' current top
s$=line_buff$(y_offs+status_line_y)
cursor 0,0:print chr$(27)+"v";
cursor 0,status_line_y:a$=s$:gosub draw_text_line
return


.scroll_down
'-----------
if y_offs<=0 then return
y_offs=y_offs-1
s$=line_buff$(y_offs)
cursor0,0:print chr$(27)+"w";:a$=s$:gosub draw_text_line
return


.display_status
'--------------
' --- display status in st$
foreground sb
print"{home}{home}";:cursor 20,status_line_y+1:print"{rvon}"+po$;
cursor 75,status_line_y+1:print control_mode_ind$(control_mode);
cursor 0,status_line_y+1:print"{rvon}"+file_changed_ind$(changed)+"{rvof}";
window 0,0,79,status_line_y
foreground fg
return


.clear_status_bar
'----------------
if rwindow(1)=25 then cb=2048+(24*80) : fb=$ff80000 + (24*80)
if rwindow(1)=50 then cb=$40800+(49*80) : fb=$ff80000 + (49*80)
edma 3,80,160,cb        '  --- clear status bar
edma 3,80,sb,fb         '  set status bar colour
print"{home}{home}";:cursor 0,status_line_y+1
foreground sb
return


.build_status_bar
'----------------
gosub clear_status_bar
print"{home}{home}";:cursor 18,status_line_y+1:print"{rvon}{SHIFT--}       {SHIFT--} {rvof}f1{rvon}load {rvof}f3{rvon}save {rvof}f5{rvon}compile {rvof}f7{rvon}lbls {rvof}f9{rvon}go {rvof}f11{rvon}cmode {SHIFT--}";
cursor 2,status_line_y+1:if curr_file$<>"" then print curr_file$;:else print "<untitled>";
window 0,0,79,status_line_y
foreground fg
return


.insert_return
'-------------
 y_pos=y_pos+1
 lastline_y=lastline_y+1
 if y_pos>lastline_y then lastline_y=y_pos
 fora=lastline_y+1to y_pos+1 step-1:line_buff$(a)=line_buff$(a-1):next a:line_buff$(y_pos)=""
 line_buff$(y_pos)=mid$(line_buff$(y_pos-1),x_pos+x_offs)
 line_buff$(y_pos-1)=left$(line_buff$(y_pos-1),x_pos+x_offs-1)
 x_pos=0:sc=1:pl$=line_buff$(y_pos-1)
 do while mid$(pl$,sc,1)=" "  ' --- autoindent
   if line_buff$(y_pos)<>"" then line_buff$(y_pos)=" "+line_buff$(y_pos)
   sc=sc+1:x_pos=x_pos+1
 loop
 if y_pos-y_offs>status_line_y then gosub scroll_up  ' scroll up
 cursor x_pos,y_pos-y_offs:print chr$(27)+"i";
 a$=line_buff$(y_pos):gosub draw_text_line
 cursor x_pos-sc+1,y_pos-y_offs-1:print chr$(27)+"q";
 a$=line_buff$(y_pos-1):gosub draw_text_line
 return


.merge_lines
'-----------
' --- merge lines
if alt_key=0 and y_pos<1 then return
if alt_key then sx=x_pos:x_pos=0:y_pos=y_pos-1:goto mg_skip
y_pos=y_pos-1:x_pos=len(line_buff$(y_pos))
line_buff$(y_pos)=line_buff$(y_pos)+line_buff$(y_pos+1)
.mg_skip
for a=y_pos+1 to lastline_y-1:line_buff$(a)=line_buff$(a+1):next a
if y_pos<=lastline_y then line_buff$(lastline_y)=""
if lastline_y>0 then lastline_y=lastline_y-1

if alt_key=0 then cursor 0,y_pos-y_offs:a$=line_buff$(y_pos):gosub draw_text_line
cursor 0,y_pos-y_offs+1:printchr$(27)+"d";
cursor 0,status_line_y:a$=line_buff$(y_offs+status_line_y):gosub draw_text_line
if y_offs+status_line_y>lastline_y-1 then cursor 0,status_line_y:print chr$(27)+"q";
if alt_key then x_pos=sx:y_pos=y_pos+1
return


.save_file
'---------
if bu and curr_file$<>"" and sa=0 then begin
  gosub clear_status_bar:print"{rvon}backing up old file...";
  b$=left$(curr_file$,12)+".bak"
  delete (b$)
  copy (curr_file$) to (b$)
bend
if curr_file$="" or sa=1 then sa=0 : gosub save_file_prompt  ' save as
if curr_file$="" then gosub build_status_bar: return
gosub clear_status_bar:print"{rvon}saving...";
delete (curr_file$)
i = max_lines:do while line_buff$(i)="":i=i-1:loop
lastline_y=i
open 1,1,3,curr_file$+",s,w":disk_errno=ds:disk_error$=ds$
for a=0 to lastline_y
  print#1,line_buff$(a)
next a
if ds<>0 then disk_errno=ds
close 1
gosub clear_status_bar
if disk_errno=0 then changed=0
foreground disk_op_result_clr(abs(disk_errno=0))
print disk_op_result$(abs(disk_errno=0));disk_error$;" ";:foreground sb
gosub save_filename_in_mailbox  ' save filename in mailbox ram
redraw_status=1
return


.load_file_prompt
'----------------
' --- load file
gosub clear_status_bar
of$=curr_file$
if changed=1 then begin
  print "{rvon}current file "+curr_file$+" not saved. really proceed? (y/n) {CTRL-O}{CBM-+}"+format_off$;
  do:getkey t$:loop until instr("yn",t$)
  gosub clear_status_bar
  if t$="n" then gosub build_status_bar:return
bend
try
gosub clear_status_bar:print "{rvon}load file name ($ for directory, return to cancel) "+chr$(27)+"t{clr}";
nf$=""
line input "";nf$
if len(nf$)>0 and left$(nf$,1)="$" then begin
  window0,0,79,status_line_y : print "{clr}"; : foreground fg
  if len(nf$) = 1 then dir : else begin
    dir (mid$(nf$,2))
  bend
  goto lf_retry
bend

.load_specified_file
print"{home}{home}";:gosub clear_status_bar
if nf$="" then gosub build_status_bar:gosub redraw_screen:return
pv$=curr_file$:pv=y_pos:curr_file$=nf$:print "{rvon}loading "+curr_file$+"...";
gosub load_file
print"{home}{home}{clr}"+format_off$;
gosub redraw_screen ' redraw screen
if disk_errno=0 then gosub save_filename_in_mailbox  ' persist filename
gosub clear_status_bar : foreground disk_op_result_clr(abs(disk_errno=0))
print "{rvon}";curr_file$;" - ";disk_op_result$(abs(disk_errno=0));disk_error$;" ";:foreground sb
if disk_errno=0 then print"{rvon}; read ";lastline_y; "lines.";
redraw_status=1
return


.save_file_prompt
'----------------
gosub clear_status_bar:print "{rvon}save file name (return to cancel) "+chr$(27)+"t{clr}";
nf$=""
line input "";nf$
print"{home}{home}";:gosub clear_status_bar
curr_file$=nf$
return


.safe_ly
'-------
'--- ensure ly is at sane position end
'     make line ly the current line
' if ly>lastline_y-status_line_y then ly=lastline_y-status_line_y
y_offs=ly :y_pos=ly
if y_offs>context_lines then y_offs=y_offs-context_lines
goto sl_skip
if y_offs<0 then y_offs=0
if y_pos<0 then y_pos=0
if y_pos>lastline_y then y_pos=lastline_y : y_offs=y_pos-status_line_y+3
if y_offs>context_lines then y_offs=y_offs-context_lines  ' context lines
if y_offs<0 then y_offs=0

.sl_skip
gosub redraw_screen
return


.jump_to_line
'------------
' --- query line no
gosub clear_status_bar:print "{rvon}jump to line # (return to cancel) "+chr$(27)+"t{clr}";
nn$="":line input nn$
nn=val(nn$):print"{home}{home}";:gosub clear_status_bar
if nn>lastline_y then return
ly=nn:context_lines=8:gosub safe_ly
gosub build_status_bar
return


.show_help
'---------
' --- help
print"{home}{home}{clr}-- cursor movement --",,"-- editor functions --{down}"
print"{rvon}cursor keys  {rvof} move the cursor"
print"{rvon}alt + up     {rvof} scroll up 1 page"
print"{rvon}alt + down   {rvof} scroll down 1 page"
print"{rvon}alt + left {$a0} {rvof} scroll left 1 char(*)"
print"{rvon}alt + right  {rvof} scroll right 1 char(*)"
print"{down}{rvon}shift + clr  {rvof} go to top of file"
print"{rvon}alt + clr    {rvof} go to end of file"
print"{down}{rvon}alt + delete {rvof} delete current line"
print"{down}{rvon}alt + return {rvof} to start of next line"
print"{down}{rvon}alt + f      {rvof} find"
print"{rvon}alt + r      {rvof} find & replace"
print"{rvon}alt + z      {rvof} go to label under cursor"
print"{down}*) only in lines with >80 characters"
print"{down}{rvon}#{rvof}) xemu-equivalent shortcut key"
print"{home}{down}{down}",,,,"{rvon} f1 {rvof} load file"
print,,,,"{rvon} f2 {rvof} new file"
print,,,,"{rvon} f3 {rvof} save file"
print,,,,"{rvon} f4 {rvof} save file as"
print,,,,"{down}{rvon} f5 {rvof} compile + run"
print,,,,"{rvon} f6 {rvof} just compile"
print,,,,"{down}{rvon} f7 {rvof} go to label"
print,,,,"{rvon} f8 {rvof} go to previous file"
print,,,,"{rvon} f9 {rvof} go to line nr     ({rvon}#{rvof})ctrl + p"
print,,,,"{rvon}ctrl-o{rvof} jump to loc prior to f7"
print,,,,"{down}{rvon}f11 {rvof} toggle ctrl mode  ({rvon}#{rvof})ctrl + 5"
print,,,,"{down}{rvon}f14 {rvof} update defaults   ({rvon}#{rvof})ctrl + z"
print,,,,"{down}{rvon}help{rvof} this screen       ({rvon}#{rvof})pageup"
gosub more_help  ' more help
gosub clear_status_bar :print"{rvon}ee cheat sheet -- press any key to return to editor{rvof}";
getkey t$:gosub redraw_screen:gosub build_status_bar:return


.read_defaults_from_bank4
'------------------------
' --- set filename from mailbox ram
bank 4:t$=chr$(peek(dec("ff00")))+chr$(peek(dec("ff01")))
if t$<>"sk" then bo=6:fg=5:bg=0:sb=14:hl=2:jump_line=-1:return
jump_line=-1  ' jump line
al=peek(dec("ff07"))and1  ' autoload flag?
if al=0 then goto skip_autoload
tf$="":i=dec("ff10"):do while peek(i)<>0
tf$=tf$+chr$(peek(i)):i=i+1:loop
curr_file$=tf$
if peek(dec("ff07"))and2 then begin  ' autojump flag
poke dec("ff07"),peek(dec("ff07"))and(255 xor 2)
jump_line=peek(dec("ff09"))+256*peek(dec("ff0a")):ee=peek(dec("ff08"))
bend

.skip_autoload
bo=peek(dec("ff02")):bg=peek(dec("ff03")):fg=peek(dec("ff04"))
hl=peek(dec("ff05")):sb=peek(dec("ff06"))
bu=peek(dec("ff07"))and4  ' backup flag
sm=peek($4ff07)and8:if sm then print chr$(27)+"5";:else print chr$(27)+"8"
bank 128
return


.new_file
'--------
if ch then begin
  gosub clear_status_bar:print"{rvon}Current file was modified! Really start over (y/n)? {CTRL-O}{CBM-+}";
  do:getkey t$:loop until t$="y" or t$="n"
  if t$="n" then gosub build_status_bar:    return
bend
for a = 0 to max_lines:line_buff$(a)="":next a:lc=0
curr_file$=""
print"{home}{home}{clr}":gosub redraw_screen
gosub build_status_bar
changed=0:x_pos=0:y_pos=0:y_offs=0
gosub clear_status_bar:print"{rvon}** New file **{rvof}";:redraw_status=1
return


.quit_ee
'-------
gosub clear_status_bar:print"{rvon}really quit ee? {CTRL-O}{CBM-+}"+chr$(27)+"o";
do:getkey t$:loop until t$="y" or t$="n"
if t$="n" then gosub build_status_bar:return
bank 128:poke248,0:key on:palette restore
print"{home}{home}{clr}tschuessn...!"
end


.filter_labels
'-------------
'--- filter labels based on fz$
for a=0 to 72:filtered_name$(a)="":filtered_lineno(a)=0:next a:fz=0:mp=-1
for a=0 to mn-1:if (len(fz$)>0 and instr(label_name$(a),fz$)>0)or len(fz$)=0 then begin
  filtered_name$(fz)=label_name$(a):filtered_lineno(fz)=label_lineno(a):fz=fz+1
  if label_lineno(a) <= y_pos then mp=mp+1
bend
next a:print "{home}{home}{clr}";:gosub clear_status_bar:if fz<>0 then print"{rvon}Choose label with cursor keys + RETURN, escape with ESC{rvof} - filter='";fz$;"'";
if fz=0 then print"{rvon}{CTRL-O}No labels found{rvof} - filter='";fz$;"'"+format_off$;:redraw_status=1:return
l=status_line_y+1:window0,0,79,status_line_y:print"{clr}":for a=0 to fz:cursor 20*int(a/l),mod(a,l):print filtered_name$(a);:next a:if mp<0 then mp=0:return:else return


.show_labels
'-----------
mp = 0
bank 1
gosub clear_status_bar:print"{rvon}Collecting labels...";:if nz%=0 then fz$=""
for a=0 to 72:label_name$(a)="":label_lineno(a)=0:filtered_name$(a)="":filtered_lineno(a)=0:next a:mn=0:fz=0
for a=0 to lastline_y:t$=line_buff$(a):lb$="":if left$(t$,1)<>"." then 7490
label_name$(mn)=mid$(t$,1,38):label_lineno(mn)=a:mn=mn+1
next
gosub clear_status_bar:gosub filter_labels
if nz%=1 then begin
  if fz=1 then t$=chr$(13):else t$=chr$(27)
  nz%=0:goto slbl_skip
bend
nz%=0
do
  sx=20*int(mp/l): sy=mod(mp,l):cursor 0,10
  cursor sx,sy:print "{rvon}";filtered_name$(mp);"{rvof}";
  getkey t$
  cursor sx,sy:print filtered_name$(mp);
  if t$="{down}" then mp=mp+1
  if t$="{rght}" then mp=mp+l
 if t$="{up}" then mp=mp-1
 if t$="{left}" then mp=mp-l
 if (t$>="a" and t$<="z") or t$="{CBM-P}" then fz$=fz$+t$:gosub filter_labels
 if t$=chr$(20) and len(fz$)>0 then fz$=left$(fz$,len(fz$)-1):gosub filter_labels
 if mp>fz-1 then mp=fz-1
  if mp<0 then mp=0
loop until t$=chr$(13) or t$=chr$(27)

.slbl_skip
if t$=chr$(13) then last_xpos=x_pos:last_ypos=y_pos:last_yoffs=y_offs:x_pos=0:ly=filtered_lineno(mp):gosub safe_ly:else gosub redraw_screen
gosub build_status_bar ' redraw status bar
return


.save_filename_in_mailbox
'------------------------
gosub copy_line_buffer_to_attic_ram  ' save to attic
for i=1 to len(curr_file$):poke $4ff10+i-1,asc(mid$(curr_file$,i,1)):next i
poke $4ff10+i-1,0
poke $4ff00,asc("s"),asc("k")
poke $4ff07,peek($4ff07)or1
bank 128:return


.compile
'-------
ab=0
if curr_file$="" then return  ' now filename, no cigar
if changed=1 then begin
  gosub clear_status_bar: foreground hl
  print "{rvon}current file not saved - press any key";:getkey t$
  foreground fg: gosub build_status_bar: ab=1
bend
if ab=1 then return
print"{home}{home}{clr}"+chr$(27)+"l";
' gosub copy_line_buffer_to_attic_ram  ' save line buffer
print"{home}{home}{clr}{down}{down}edma 0,$d400,$8010000,$2001:new restore{down}{down}":print"run{home}"; ' load '11.parse' from attic cache
bank 128
poke 208,2      ' no of chars in keyboard buffer
poke 688,13,13  ' 2x return
end


.ml_helper
'---------
'--- m/l helper for vic-iv mode
 7950 bank128:restore 7970
 7960 for i=0 to 12:read a$:poke dec("1600")+i,dec(a$):next i
 7970 data 78,a2,47,a0,53,8e,2f,d0,8c,2f,d0,58,60
 7980 ml_v4=dec("1600")
 7990 return


.find_replace
'------------
' find and replace
gosub clear_status_bar:print "{rvon}find:"+chr$(27)+"t{clr}";
fs$="":rs$="":sd=1  ' find string, replace string, search dir
ra%=0  ' replace all
open 1,0,0:line input#1,fs$:close 1
if fr%=0 then8110
  print"{home}{home}";:gosub clear_status_bar:print"{rvon}replace:"+chr$(27)+"t{clr}";
  open 1,0,0:line input#1,rs$:close 1
print"{home}{home}";:gosub clear_status_bar
if fs$="" or fs$=" " then print"{rvon}find cancelled.";:redraw_status=1:return
s=y_pos

.fnr_retry
gosub clear_status_bar:print"searching...";
do while s>=0 and s<=lastline_y
  fp=instr(line_buff$(s),fs$)  ' found pos
  if fp=0 then8240
     ly=s:context_lines=8: gosub safe_ly
     bank 128
     ' mark found line
     for rv=0 to 79
       i=((y_pos-y_offs)*80)+rv
       poke 2048+i,peek(2048+i) xor 128
     next rv
     ' flash found string
     for rv=0 to len(fs$)-1
       i=((y_pos-y_offs)*80)+fp+rv-1
       poke $d800+i,peek($d800+i) or 16
     next rv
     if fr%=0 then gosub find_next : else gosub replace
  s=s+sd
loop
if fd$="q" then gosub build_status_bar: else gosub clear_status_bar:foreground hl:print"{rvon}end of found";:redraw_status=1
if fd$<>"q" and s>lastline_y then print " - search from start?{$a0}(y/n):";:get key fd$: if fd$="y" then s=0:goto fnr_retry:else gosub clear_status_bar
x_pos=0:cursor x_pos,y_pos-y_offs:print chr$(27)+"q";
foreground fg
a$=line_buff$(y_pos):gosub draw_text_line ' redraw current
return


.find_next
'---------
bank 128
gosub clear_status_bar:print "{rvon}find n)ext p)rev q)uit";
do:getkey fd$:loop until instr("npq",fd$)
if fd$="n" then sd=1
if fd$="p" then sd=-1
if fd$="q" then s=lastline_y+2
return


.replace
'-------
bank 128
gosub clear_status_bar:print "{rvon}r)eplace a)ll n)ext p)rev q)uit";
do:getkey fd$:loop until instr("ranpq",fd$)
if fd$="n" then sd=1
if fd$="p" then sd=-1
if fd$="q" then s=lastline_y+1
if fd$="r" then gosub replace_item
return


.replace_item
'------------
ts$=line_buff$(s)
ns$=left$(ts$,fp-1)+rs$+mid$(ts$,fp+len(fs$),255)
line_buff$(s)=ns$
cursor x_pos,y_pos-y_offs:print chr$(27)+"q";
a$=line_buff$(s):gosub draw_text_line
s=s-1
return


.error_handler
'-------------
if er=30 then resume
trap
bank 128:poke248,0:key on:palette restore
print chr$(27)+"q"+chr$(27)+"l{home}{home}{lgrn}";err$(er),el
end


.copy_line_buffer_to_attic_ram
'-----------------------------
cb=$8030000 : c=cb
wpoke c,lastline_y+1 : c=c+2  ' store no of lines
for a=0 to lastline_y
  p=pointer(line_buff$(a)):l=len(line_buff$(a)):b=$10000+wpeek(p+1)
  poke c,l : c=c+1
  if l<>0 then edma 0,l,b,c:c=c+l
next
return


.copy_attic_ram_to_line_buffer
'-----------------------------
cb=$8030000 : c=cb
lastline_y=wpeek(c):c=c+2
cl=0
do while cl<>lastline_y
  l=peek(c):line_buff$(cl)=left$(blank_line$,l):p=pointer(line_buff$(cl))
  b=$10000+wpeek(p+1):c=c+1
  if l<>0 then edma 0,l,c,b : c=c+l
  cl=cl+1
loop
return


.post_file_to_pc
'---------------
bload "b65support.bin":sys $1600
a=usr(0) ' set issue# to zero
l$="/file:"+curr_file$:gosub post_chunk  ' post filename
dopen#2,(curr_file$)
l$=""

.post_loop
get#2,c$
l$=l$+c$
if len(l$)=128 then gosub post_chunk  ' post chunk
print n$;
if not st and 64 then post_loop
dclose#2
if len(l$)<>0 then gosub post_chunk  ' post remaining chunk
gosub post_chunk  ' post empty chunk to declare we've finished
print "finished posting!"
return


.post_chunk
'----------
' *** post chunk ***
l$=chr$(len(l$)+1) + l$ ' prepend the length of string+1
a=usr("/"+l$)
print "posting chunk..."
l$=""
return


.next_word
'---------
' move to next word
do while mid$(curr_line$,x_pos+x_offs,1) <> " "
  gosub next_char
  if ef then exit
loop
do while mid$(curr_line$,x_pos+x_offs,1) = " "
  gosub next_char
  if ef then exit
loop
return


.next_char
'---------
' move to next char
ef=0:x_pos=x_pos+1
if x_pos>len(curr_line$) then begin
  if y_pos< lastline_y then x_pos=0:y_pos=y_pos+1:curr_line$=line_buff$(y_pos):if y_pos-y_offs>20 then gosub scroll_up
  ef=1
bend
return


.previous_word
'-------------
' move to previous word
gosub previous_char  ' prev char
do while mid$(curr_line$,x_pos+x_offs,1) = " "
  gosub previous_char
  if ef then exit
loop
do while mid$(curr_line$,x_pos+x_offs,1) <> " "
  gosub previous_char
  if ef then exit
loop
gosub previous_char  ' next char
return


.previous_char
'-------------
' move to prev char
ef=0:x_pos=x_pos-1
if x_pos<0 then begin
  if y_pos>0 then y_pos=y_pos-1:curr_line$=line_buff$(y_pos):x_pos=len(curr_line$)-1:if y_pos-y_offs<0then gosub scroll_down
  if x_pos<0 then x_pos=0
  ef=1
bend
if ef=1 and len(curr_line$) and y_pos>0 then goto previous_char
return


.delete_curr_char
'----------------
' delete current char
if x_pos<len(curr_line$) then begin
  changed=1
  curr_line$=left$(curr_line$,x_pos+x_offs-1)+mid$(curr_line$,x_pos+x_offs+1)
  line_buff$(y_pos)=curr_line$
  gosub redraw_line
bend
return


.redraw_line
'-----------
iv=0:gosub invert_if_marked
cursor 0,y_pos-y_offs:a$=curr_line$:gosub draw_text_line:if len(curr_line$)<=79 then print" ";
iv=0
return


.invert_if_marked
'----------------
iv=0:if mf=0 then return
if ms<me and ms<=y_pos and y_pos<=me then iv=1:return
if me<=ms and me<=y_pos and y_pos<=ms then iv=1
return


.mark_mode_handler
'-----------------
if t$="{down}" then begin
  me=y_pos+1:curr_line$=line_buff$(y_pos):gosub redraw_line : y_pos=y_pos+1
  if y_pos-y_offs>status_line_y-3 then gosub scroll_up :return:else begin
    curr_line$=line_buff$(y_pos):gosub redraw_line :return
  bend
bend
if t$="{up}" and y_pos>0 then begin
  me=y_pos-1:curr_line$=line_buff$(y_pos):gosub redraw_line :y_pos=y_pos-1
  if y_pos-y_offs<0 then gosub scroll_down :return:else begin
    curr_line$=line_buff$(y_pos):gosub redraw_line :return
  bend
bend
if t$="x" or t$="c" then begin
  if ms>me then iv=ms:ms=me:me=iv
  for iv=me to ms step -1
    clipboard$(iv-ms) = line_buff$(iv)
  next iv
  if t$="x" then begin
    for a=ms to lastline_y-(me-ms+1)
      line_buff$(a)=line_buff$(a+(me-ms+1))
    next a
    lastline_y=lastline_y-(me-ms+1):for a=lastline_y+1 to lastline_y+(me-ms+1):line_buff$(a)="":next a
    y_pos=ms:if y_pos-y_offs<0 then ly=ms:gosub safe_ly
  bend
  iv=0:mf=0:cs=me-ms+1:ms=0:me=0
bend
gosub  redraw_screen
return


.paste
'-----
for iv=cs-1 to 0 step -1
  ' insert a line at y_pos
  mf=y_pos ' temp
  changed=1:gosub insert_return
  y_pos=mf:mf=0
  line_buff$(y_pos)=clipboard$(iv)
next iv
gosub  redraw_screen
return


.enable_raw_fn_keys
'------------------
'--- set fn keys and make sure f11 doesn't generate paste
key on
fk$="{f1}{f2}{f3}{f4}{f5}{f6}{f7}{f8}{CTRL-P}{CTRL-U}{pur}{CTRL-W}{CTRL-Y}{CTRL-Z}"+chr$(132)
for r=1 to 15:key r, mid$(fk$,r,1):next r
return


.more_help
'---------
10550 if rwindow(1)=25 then print "{rvon}-- press any key for next page{rvof}";:getkeyt$:print"{home}{home}{clr}";:gosub clear_status_bar:else print "{down}{down}{down}";
10560 print"{rvon}alt + p{rvof} post to pc (m65postbox)"
10570 print"{rvon}esc, j{rvof} go to start of line"
10580 print"{rvon}esc, k{rvof} go to end of line"
10590 print"{rvon}esc, d{rvof} delete current line"
10600 print"{rvon}ctrl + w{rvof} move to next word"
10610 print"{rvon}ctrl + u{rvof} move to previous word"
10620 print"{rvon}shift + inst/del{rvof} delete current character"
10630 print"{rvon}ctrl + ={rvof} toggle compilation debugging on/off"
10640 print"{down}{rvon}esc, m{rvof} turn 'mark-mode' on"
10650 print"  - when 'mark-mode' is on, use cursor up/down to select lines"
10660 print"  - press '{rvon}x{rvof}' to cut"
10670 print"  - press '{rvon}c{rvof}' to copy"
10680 print"  - Then use {rvon}esc,p{rvof} to paste clipboard contents"
10690 return


.jump_to_prev_file
'-----------------
if changed=1 then begin
  gosub clear_status_bar:foreground hl
  print "{rvon}current file not saved - press any key";:get key t$
  gosub clear_status_bar:gosub build_status_bar:return
bend
temp=pv:nf$=pv$:gosub load_specified_file
ly=temp:gosub safe_ly
return


.navigate_to_label_under_cursor
'------------------------------
gosub get_term_under_cursor  ' get term under cursor
nz%=1 ' nav to label flag = true
gosub show_labels  ' gosub find label screen
return


.get_term_under_cursor
'---------------------
'--- get term under cursor into fz$
gosub get_start_of_label  ' start of label into sz
gosub get_end_of_label  ' end of label into ez
fz$ = mid$(curr_line$,sz+1,ez-sz+1)
return


.get_start_of_label
'------------------
'--- get start of label into sz
k = x_pos - 1 : zz$=" :" : sz = 0
do while k >= 0
  if instr(zz$,mid$(curr_line$,k+1,1)) then sz = k + 1 : exit
  k = k - 1
loop
return


.get_end_of_label
'----------------
'--- get end of label into ez
k = x_pos : zz$=" :" : ez = len(curr_line$)-1
do while k <= ez
  if instr(zz$,mid$(curr_line$,k+1,1)) then ez = k - 1 : exit
  k = k + 1
loop
return

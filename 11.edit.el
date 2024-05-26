    0 last_ypos=0
      last_yoffs=0
      last_xpos=0
      goto general_init_and_main_loop  ' rem buffer of past line location

.draw_text_line
    1 a$=mid$(a$,x_offs)
      b=len(a$)
      o=b>80
      print"{rvof}";
      if o then b=79
    2 p=pointer(a$)
      bank 0
      m=peek(p+1)+256*peek(p+2)
      c=m+b-1
      if b=0 then 6
    3 if iv=1 then print "{rvon}";
    4 bank 1
      for a = m to c
        za=peek(a)
        printtransl$(za);
        if iv=1 and za=34 then print "{rvon}";
    5 next
      bank 128
      if o then begin
        foreground hl
        printtab(79);"$";
        foreground fg
      bend
    6 return

  100 :
  110 rem --- general initializations ---

.general_init_and_main_loop
  130 trap error_handler
  135 palette restore
  140 poke 0,65             : rem m65 fast mode
  150 gosub ml_helper: rem install m/l
  160 pf$="11.edit"
  170 key 7,"scratch "+chr$(34)+pf$+chr$(34)+":dsave"+chr$(34)+pf$+chr$(34)+":dverify"+chr$(34)+pf$+chr$(34)
  180 if rwindow(2)<>80 then print chr$(27)+"x";:rem force 80cols mode
  190 gosub read_defaults_from_bank4
  200 background bg
      border bo
      foreground fg
      print"{home}{home}{clr}{swlc}"+chr$(27)+"m";
  210 gosub clear_status_bar
      print"{rvon} ee v0.5.0{rvof}";
  220 gosub enable_raw_fn_keys
  230 :
  240 max_lines=2000               : rem max number of lines
  242 esc_flag%=0                 : rem escape flag
  245 t$="                                                                               "
      blank_line$=t$+t$+t$:t$=""
  250 quote$=chr$(34)           : rem fast access to quote char
  260 format_off$=chr$(27)+chr$(27) : rem escape flash etc.
  270 ctrl_chars$="{home}{clr}{f2}{$84}{left}{up}{down}{rght}{f1}{f3}{f5}{f7}{f4}{f8}{CTRL-O}{CTRL-Z}{CTRL-P}{$83}{CTRL-W}{CTRL-U}{inst}{blu}"+chr$(22)+chr$(20)+chr$(148)+chr$(13) : rem ctrl chars
  280 horz_move_chars$="{left}{rght}"+chr$(20)     : rem horizontal movement characters
  290 file_changed_ind$(0)=" ":file_changed_ind$(1)="*" : rem for file changed indicator
  300 control_mode_ind$(0)="    ":        : rem for control char mode indicator
  310 control_mode_ind$(1)="{CTRL-O}ctrl"+format_off$
  320 disk_op_result$(1)="success: ":disk_op_result$(0)="error: " : rem disc operation result
  330 disk_op_result_clr(1)=sb:disk_op_result_clr(0)=hl                 : rem disc operation result colors
  340 default_file$="11.defaults"                 : rem defaults file name
  350 dim line_buff$(max_lines)           : rem line buffer
  355 dim clipboard$(100)        : rem clipboard of lines
  360 dim label_name$(72),label_lineno(72),filtered_name$(72),filtered_lineno(72)   : rem marker table
  370 ee$(0)="variable not declared"
  420 :
  430 rem --- build char translation table ---
  440 :
  450 dim transl$(255)           : rem char translation table
  460 for a=1 to 31:transl$(a)="{rvon}"+chr$(64+a)+"{rvof}":transl$(128+a)="{rvon}"+chr$(192+a)+"{rvof}":next a
  470 for a=32 to 127:transl$(a)=chr$(a):transl$(a+128)=chr$(a+128):next a
  480 transl$(34)=chr$(34)+format_off$
  490 transl$(160)="{CBM-+}" : rem make shift+space visible
  500 :
  510 :
  520 x_offs=1 : rem horizontal scroll index in current line
  530 bank 128
  540 poke 248,peek(248)or2^6    : rem disable screen editor line linking
  550 x_pos=0:y_pos=0:status_line_y=rwindow(1)-2 : rem cursor pos, status line position
  560 if curr_file$<>"" then begin
  565   if peek($4ff70)=55 and peek($4ff71)=77 then begin
  566     gosub copy_attic_ram_to_line_buffer
  567   bend:else begin
  570     gosub load_file: rem load file if set
  572     gosub copy_line_buffer_to_attic_ram: rem keep copy of file in attic ram
  575     poke $4ff70,55,77
  580     if disk_errno<>0 then curr_file$="" : rem delete current filename if file not there
  585   bend
  590 bend
  600 :
  610 rem uncomment this for output performance test
  620 rem ts=ti:for te=0 to 10:gosub 2080:next te:print"{down}{down}{down}"ti-ts:stop
  630 :
  640 gosub redraw_screen: rem redraw screen
  650 gosub build_status_bar: rem build status bar
 2000 :
 2010 rem **********************
 2020 rem ** main editor loop **
 2030 rem **********************
 2040 :
 2050 bank 128:sys ml_v4 : rem enable vic iv
 2060 if jump_line<>-1 then begin
 2070   ly=jump_line:context_lines=5:gosub safe_ly: rem bring line in context
 2080   if peek($4ff30)<>0 then begin
 2090     r=0:parser_error$="":do:p=peek($4ff30+r):parser_error$=parser_error$+chr$(p):r=r+1:loop while p<>0
 2110     gosub clear_status_bar:print "{rvon}";parser_error$;:redraw_status=1
 2115     poke $4ff30,0
 2120   bend
 2130   jump_line=-1:context_lines=0             : rem clear jump line
 2140 bend

 2150 do
.main_loop
 2160   if y_pos-y_offs<0 then y_offs=0:y_pos=0
 2170   if redraw_status<>1 then po$="      {left}{left}{left}{left}{left}{left}"+mid$(str$(x_pos),2)+":"+mid$(str$(y_pos),2):gosub display_status
 2180   curr_line$=line_buff$(y_pos)
 2185   if (y_pos-y_offs)>rwindow(1)-1 then y_pos=y_offs+rwindow(1)-1
 2190   cursor x_pos,y_pos-y_offs
 2200   foreground sb
        cursor on
        getkey t$
        cursor off
        foreground fg

 2201   if mf=1 then gosub mark_mode_handler : goto main_loop :rem mark-mode handling
 2210   if redraw_status=1 then gosub build_status_bar: bank 128:sys ml_v4: redraw_status=0
 2220   bank 128:alt_key = peek(54801) and 16 : rem check alt key
 2225   rem -- if t$=f11 (also ctrl+5/green for xemu?)
 2230   if t$="{pur}" or (alt_key and t$="c") then control_mode=abs(control_mode-1) : goto main_loop
 2240     ctrl_or_alt = (instr(ctrl_chars$,t$) and control_mode=0) or alt_key
 2250    if ctrl_or_alt then begin
 2260     if t$="g" then t$="{CTRL-P}" : rem workaround for challenged keyboards
 2270     if t$="s" then t$="{CTRL-Z}"
 2280     if t$=chr$(13) then changed=1: gosub insert_return : rem insert return
 2290     if instr(horz_move_chars$,t$)=0 and x_offs>1 then begin
 2300        x_offs=1:cursor 0,y_pos-y_offs:curr_line$=line_buff$(y_pos):a$=curr_line$:gosub draw_text_line
 2310        if t$=chr$(13) then cursor 0,y_pos-y_offs-1:a$=line_buff$(y_pos-1):gosub draw_text_line
 2320     bend
 2321     if t$="{CTRL-W}" then gosub next_word :rem ctrl-w = next word
 2322     if t$="{CTRL-U}" then gosub previous_word:rem ctrl-u = previous word
 2323     if t$="{inst}" then gosub delete_curr_char:rem shift-del = delete current char
 2324     if t$="{blu}" then poke $4ff07,peek($4ff07) xor 8:if peek($4ff07)and8 then play "t0o5sg"  ' toggle debug on/off
 2325     if t$="p" then gosub post_file_to_pc
 2326     if t$="z" then gosub navigate_to_label_under_cursor
 2330     if t$="f" then fr%=0 : gosub find_replace : rem find
 2340     if t$="r" then fr%=1 : gosub find_replace : rem find and replace
 2350     if t$="{f3}" then gosub save_file : rem save
 2360     if t$="{f4}" then sa=1:gosub save_file : rem save as
 2370     if t$="{f1}" then gosub load_file_prompt : rem load
 2380     if t$="{$83}" then gosub quit_ee : rem stop
 2390     if t$="{f2}" then gosub new_file : rem new
 2400     if t$="{f7}" then gosub show_labels : rem labels
 2410     if t$="{CTRL-P}" then gosub jump_to_line : rem goto line
 2415     if t$="{f8}" then gosub jump_to_prev_file : rem go to previous file (pv$)
 2420     if t$="{$84}" then gosub show_help : rem help
 2425     if t$="{CTRL-O}" then temp=y_pos:y_pos=last_ypos:last_ypos=temp: temp=y_offs:y_offs=last_yoffs:last_yoffs=temp: temp=x_pos:x_pos=last_xpos:last_xpos=temp:gosub  redraw_screen
 2430     if t$="{home}" then begin           : rem home/end
 2440       if alt_key=0 then ly=0:x_pos=0:gosub safe_ly: else ly=lastline_y:x_pos=0:gosub safe_ly
 2450     bend
 2460     if t$="{rght}" then begin : rem -------- cursor right
 2470       if alt_key then begin
 2480          if 79+x_offs<len(curr_line$) then x_offs=x_offs+1:cursor0,y_pos-y_offs:a$=curr_line$:gosub draw_text_line
 2490       bend : else x_pos=x_pos+1
 2500       if x_pos>79 then begin
 2510         x_pos=79:if x_pos+x_offs<len(curr_line$) then x_offs=x_offs+1
 2520         cursor 0,y_pos-y_offs:a$=curr_line$:gosub draw_text_line
 2530       bend
 2540     bend
 2550     if t$="{left}" then begin
 2560       if x_pos>0 then x_pos=x_pos-1 : else if y_pos>0 then y_pos=y_pos-1:x_pos=len(line_buff$(y_pos)):if x_pos>79 then x_pos=79
 2570     bend
 2580     if t$="{down}" then begin
 2590       if alt_key then ly=y_pos+status_line_y:gosub safe_ly:else y_pos=y_pos+1
 2600       if y_pos-y_offs>status_line_y-3 then gosub scroll_up : if y_pos-y_offs>status_line_y then y_pos=y_pos-1
 2610     bend
 2620     if t$="{up}" then begin
 2630       if alt_key then ly=y_pos-status_line_y:gosub safe_ly:else y_pos=y_pos-1
 2640       if y_pos-y_offs<0 then gosub scroll_down : if y_pos-y_offs<0 then y_pos=y_pos+1
 2650     bend
 2660     if t$="{f5}" then gosub compile : rem --- f5 - compile
 2670     if t$=chr$(20) then begin:rem delete
 2680       changed=1
 2690       if x_pos=0 or alt_key then gosub merge_lines :else begin : rem merge with last line
 2700       curr_line$=left$(curr_line$,x_pos+x_offs-2)+mid$(curr_line$,x_pos+x_offs)
 2710       line_buff$(y_pos)=curr_line$
 2720       cursor 0,y_pos-y_offs:a$=curr_line$:gosub draw_text_line:if len(curr_line$)<=79 then print " ";
 2730       if x_pos>0 then x_pos=x_pos-1
 2740     bend
 2750   bend
 2760   if t$="{CTRL-Z}" then begin : rem --- f14 - save defaults
 2770     gosub clear_status_bar:print"{rvon}saving defaults...{rvof}";
 2780     scratch (default_file$):bsave (default_file$),b(4),p(dec("ff00")) to p(dec("ff80"))
 2785     disk_errno=ds : disk_error$=ds$
 2790     foreground disk_op_result_clr(abs(disk_errno=0))
 2800     gosub clear_status_bar:print "{rvon}";disk_op_result$(abs(disk_errno=0));disk_error$;" ";:foreground sb
 2810     redraw_status=1
 2820   bend
 2830 bend:else begin:if t$=chr$(27) then begin: rem -- check escape flag
 2831   t$="":if esc_flag%=0 then esc_flag%=1:else esc_flag%=0
 2832 bend:else begin:if esc_flag%=1 then begin : rem -- check escape characters
 2833   if t$="k" then x_pos=len(curr_line$):if x_pos>78 then x_offs=x_pos-78:x_pos=78:gosub redraw_line
 2834   if t$="j" then x_pos=0:if x_offs>1 then x_offs=1:gosub redraw_line
 2835   if t$="d" then alt_key=1:gosub merge_lines :changed=1
 2836   if t$="m" then mf=1:ms=y_pos:me=y_pos:gosub redraw_line
 2837   if t$="p" then gosub paste
 2838   esc_flag%=0
 2839 bend:else begin : rem -- insert char
 2840     changed=1
 2850     if x_pos=79 then x_offs=x_offs+1
 2860     do while len(curr_line$)<x_pos:curr_line$=curr_line$+" ":loop: rem fill up spaces if needed
 2870     curr_line$=left$(curr_line$,x_pos+x_offs-1)+t$+mid$(curr_line$,x_pos+x_offs)
 2880     line_buff$(y_pos)=curr_line$
 2890     cursor 0,y_pos-y_offs:a$=curr_line$:gosub draw_text_line
 2900     x_pos=x_pos+1 : if x_pos>79 then x_pos=79
 2910   bend:rem endif esc_flag%=1
 2912   bend:rem endif t$=chr$(27)
 2914   bend:rem endif ctrl_or_alt (ctrl-char)
 2920 loop until qu=1
 2930 stop
 2940 :
 5000 rem ==================== subroutines =====================

.load_file
 5010 rem --- read in f$
 5020 open 1,1,5,curr_file$+",s,r":disk_errno=ds:disk_error$=ds$
 5030 if disk_errno<>0 then curr_file$=of$:close 1:return
 5040 for a = 0 to max_lines:line_buff$(a)="":next a:lc=0
 5050 lastline_y=0:y_offs=0:y_pos=0:x_pos=0
 5060 do
 5070   l$="":line input#1,l$
 5080   line_buff$(lastline_y)=l$
 5090   lastline_y=lastline_y+1
 5100 loop while st=0
 5110 lastline_y=lastline_y-1:changed=0
 5120 close 1
 5130 return
 5140 :

.redraw_screen
 5150 rem ---  redraw screen
 5160 foreground fg : print "{rvof}";
 5170 if y_offs<0 then y_offs=0
 5180 for i=0 to status_line_y
 5190   cursor 0,i
 5200   a$=line_buff$(y_offs+i)
 5210   print chr$(27)+"q";:zy=y_pos:y_pos=y_offs+i:gosub invert_if_marked :y_pos=zy:gosub draw_text_line
 5220 next i
 5230 return
 5240 :

.scroll_up
 5250 rem --- scroll up
 5260 if y_offs+status_line_y>lastline_y-1 then return
 5270 y_offs=y_offs+1 : rem current top
 5280 s$=line_buff$(y_offs+status_line_y)
 5290 cursor 0,0:print chr$(27)+"v";
 5300 cursor 0,status_line_y:a$=s$:gosub draw_text_line
 5310 return
 5320 :

.scroll_down
 5330 rem --- scroll down
 5340 if y_offs<=0 then return
 5350 y_offs=y_offs-1
 5360 s$=line_buff$(y_offs)
 5370 cursor0,0:print chr$(27)+"w";:a$=s$:gosub draw_text_line
 5380 return
 5390 :

.display_status
 5400 rem --- display status in st$
 5410 foreground sb
 5420 print"{home}{home}";:cursor 20,status_line_y+1:print"{rvon}"+po$;
 5430 cursor 75,status_line_y+1:print control_mode_ind$(control_mode);
 5440 cursor 0,status_line_y+1:print"{rvon}"+file_changed_ind$(changed)+"{rvof}";
 5450 window 0,0,79,status_line_y
 5460 foreground fg
 5470 return
 5480 :

.clear_status_bar
 5490 if rwindow(1)=25 then cb=2048+(24*80) : fb=$ff80000 + (24*80)
 5492 if rwindow(1)=50 then cb=$40800+(49*80) : fb=$ff80000 + (49*80)
 5495 edma 3,80,160,cb       : rem  --- clear status bar
 5500 edma 3,80,sb,fb        : rem  set status bar colour
 5510 print"{home}{home}";:cursor 0,status_line_y+1
 5520 foreground sb
 5530 return

.build_status_bar
 5540 rem --- clear status
 5550 gosub clear_status_bar
 5580 print"{home}{home}";:cursor 18,status_line_y+1:print"{rvon}{SHIFT--}       {SHIFT--} {rvof}f1{rvon}load {rvof}f3{rvon}save {rvof}f5{rvon}compile {rvof}f7{rvon}lbls {rvof}f9{rvon}go {rvof}f11{rvon}cmode {SHIFT--}";
 5590 cursor 2,status_line_y+1:if curr_file$<>"" then print curr_file$;:else print "<untitled>";
 5600 window 0,0,79,status_line_y
 5610 foreground fg
 5620 return
 5630 :

.insert_return
 5640 rem --- insert return
 5650 y_pos=y_pos+1
 5660 lastline_y=lastline_y+1
 5670 if y_pos>lastline_y then lastline_y=y_pos
 5680 fora=lastline_y+1to y_pos+1 step-1:line_buff$(a)=line_buff$(a-1):next a:line_buff$(y_pos)=""
 5690 line_buff$(y_pos)=mid$(line_buff$(y_pos-1),x_pos+x_offs)
 5700 line_buff$(y_pos-1)=left$(line_buff$(y_pos-1),x_pos+x_offs-1)
 5710 x_pos=0:sc=1:pl$=line_buff$(y_pos-1)
 5720 do while mid$(pl$,sc,1)=" " : rem --- autoindent
 5730   if line_buff$(y_pos)<>"" then line_buff$(y_pos)=" "+line_buff$(y_pos)
 5740   sc=sc+1:x_pos=x_pos+1
 5750 loop
 5760 if y_pos-y_offs>status_line_y then gosub scroll_up : rem scroll up
 5770 cursor x_pos,y_pos-y_offs:print chr$(27)+"i";
 5780 a$=line_buff$(y_pos):gosub draw_text_line
 5790 cursor x_pos-sc+1,y_pos-y_offs-1:print chr$(27)+"q";
 5800 a$=line_buff$(y_pos-1):gosub draw_text_line
 5810 return

.merge_lines
 5820 :
 5830 rem --- merge lines
 5840 if alt_key=0 and y_pos<1 then return
 5850 if alt_key then sx=x_pos:x_pos=0:y_pos=y_pos-1:goto mg_skip
 5860 y_pos=y_pos-1:x_pos=len(line_buff$(y_pos))
 5870 line_buff$(y_pos)=line_buff$(y_pos)+line_buff$(y_pos+1)
.mg_skip
 5880 for a=y_pos+1 to lastline_y-1:line_buff$(a)=line_buff$(a+1):next a
 5890 if y_pos<=lastline_y then line_buff$(lastline_y)=""
 5900 if lastline_y>0 then lastline_y=lastline_y-1
 5910 :
 5920 if alt_key=0 then cursor 0,y_pos-y_offs:a$=line_buff$(y_pos):gosub draw_text_line
 5930 cursor 0,y_pos-y_offs+1:printchr$(27)+"d";
 5940 cursor 0,status_line_y:a$=line_buff$(y_offs+status_line_y):gosub draw_text_line
 5950 if y_offs+status_line_y>lastline_y-1 then cursor 0,status_line_y:print chr$(27)+"q";
 5960 if alt_key then x_pos=sx:y_pos=y_pos+1
 5970 return
 5980 :

.save_file
 5990 rem --- save file
 6000 if bu and curr_file$<>"" and sa=0 then begin
 6010   gosub clear_status_bar:print"{rvon}backing up old file...";
 6020   b$=left$(curr_file$,12)+".bak"
 6030   delete (b$)
 6040   copy (curr_file$) to (b$)
 6050 bend
 6060 if curr_file$="" or sa=1 then sa=0 : gosub save_file_prompt : rem save as
 6070 if curr_file$="" then gosub build_status_bar: return
 6080 gosub clear_status_bar:print"{rvon}saving...";
 6090 delete (curr_file$)
 6100 i = max_lines:do while line_buff$(i)="":i=i-1:loop
 6110 lastline_y=i
 6120 open 1,1,3,curr_file$+",s,w":disk_errno=ds:disk_error$=ds$
 6130 for a=0 to lastline_y
 6140   print#1,line_buff$(a)
 6150 next a
 6155 if ds<>0 then disk_errno=ds
 6160 close 1
 6170 gosub clear_status_bar
 6180 if disk_errno=0 then changed=0
 6190 foreground disk_op_result_clr(abs(disk_errno=0))
 6200 print disk_op_result$(abs(disk_errno=0));disk_error$;" ";:foreground sb
 6210 gosub save_filename_in_mailbox : rem save filename in mailbox ram
 6220 redraw_status=1
 6230 return
 6240 :
 6250 :

.load_file_prompt
 6260 rem --- load file
 6270 gosub clear_status_bar
 6280 of$=curr_file$
 6290 if changed=1 then begin
 6300   print "{rvon}current file "+curr_file$+" not saved. really proceed? (y/n) {CTRL-O}{CBM-+}"+format_off$;
 6310   do:getkey t$:loop until instr("yn",t$)
 6320   gosub clear_status_bar
 6330   if t$="n" then gosub build_status_bar:return
 6340 bend
.lf_retry
 6350 gosub clear_status_bar:print "{rvon}load file name ($ for directory, return to cancel) "+chr$(27)+"t{clr}";
 6360 nf$=""
 6370 line input "";nf$
 6380 if len(nf$)>0 and left$(nf$,1)="$" then begin
 6381   window0,0,79,status_line_y : print "{clr}"; : foreground fg
 6382   if len(nf$) = 1 then dir : else begin
 6383     dir (mid$(nf$,2))
 6384   bend
 6385   goto lf_retry
 6386 bend
.load_specified_file
 6390 print"{home}{home}";:gosub clear_status_bar
 6400 if nf$="" then gosub build_status_bar:gosub redraw_screen:return
 6410 pv$=curr_file$:pv=y_pos:curr_file$=nf$:print "{rvon}loading "+curr_file$+"...";
 6420 gosub load_file
 6430 print"{home}{home}{clr}"+format_off$;
 6440 gosub redraw_screen: rem redraw screen
 6450 if disk_errno=0 then gosub save_filename_in_mailbox : rem persist filename
 6460 gosub clear_status_bar : foreground disk_op_result_clr(abs(disk_errno=0))
 6470 print "{rvon}";curr_file$;" - ";disk_op_result$(abs(disk_errno=0));disk_error$;" ";:foreground sb
 6480 if disk_errno=0 then print"{rvon}; read ";lastline_y; "lines.";
 6490 redraw_status=1
 6500 return
 6510 :

.save_file_prompt
 6520 rem --- query save file name
 6530 gosub clear_status_bar:print "{rvon}save file name (return to cancel) "+chr$(27)+"t{clr}";
 6540 nf$=""
 6550 line input "";nf$
 6560 print"{home}{home}";:gosub clear_status_bar
 6570 curr_file$=nf$
 6580 return
 6590 :

.safe_ly
 6600 rem --- ensure ly is at sane position end
 6602 rem     make line ly the current line
 6605 rem if ly>lastline_y-status_line_y then ly=lastline_y-status_line_y
 6610 y_offs=ly :y_pos=ly
 6620 if y_offs>context_lines then y_offs=y_offs-context_lines
 6630 goto sl_skip
 6640 if y_offs<0 then y_offs=0
 6650 if y_pos<0 then y_pos=0
 6660 if y_pos>lastline_y then y_pos=lastline_y : y_offs=y_pos-status_line_y+3
 6670 if y_offs>context_lines then y_offs=y_offs-context_lines : rem context lines
 6680 if y_offs<0 then y_offs=0
.sl_skip
 6690 gosub redraw_screen
 6700 return

.jump_to_line
 6710 rem --- query line no
 6720 gosub clear_status_bar:print "{rvon}jump to line # (return to cancel) "+chr$(27)+"t{clr}";
 6730 nn$="":line input nn$
 6740 nn=val(nn$):print"{home}{home}";:gosub clear_status_bar
 6750 if nn>lastline_y then return
 6760 ly=nn:context_lines=8:gosub safe_ly
 6770 gosub build_status_bar
 6780 return

.show_help
 6790 rem --- help
 6800 print"{home}{home}{clr}-- cursor movement --",,"-- editor functions --{down}"
 6810 print"{rvon}cursor keys  {rvof} move the cursor"
 6820 print"{rvon}alt + up     {rvof} scroll up 1 page"
 6830 print"{rvon}alt + down   {rvof} scroll down 1 page"
 6840 print"{rvon}alt + left {$a0} {rvof} scroll left 1 char(*)"
 6850 print"{rvon}alt + right  {rvof} scroll right 1 char(*)"
 6860 print"{down}{rvon}shift + clr  {rvof} go to top of file"
 6870 print"{rvon}alt + clr    {rvof} go to end of file"
 6880 print"{down}{rvon}alt + delete {rvof} delete current line"
 6890 print"{down}{rvon}alt + return {rvof} to start of next line"
 6892 print"{down}{rvon}alt + f      {rvof} find"
 6894 print"{rvon}alt + r      {rvof} find & replace"
 6896 print"{rvon}alt + z      {rvof} go to label under cursor"
 6900 print"{down}*) only in lines with >80 characters"
 6901 print"{down}{rvon}#{rvof}) xemu-equivalent shortcut key"
 6910 print"{home}{down}{down}",,,,"{rvon} f1 {rvof} load file"
 6920 print,,,,"{rvon} f2 {rvof} new file"
 6930 print,,,,"{rvon} f3 {rvof} save file"
 6940 print,,,,"{rvon} f4 {rvof} save file as"
 6950 print,,,,"{down}{rvon} f5 {rvof} compile + run"
 6960 print,,,,"{rvon} f6 {rvof} just compile"
 6970 print,,,,"{down}{rvon} f7 {rvof} go to label"
 6975 print,,,,"{rvon} f8 {rvof} go to previous file"
 6980 print,,,,"{rvon} f9 {rvof} go to line nr     ({rvon}#{rvof})ctrl + p"
 6982 print,,,,"{rvon}ctrl-o{rvof} jump to loc prior to f7"
 6990 print,,,,"{down}{rvon}f11 {rvof} toggle ctrl mode  ({rvon}#{rvof})ctrl + 5"
 7000 print,,,,"{down}{rvon}f14 {rvof} update defaults   ({rvon}#{rvof})ctrl + z"
 7010 print,,,,"{down}{rvon}help{rvof} this screen       ({rvon}#{rvof})pageup"
 7015 gosub more_help : rem more help
 7020 gosub clear_status_bar :print"{rvon}ee cheat sheet -- press any key to return to editor{rvof}";
 7030 getkey t$:gosub redraw_screen:gosub build_status_bar:return

.read_defaults_from_bank4
 7040 rem --- set filename from mailbox ram
 7050 bank 4:t$=chr$(peek(dec("ff00")))+chr$(peek(dec("ff01")))
 7060 if t$<>"sk" then bo=6:fg=5:bg=0:sb=14:hl=2:jump_line=-1:return
 7070 jump_line=-1 : rem jump line
 7080 al=peek(dec("ff07"))and1 : rem autoload flag?
 7090 if al=0 then goto skip_autoload
 7100 tf$="":i=dec("ff10"):do while peek(i)<>0
 7110 tf$=tf$+chr$(peek(i)):i=i+1:loop
 7120 curr_file$=tf$
 7130 if peek(dec("ff07"))and2 then begin : rem autojump flag
 7140 poke dec("ff07"),peek(dec("ff07"))and(255 xor 2)
 7150 jump_line=peek(dec("ff09"))+256*peek(dec("ff0a")):ee=peek(dec("ff08"))
 7160 bend
.skip_autoload
 7170 bo=peek(dec("ff02")):bg=peek(dec("ff03")):fg=peek(dec("ff04"))
 7180 hl=peek(dec("ff05")):sb=peek(dec("ff06"))
 7190 bu=peek(dec("ff07"))and4 : rem backup flag
 7195 sm=peek($4ff07)and8:if sm then print chr$(27)+"5";:else print chr$(27)+"8"
 7200 bank 128
 7210 return
 7220 :

.new_file
 7230 rem ------ new
 7240 if ch then begin
 7250 :  gosub clear_status_bar:print"{rvon}Current file was modified! Really start over (y/n)? {CTRL-O}{CBM-+}";
 7260 :  do:getkey t$:loop until t$="y" or t$="n"
 7270 :  if t$="n" then gosub build_status_bar:    return
 7280 bend
 7290 for a = 0 to max_lines:line_buff$(a)="":next a:lc=0
 7300 curr_file$=""
 7310 print"{home}{home}{clr}":gosub redraw_screen
 7320 gosub build_status_bar
 7330 changed=0:x_pos=0:y_pos=0:y_offs=0
 7340 gosub clear_status_bar:print"{rvon}** New file **{rvof}";:redraw_status=1
 7350 return

.quit_ee
 7360 rem ------ new
 7370 gosub clear_status_bar:print"{rvon}really quit ee? {CTRL-O}{CBM-+}"+chr$(27)+"o";
 7380 do:getkey t$:loop until t$="y" or t$="n"
 7390 if t$="n" then gosub build_status_bar:return
 7400 bank 128:poke248,0:key on:palette restore
 7410 print"{home}{home}{clr}tschuessn...!"
 7420 end

.filter_labels
 7421 rem --- filter labels based on fz$
 7422 for a=0 to 72:filtered_name$(a)="":filtered_lineno(a)=0:next a:fz=0:mp=-1
 7423 for a=0 to mn-1:if (len(fz$)>0 and instr(label_name$(a),fz$)>0)or len(fz$)=0 then begin
 7424   filtered_name$(fz)=label_name$(a):filtered_lineno(fz)=label_lineno(a):fz=fz+1
 7425   if label_lineno(a) <= y_pos then mp=mp+1
 7426 bend
 7427 next a:print "{home}{home}{clr}";:gosub clear_status_bar:if fz<>0 then print"{rvon}Choose label with cursor keys + RETURN, escape with ESC{rvof} - filter='";fz$;"'";
 7428 if fz=0 then print"{rvon}{CTRL-O}No labels found{rvof} - filter='";fz$;"'"+format_off$;:redraw_status=1:return
 7429 l=status_line_y+1:window0,0,79,status_line_y:print"{clr}":for a=0 to fz:cursor 20*int(a/l),mod(a,l):print filtered_name$(a);:next a:if mp<0 then mp=0:return:else return

.show_labels
 7430 rem --- labels
 7435 mp = 0
 7440 bank 1
 7450 gosub clear_status_bar:print"{rvon}Collecting labels...";:if nz%=0 then fz$=""
 7460 for a=0 to 72:label_name$(a)="":label_lineno(a)=0:filtered_name$(a)="":filtered_lineno(a)=0:next a:mn=0:fz=0
 7470 for a=0 to lastline_y:t$=line_buff$(a):lb$="":if left$(t$,1)<>"." then 7490
 7480 label_name$(mn)=mid$(t$,1,38):label_lineno(mn)=a:mn=mn+1
 7490 next
 7500 gosub clear_status_bar:gosub filter_labels
 7510 if nz%=1 then begin
 7512   if fz=1 then t$=chr$(13):else t$=chr$(27)
 7514   nz%=0:goto slbl_skip
 7516 bend
 7520 nz%=0
 7570 do
 7580   sx=20*int(mp/l): sy=mod(mp,l):cursor 0,10
 7590   cursor sx,sy:print "{rvon}";filtered_name$(mp);"{rvof}";
 7600   getkey t$
 7610   cursor sx,sy:print filtered_name$(mp);
 7620   if t$="{down}" then mp=mp+1
 7630   if t$="{rght}" then mp=mp+l
 7640  if t$="{up}" then mp=mp-1
 7650  if t$="{left}" then mp=mp-l
 7655  if (t$>="a" and t$<="z") or t$="{CBM-P}" then fz$=fz$+t$:gosub filter_labels
 7656  if t$=chr$(20) and len(fz$)>0 then fz$=left$(fz$,len(fz$)-1):gosub filter_labels
 7660  if mp>fz-1 then mp=fz-1
 7670   if mp<0 then mp=0
 7680 loop until t$=chr$(13) or t$=chr$(27)
.slbl_skip
 7690 if t$=chr$(13) then last_xpos=x_pos:last_ypos=y_pos:last_yoffs=y_offs:x_pos=0:ly=filtered_lineno(mp):gosub safe_ly:else gosub redraw_screen
 7700 gosub build_status_bar: rem redraw status bar
 7710 return

.save_filename_in_mailbox
 7720 rem --- save filename in mailbox ram
 7730 gosub copy_line_buffer_to_attic_ram : rem save to attic
 7740 for i=1 to len(curr_file$):poke $4ff10+i-1,asc(mid$(curr_file$,i,1)):next i
 7750 poke $4ff10+i-1,0
 7760 poke $4ff00,asc("s"),asc("k")
 7770 poke $4ff07,peek($4ff07)or1
 7780 bank128:return

.compile
 7790 rem --- compile
 7800 ab=0
 7810 if curr_file$="" then return : rem now filename, no cigar
 7820 if changed=1 then begin
 7830   gosub clear_status_bar: foreground hl
 7840   print "{rvon}current file not saved - press any key";:getkey t$
 7850   foreground fg: gosub build_status_bar: ab=1
 7860 bend
 7870 if ab=1 then return
 7880 print"{home}{home}{clr}"+chr$(27)+"l";
 7885 rem gosub copy_line_buffer_to_attic_ram : rem save line buffer
 7890 print"{home}{home}{clr}{down}{down}edma 0,$d400,$8010000,$2001:new restore{down}{down}":print"run{home}";:rem load '11.parse' from attic cache
 7900 bank 128
 7910 poke 208,2     : rem no of chars in keyboard buffer
 7920 poke 688,13,13 : rem 2x return
 7930 end
 7940 rem --- m/l helper for vic-iv mode

.ml_helper
 7950 bank128:restore 7970
 7960 for i=0 to 12:read a$:poke dec("1600")+i,dec(a$):next i
 7970 data 78,a2,47,a0,53,8e,2f,d0,8c,2f,d0,58,60
 7980 ml_v4=dec("1600")
 7990 return

.find_replace
 8000 rem ================
 8010 rem find and replace
 8020 rem ================
 8030 :
 8040 gosub clear_status_bar:print "{rvon}find:"+chr$(27)+"t{clr}";
 8050 fs$="":rs$="":sd=1 : rem find string, replace string, search dir
 8060 ra%=0 : rem replace all
 8070 open 1,0,0:line input#1,fs$:close 1
 8080 if fr%=0 then8110
 8090   print"{home}{home}";:gosub clear_status_bar:print"{rvon}replace:"+chr$(27)+"t{clr}";
 8100   open 1,0,0:line input#1,rs$:close 1
 8110 print"{home}{home}";:gosub clear_status_bar
 8120 if fs$="" or fs$=" " then print"{rvon}find cancelled.";:redraw_status=1:return
 8130 s=y_pos
.fnr_retry
 8140 gosub clear_status_bar:print"searching...";
 8150 do while s>=0 and s<=lastline_y
 8160   fp=instr(line_buff$(s),fs$) : rem found pos
 8170   if fp=0 then8240
 8180      ly=s:context_lines=8: gosub safe_ly
 8190      bank 128
 8195      rem mark found line
 8200      for rv=0 to 79
 8205        i=((y_pos-y_offs)*80)+rv
 8210        poke 2048+i,peek(2048+i) xor 128
 8220      next rv
 8222      rem flash found string
 8225      for rv=0 to len(fs$)-1
 8226        i=((y_pos-y_offs)*80)+fp+rv-1
 8227        poke $d800+i,peek($d800+i) or 16
 8228      next rv
 8230      if fr%=0 then gosub find_next : else gosub replace
 8240   s=s+sd
 8250 loop
 8260 if fd$="q" then gosub build_status_bar: else gosub clear_status_bar:foreground hl:print"{rvon}end of found";:redraw_status=1
 8265 if fd$<>"q" and s>lastline_y then print " - search from start?{$a0}(y/n):";:get key fd$: if fd$="y" then s=0:goto fnr_retry:else gosub clear_status_bar
 8270 x_pos=0:cursor x_pos,y_pos-y_offs:print chr$(27)+"q";
 8280 foreground fg
 8290 a$=line_buff$(y_pos):gosub draw_text_line: rem redraw current
 8300 return

.find_next
 8310 rem --- find next
 8320 bank 128
 8330 gosub clear_status_bar:print "{rvon}find n)ext p)rev q)uit";
 8340 do:getkey fd$:loop until instr("npq",fd$)
 8350 if fd$="n" then sd=1
 8360 if fd$="p" then sd=-1
 8370 if fd$="q" then s=lastline_y+2
 8380 return

.replace
 8390 rem --- replace
 8400 bank 128
 8410 gosub clear_status_bar:print "{rvon}r)eplace a)ll n)ext p)rev q)uit";
 8420 do:getkey fd$:loop until instr("ranpq",fd$)
 8430 if fd$="n" then sd=1
 8440 if fd$="p" then sd=-1
 8450 if fd$="q" then s=lastline_y+1
 8460 if fd$="r" then gosub replace_item
 8470 return

.replace_item
 8480 ts$=line_buff$(s)
 8490 ns$=left$(ts$,fp-1)+rs$+mid$(ts$,fp+len(fs$),255)
 8500 line_buff$(s)=ns$
 8510 cursor x_pos,y_pos-y_offs:print chr$(27)+"q";
 8520 a$=line_buff$(s):gosub draw_text_line
 8530 s=s-1
 8540 return
 8550 return
 8560 rem --- error handler

.error_handler
 8570 if er=30 then resume
 8580 trap
 8590 bank 128:poke248,0:key on:palette restore
 8600 print chr$(27)+"q"+chr$(27)+"l{home}{home}{lgrn}";err$(er),el
 8610 end

.copy_line_buffer_to_attic_ram
 9000 rem --- copy line buffer to attic ram
 9020 cb=$8030000 : c=cb
 9025 wpoke c,lastline_y+1 : c=c+2 : rem store no of lines
 9030 for a=0 to lastline_y
 9040   p=pointer(line_buff$(a)):l=len(line_buff$(a)):b=$10000+wpeek(p+1)
 9050   poke c,l : c=c+1
 9060   if l<>0 then edma 0,l,b,c:c=c+l
 9070 next
 9080 return

.copy_attic_ram_to_line_buffer
 9100 rem --- copy attic ram to line buffer
 9110 cb=$8030000 : c=cb
 9120 lastline_y=wpeek(c):c=c+2
 9130 cl=0
 9140 do while cl<>lastline_y
 9150   l=peek(c):line_buff$(cl)=left$(blank_line$,l):p=pointer(line_buff$(cl))
 9155   b=$10000+wpeek(p+1):c=c+1
 9165   if l<>0 then edma 0,l,c,b : c=c+l
 9180   cl=cl+1
 9190 loop
 9200 return

.post_file_to_pc
 9300 bload "b65support.bin":sys $1600
 9310 a=usr(0):rem set issue# to zero
 9320 l$="/file:"+curr_file$:gosub post_chunk :rem post filename
 9330 dopen#2,(curr_file$)
 9340 l$=""
.post_loop
 9350 get#2,c$
 9360 l$=l$+c$
 9370 if len(l$)=128 then gosub post_chunk :rem post chunk
 9380 print n$;
 9390 if not st and 64 then post_loop
 9400 dclose#2
 9410 if len(l$)<>0 then gosub post_chunk :rem post remaining chunk
 9420 gosub post_chunk :rem post empty chunk to declare we've finished
 9430 print "finished posting!"
 9440 return

.post_chunk
 9450 rem *** post chunk ***
 9460 l$=chr$(len(l$)+1) + l$:rem prepend the length of string+1
 9470 a=usr("/"+l$)
 9480 print "posting chunk..."
 9490 l$=""
 9500 return

.next_word
 9505 rem move to next word
 9510 do while mid$(curr_line$,x_pos+x_offs,1) <> " "
 9520   gosub next_char
 9530   if ef then exit
 9540 loop
 9550 do while mid$(curr_line$,x_pos+x_offs,1) = " "
 9560   gosub next_char
 9570   if ef then exit
 9580 loop
 9590 return

.next_char
 9600 rem move to next char
 9610 ef=0:x_pos=x_pos+1
 9620 if x_pos>len(curr_line$) then begin
 9630   if y_pos< lastline_y then x_pos=0:y_pos=y_pos+1:curr_line$=line_buff$(y_pos):if y_pos-y_offs>20 then gosub scroll_up
 9635   ef=1
 9640 bend
 9650 return

.previous_word
 9700 rem move to previous word
 9705 gosub previous_char :rem prev char
 9710 do while mid$(curr_line$,x_pos+x_offs,1) = " "
 9720   gosub previous_char
 9730   if ef then exit
 9740 loop
 9750 do while mid$(curr_line$,x_pos+x_offs,1) <> " "
 9760   gosub previous_char
 9770   if ef then exit
 9780 loop
 9785 gosub previous_char :rem next char
 9790 return

.previous_char
 9800 rem move to prev char
 9810 ef=0:x_pos=x_pos-1
 9820 if x_pos<0 then begin
 9830   if y_pos>0 then y_pos=y_pos-1:curr_line$=line_buff$(y_pos):x_pos=len(curr_line$)-1:if y_pos-y_offs<0then gosub scroll_down
 9835   if x_pos<0 then x_pos=0
 9840   ef=1
 9850 bend
 9855 if ef=1 and len(curr_line$) and y_pos>0 then goto previous_char
 9860 return

.delete_curr_char
 9900 rem delete current char
 9910 if x_pos<len(curr_line$) then begin
 9920   changed=1
 9930   curr_line$=left$(curr_line$,x_pos+x_offs-1)+mid$(curr_line$,x_pos+x_offs+1)
 9940   line_buff$(y_pos)=curr_line$
 9945   gosub redraw_line
 9950 bend
 9960 return

.redraw_line
 9970 rem *** redraw line ***
 9975 iv=0:gosub invert_if_marked
 9980 cursor 0,y_pos-y_offs:a$=curr_line$:gosub draw_text_line:if len(curr_line$)<=79 then print" ";
 9985 iv=0
 9990 return

.invert_if_marked
10000 rem *** invert if marked line
10010 iv=0:if mf=0 then return
10020 if ms<me and ms<=y_pos and y_pos<=me then iv=1:return
10030 if me<=ms and me<=y_pos and y_pos<=ms then iv=1
10040 return

.mark_mode_handler
10050 rem *** mark-mode handling ***
10060 if t$="{down}" then begin
10061   me=y_pos+1:curr_line$=line_buff$(y_pos):gosub redraw_line : y_pos=y_pos+1
10062   if y_pos-y_offs>status_line_y-3 then gosub scroll_up :return:else begin
10063     curr_line$=line_buff$(y_pos):gosub redraw_line :return
10064   bend
10065 bend
10070 if t$="{up}" and y_pos>0 then begin
10071   me=y_pos-1:curr_line$=line_buff$(y_pos):gosub redraw_line :y_pos=y_pos-1
10072   if y_pos-y_offs<0 then gosub scroll_down :return:else begin
10073     curr_line$=line_buff$(y_pos):gosub redraw_line :return
10074   bend
10075 bend
10080 if t$="x" or t$="c" then begin
10090   if ms>me then iv=ms:ms=me:me=iv
10100   for iv=me to ms step -1
10110     clipboard$(iv-ms) = line_buff$(iv)
10130   next iv
10131   if t$="x" then begin
10132     for a=ms to lastline_y-(me-ms+1)
10133       line_buff$(a)=line_buff$(a+(me-ms+1))
10134     next a
10135     lastline_y=lastline_y-(me-ms+1):for a=lastline_y+1 to lastline_y+(me-ms+1):line_buff$(a)="":next a
10136     y_pos=ms:if y_pos-y_offs<0 then ly=ms:gosub safe_ly
10138   bend
10140   iv=0:mf=0:cs=me-ms+1:ms=0:me=0
10150 bend
10155 gosub  redraw_screen
10160 return

.paste
10200 rem *** paste ***
10210 for iv=cs-1 to 0 step -1
10220   rem insert a line at y_pos
10230   mf=y_pos:rem temp
10240   changed=1:gosub insert_return
10250   y_pos=mf:mf=0
10260   line_buff$(y_pos)=clipboard$(iv)
10270 next iv
10280 gosub  redraw_screen
10290 return

.enable_raw_fn_keys
10500 rem --- set fn keys and make sure f11 doesn't generate paste
10510 key on
10520 fk$="{f1}{f2}{f3}{f4}{f5}{f6}{f7}{f8}{CTRL-P}{CTRL-U}{pur}{CTRL-W}{CTRL-Y}{CTRL-Z}"+chr$(132)
10530 for r=1 to 15:key r, mid$(fk$,r,1):next r
10540 return

.more_help
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
10700 rem --- jump to previous file
10710 if changed=1 then begin
10720   gosub clear_status_bar:foreground hl
10730   print "{rvon}current file not saved - press any key";:get key t$
10740   gosub clear_status_bar:gosub build_status_bar:return
10750 bend
10760 temp=pv:nf$=pv$:gosub load_specified_file
10765 ly=temp:gosub safe_ly
10770 return

.navigate_to_label_under_cursor
11000 rem --- navigate to label under cursor
11010 gosub get_term_under_cursor : rem get term under cursor
11020 nz%=1: rem nav to label flag = true
11030 gosub show_labels : rem gosub find label screen
11040 return

.get_term_under_cursor
11100 rem --- get term under cursor into fz$
11110 gosub get_start_of_label : rem start of label into sz
11120 gosub get_end_of_label : rem end of label into ez
11130 fz$ = mid$(curr_line$,sz+1,ez-sz+1)
11140 return

.get_start_of_label
11200 rem --- get start of label into sz
11205 k = x_pos - 1 : zz$=" :" : sz = 0
11210 do while k >= 0
11220   if instr(zz$,mid$(curr_line$,k+1,1)) then sz = k + 1 : exit
11230   k = k - 1
11240 loop
11250 return

.get_end_of_label
11300 rem --- get end of label into ez
11310 k = x_pos : zz$=" :" : ez = len(curr_line$)-1
11320 do while k <= ez
11330   if instr(zz$,mid$(curr_line$,k+1,1)) then ez = k - 1 : exit
11340   k = k + 1
11350 loop
11360 return

#output "11.parse-new"

  clr

#declare quote$, parser_file$, t$, blank_line$, type_ident$
#declare curr_src_line$, next_line_flag, next_line$

  parser_file$="11.parse":mk$="@~":quote$=chr$(34):dbl_quote_char=34:sngl_quote_char=39
  key on
  print "{rvon}ELEVEN preprocessor v0.5.0{rvof}":print
  key7,"scratch"+quote$+parser_file$+quote$+":dsave"+chr$(34)+parser_file$+quote$+":dverify"+quote$+parser_file$
  t$="                                                                               ":blank_line$=t$+t$+t$:t$=""
  tokens$(0)=" print input if then else do loop while until gosub goto open close dopen dclose for next getkey hex$ dim peek poke wait dec chr$ asc sgn sqr str$"
  tokens$(0)=tokens$(0)+" graphic clr screen def begin bend len mid$ right$ left$ instr for next step trap border and foreground "
  tokens$(1)=" background set abs sin cos tan log fre cursor pixel window rwindow line box circle ellipse palette restore data err$ er el cursor on off"
  tokens$(1)=tokens$(1)+" val scratch return rnd stop bank ti do or st if el er on to pen get end int not ds run using dot "
  tokens$(2)=" append atn auto backup bload boot bsave bump bverify catalog change char cmd collision color concat cont copy wpoke wpeek setbit clrbit "
  tokens$(3)=" dclear deffn delete fn dir disk dload dma dmode dpat dsave dverify edma envelope erase exit exp fast filter find go64 header help highlight "
  tokens$(4)=" joy list load locate lpen mod monitor mouse movspr new paint play pointer polygon pos pot pudef "
  tokens$(5)=" rcolor rdot read record rem rename resume rgr rmouse rplay rreg rspcolor rsppos rsprite save scnclr sleep slow sound spc sprcolor "
  tokens$(6)=" sprite sprsav sys tab tempo troff tron type usr verify vol xor key vsync rcursor t@& c@& rgraphic fread pointer "
  dumb_cmds$=" bload bsave dload to save dir collect dopen dclose backup fread get "
  gosub get_filename
  bank 128:poke 0,65
  type_ident$(0)="":type_ident$(1)="%":type_ident$(2)="$":type_ident$(3)="&"
  dim bin_conv(16) : rem bit shifter's fast binary conversion. thanks a lot!
  bin_conv(0)=1:for i=1 to 16:bin_conv(i)=bin_conv(i-1)+bin_conv(i-1):next i
  dim map_dest_to_src_lineno%(1000)             : rem src file line nr <-> internal idx mapping
  dim dest_line$(1000)             : rem post processed lines
  dim element_cnt(4)                 : rem element count per type
  dim var_table$(4,200)            : rem variable table per type
  dim define_val$(200)              : rem define values table
  dim label_name$(200),label_lineno(200):label_cnt=0 : rem label table & count
  dim args$(32)               : rem argument list
  dim struct_name$(30),struct_fields$(30)       : rem struct names, fields
  dim struct_vars$(30)               : rem struct vars (each entry has string of vars)
  struct_idx=0                      : rem index to next free struct definition

rem ------------------------- pass 1 ------------------------------------

.pass_1
'------
  next_line_flag = 0
  whitespace$=chr$(32)+chr$(160)+"{rght}{ensh}" : rem whitespace
  rem cleanup temporary files
  src_lineno = 0 : rem source code line counter
  print "pass 1 ";:cur_src_lineno=0
  clr ti: rem keep start time for timing
  cb=$8030000:curr_addr=cb:total_lines=peek(curr_addr)+256*peek(curr_addr+1):curr_addr=curr_addr+2
  do while cur_src_lineno<>total_lines : rem until target lines is reached
    gosub read_next_line
    cut_tail_idx=instr(curr_src_line$, "'")  ' single quote
    if cut_tail_idx<>0 then begin
      if instr(curr_src_line$, quote$)<>0 then begin  ' double quote
        cut_tail_idx=0
        bank 0:cur_line_len=peek(ptr)-1:addr=peek(ptr+1)+256*peek(ptr+2):bank 1
        for r= . to cur_line_len
          chr=peek(addr+r)
          if chr=dbl_quote_char then quote_flag=abs(quote_flag-1):else if chr=sngl_quote_char and quote_flag=0 then cut_tail_idx=r+1:r=999
        next
      bend
      if cut_tail_idx then curr_src_line$=left$(curr_src_line$,cut_tail_idx-1)
    bend
    rem strip whitespace from end
    s$=curr_src_line$:gosub strip_characters :curr_src_line$=s$
    if curr_src_line$<>"" then begin
      delete_line_flag=0 : rem delete line flag
      if verbose then print ">>"dest_line_idx;src_lineno;curr_src_line$
      if left$(curr_src_line$,1)="." then next_line_flag=1:gosub add_to_label_table : rem label
      if left$(curr_src_line$,1)="#" then begin
        if instr(curr_src_line$,"ifdef")=2 then s$=mid$(curr_src_line$,8):gosub is_s$_defined :delete_line_flag=1
        if instr(curr_src_line$,"endif")=2 then inside_ifdef=0:delete_line_flag=1
        if instr(curr_src_line$,"define")=2 then define_flag=1:gosub declare_s$_var :define_flag=0
        if instr(curr_src_line$,"declare")=2 then define_flag=0:gosub declare_s$_var 
        if instr(curr_src_line$,"output")=2 then gosub set_output_file
        if instr(curr_src_line$,"struct")=2 then gosub read_in_struct_details :delete_line_flag=1
      bend
      if inside_ifdef=1 then goto parser_loop_skip
      if left$(curr_src_line$,4)="data" or right$(curr_src_line$,5)="begin" then next_line_flag=1
      if delete_line_flag=0 then begin
        if verbose=0 then print ".";
        s$=curr_src_line$
        gosub replace_vars_and_labels :rem replace vars & labels in s$
        gosub check_for_creation_of_struct_object
        if right$(curr_dest_line$,1)="_" then curr_dest_line$=left$(curr_dest_line$,len(curr_dest_line$)-1):next_line_flag=0:cont_next_line_flag=1:else cont_next_line_flag=0
        gosub safe_add_to_current_or_next_line :rem safe add curr_dest_line$+s$ to current or next dest_line$(dest_line_idx)
        if right$(s$,4)="bend" or right$(s$,6)="return" or left$(s$,2)="if" then next_line_flag=1
      bend : rem endif delete_line_flag=0
    bend : rem endif curr_src_line$<>""

.parser_loop_skip
    src_lineno=src_lineno+1 : rem increase source code line (for error msgs...)
    if verbose then print "src_lineno=";src_lineno:get key z$
  loop
  
  if curr_dest_line$<>"" then dest_line$(dest_line_idx)=curr_dest_line$:dest_line_idx=dest_line_idx+1
  close 1
  gosub save_filename : rem set output filename
  scratch "11temp":scratch "11tokenized"

rem ------------------------- pass 2 ------------------------------------

.pass_2
'------
  open 1,1,5,"11temp,s,w"
  print chr$(13)"{down}pass 2 ";
  for dst_idx=0 to dest_line_idx-1
    s$=dest_line$(dst_idx)  : if verbose then print dst_idx;"{yel}=> "s$:else print ".";
    do while instr(s$,mk$)<>0
       s1=instr(s$,mk$):s2=instr(s$,mk$,s1+2)
       c$=mid$(s$,s1+2,s2-s1-2)
       gosub check_c$_is_label
       s$=left$(s$,s1-1)+c$+mid$(s$,s2+2)
    loop
    if verbose then print "<= ";str$(dst_idx)+s$
      print #1,str$(dst_idx)+" "+s$
  next dst_idx

  gosub dump_vars :rem --- dump vars
  for r=0 to 10:print #1,str$(32000+r):next r
  f$="dC:dS"+quote$+"11tokenized"+quote$+":ifds<>0then?"+quote$+"disc error: "+quote$+";ds$:else?"+quote$+"{home}{home}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}"+quote$+":dL"+quote$+"11.post"
  print #1,f$
  close 1
  print"{down}"
  print "elapsed time:";ti;"ticks"
  print"{home}{home}{clr}{down}{down}edma 0,$1fff,$8022000,$2001:new restore{down}{down}":print"run{home}";:rem load '11.tokenize' from cache
  bank 128
  poke 208,2   : rem no of chars in keyboard buffer
  poke 688,13,13:rem return
  end


.declare_s$_var
'--------------
  rem declare var(s) in s$
  s$=mid$(curr_src_line$,10-define_flag):d$=",;":ib=1:gosub parse_arguments :ib=0: rem split parameters
  rem (ib=ignore brackets)
  next_line$="" : rem new line if dimensioning...
  if arg_cnt<0 then parser_error$="?declare parameter missing in line "+str$(src_lineno):goto return_to_editor_with_error
  for i=0 to arg_cnt
    p$=args$(i):gosub parse_declared_var
  next i
  if next_line$<>"" then delete_line_flag=0:curr_src_line$="^^"+next_line$:else delete_line_flag=1
  return

.parse_declared_var
'------------------
  rem --- parse declared var
    dimension$="" : value$=""
    bkt_open_idx=instr(p$,"("):bkt_close_idx=instr(p$,")"):equals_idx=instr(p$,"=")
    if equals_idx<>0 then begin : rem --- assignment
      value$=mid$(p$,equals_idx+1)
      p$=left$(p$,equals_idx-1)
      tr$=whitespace$
      s$=p$:gosub strip_characters :p$=s$
      s$=value$:gosub strip_tr$_from_beginning :gosub strip_characters :value$=s$
      if left$(value$,1)="$"then hx$=mid$(value$,2):gosub convert_hex :value$=c$
      if left$(value$,1)="%"then bi$=mid$(value$,2):gosub convert_binary :value$=c$
    bend
    if bkt_open_idx<>0 and bkt_close_idx<>0 then begin : rem --- dimension
      dimension$=mid$(p$, bkt_open_idx+1, bkt_close_idx-bkt_open_idx-1)
      pp$=left$(p$, bkt_open_idx-1)
      s$=dimension$:dz=1:gosub replace_vars_and_labels :dz=0:dimension$=s$:p$=pp$: rem check for define tokens
      delete_line_flag=0
    bend
    ty=0 : rem var type
    t$=right$(p$,1): rem type (if any) in t$
    if verbose then print "adding {rvon}";
    if instr("%&$",t$)=0 then t$="":ty=0
    if define_flag=1 then ty=4
    if t$="%" then ty=1
    if t$="$" then ty=2
    if t$="&" then ty=3
    var_table$(ty,element_cnt(ty)) = p$
    if dimension$<>"" then begin
      id=element_cnt(ty):gosub generate_varname : rem fetch varname in vn$
      if define_flag=0 then next_line$=next_line$+"dim "+vn$+t$+"("+dimension$+"):"
    bend
    if value$<>"" then begin
      id=element_cnt(ty):gosub generate_varname 
      if define_flag=0 then next_line$=next_line$+vn$+t$+"="+value$+":"
    bend
    if define_flag=1 then define_val$(element_cnt(ty))=value$
    if verbose then print p$;"{rvof}: ";element_cnt(ty)
    element_cnt(ty)=element_cnt(ty)+1
  return

.set_output_file
'---------------
  s$=mid$(curr_src_line$,8):d$=",;":gosub parse_arguments
  if arg_cnt<>0 then print "?invalid parameters in line ";src_lineno:end
  s$=args$(0):tr$=quote$:gosub strip_tr$_from_beginning :gosub strip_characters : rem trim quotes left & right
  if verbose then print "setting output file to {rvon}"+s$+"{rvof}"
  of$=s$
  delete_line_flag=1 : rem disable passthrough
  return


.add_to_label_table
'------------------
  if verbose then print "label ";curr_src_line$;" at line ";dest_line_idx
  delete_line_flag=1
  label_name$(label_cnt)=mid$(curr_src_line$,2)
  label_lineno(label_cnt)=dest_line_idx+1
  label_cnt=label_cnt+1 : rem increase label count
  return

.return_to_editor_with_error
'---------------------------
  rem return to editor with error
  bank 4 : rem set error mailbox flag
  for r=1 to len(parser_error$):poke $4ff30+r-1,asc(mid$(parser_error$,r,1)):next r
  poke $4ff30+r-1,0
  poke dec("ff09"),mod(src_lineno,256):poke dec("ff0a"),src_lineno/256
  poke dec("ff07"),peek(dec("ff07"))or2 : rem set autojump flag
  dclose
  goto chain_editor


.strip_tr$_from_beginning
'------------------------
  rem -- strip tr$ from beginning of string in s$ --
  do while instr(tr$,(left$(s$,1)))
    s$=mid$(s$,2)
  loop
  return


.strip_characters
'----------------
  rem -- strip charcters in tr$ from end of s$ --
  do while instr(tr$,right$(s$,1))
    s$=left$(s$,len(s$)-1)
  loop
  return


.parse_arguments
'---------------
  rem -- parse arguments --
  rem     in: s$ = string, d$=delimiter
  rem    out: args$(x)=argument list, arg_cnt=argument count
  rem         args$(0)=first arg, args$(1)=second arg...
  rem
  
  arg_cnt=0:al$=s$:al=len(s$):ig=0
  if al=0 then arg_cnt=-1:return : rem no string
  for ai=0 to 31:args$(ai)="":next ai
  for ai=1 to al
  b$=mid$(al$,ai,1)
  if b$="(" and ib=1 then ig=1
  if b$=")" and ib=1 then ig=0
  if instr(d$,b$)<>0 and ig=0 then begin
      s$=args$(arg_cnt):tr$=" ":gosub strip_tr$_from_beginning :gosub strip_characters :args$(arg_cnt)=s$
      arg_cnt=arg_cnt+1
    bend : else begin
      args$(arg_cnt)=args$(arg_cnt)+b$
    bend
  next ai
  s$=args$(arg_cnt):tr$=" ":gosub strip_tr$_from_beginning :gosub strip_characters :args$(arg_cnt)=s$
  s$=al$ : rem restore s$
  return


.replace_vars_and_labels
'-----------------------
  rem -- replace vars & labels in source string --
  rem    in:   s$ = source string
  rem    out:  s$ = dest string with replaced items
  
  if left$(s$,2)="^^" then s$=right$(s$,len(s$)-2):return
  quote_flag=0:a$="":c$="":struct_idx=0:tg=0
  cx$="?<>=+-#*/^,.:;() " : d$=cx$
  for ii=1 to len(s$):b$=mid$(s$,ii,1)
  if b$=":" and quote_flag=0 then struct_idx=0:ri=0
  if struct_idx and b$="(" then ri=0 : d$=cx$
  if struct_idx and b$=")" then ri=-1 : d$=cx$ + "dpub"
    if b$=quote$ then begin : quote_flag=abs(quote_flag-1)
    if quote_flag=1 then gosub check_token_for_subbing :a$=a$+c$:c$="":else a$=a$+b$:b$=""
  bend
  if quote_flag=1 then a$=a$+b$:goto rval_skip
  if instr(d$,b$)<>0 then begin
  gosub check_token_for_subbing :a$=a$+c$:c$="":if b$=" "then b$=""
  a$=a$+b$
  bend : else c$=c$+b$

.rval_skip
  next
  gosub check_token_for_subbing 
  s$=a$+c$
  return
  end

.check_token_for_subbing
'-----------------------
  if c$="" or c$="_" then return
  if val(c$)<>0 then tg=0: return     : rem never change numbers
  if c$="0" then c$=".":return        : rem stupid ms basic optimization
  if tg and dz=0 then gosub check_marked_c$_is_label :tg=0:return   : rem replace label
  if c$="goto" then next_line_flag=1
  if c$="goto" or c$="gosub" or c$="trap" then tg=1
  dr=0 : rem did replace flag
  if left$(c$,1)="$" then hx$=mid$(c$,2):gosub convert_hex :return
  if left$(c$,1)="%" then bi$=mid$(c$,2):gosub convert_binary :return
  p$=" "+c$+" "
  for t=0 to 6:if instr(tokens$(t),p$)<>0 then lc$=c$:gosub check_if_command_triggers_shitty_syntax :return
  next
  t$=right$(c$,1):ty=0
  if t$="%" then ty=1
  if t$="$" then ty=2
  if t$="&" then ty=3
  for id=0 to element_cnt(ty)
  if c$=var_table$(ty,id) then gosub generate_varname :c$=vn$+type_ident$(ty):id=element_cnt(ty):dr=1
  next id
  if dr=1 then return
  for id=0 to element_cnt(4):rem check defines table too
    if c$=var_table$(4,id) then c$=define_val$(id):return
  next id
  ci=-1
  for id=0 to struct_idx-1:rem check struct names
    if c$=struct_name$(id) then gosub check_for_creation_of_struct_object :ci=id:id=struct_idx-1:rem create new struct object
  next id
  if ci<>-1 then return:else if asc(c$)=222 then return:rem pi
  if instr(dumb_cmds$, lc$)<>0 and (c$="r" or c$="p" or c$="u8" or c$="w") then return

.unresolved_c$
  parser_error$="?unresolved identifier: '"+c$+"' in line "+str$(src_lineno):sleep 1:goto return_to_editor_with_error
  return


.check_if_command_triggers_shitty_syntax
'---------------------------------------
  rem check if command triggers shitty syntax mode
  rem todo: compare ubik's logic here versus my own 4079 lc$="dopen" logic
  return

.check_marked_c$_is_label
'------------------------
  c$=mk$+c$+mk$ : return : rem mark label

.check_c$_is_label
  dr=0
  for id=0 to label_cnt-1
  if c$=label_table$(id) then c$=str$(label_lineno(id)):id=label_cnt:dr=1
  next id
  if dr then return
  parser_error$="?unresolved label: '"+c$+"' in line"+str$(map_dest_to_src_lineno%(dst_idx-1)):sleep 1:goto return_to_editor_with_error
  return

.convert_binary
'--------------
  rem --- convert binary
  br=0 : rem result
  for b=0 to len(bi$)-1
  bc$=mid$(bi$,len(bi$)-b,1)
  if bc$<>"1" and bc$<>"0" then bank4:poke dec("ff08"),132:goto unresolved_c$
  if bc$="1" then br=br+bin_conv(b)
  next b
  c$=mid$(str$(br),2)
  return
  stop

.convert_hex
'-----------
  rem --- convert hex
  trap illegal_hex_handler
  vl=dec(hx$)
  c$=mid$(str$(vl),2)
  trap
  return

.illegal_hex_handler
'-------------------
  trap:bank 4:poke dec("ff08"),131 : rem set illegal hex
  goto unresolved_c$ : rem jump into error handler


.generate_varname
'----------------
  rem generate varname from index

  if id<26 then vn$=chr$(65+id) : return
  n2=mod(id,26):n1=int(id/26)-1
  vn$=chr$(65+n1)+chr$(65+n2)
  if vn$="do" then vn$="d1":rem avoid any basic terms as var names
  if vn$="go" then vn$="g1"
  if vn$="to" then vn$="t1"
  if vn$="ds" then vn$="d2"
  if vn$="dt" then vn$="d3"
  if vn$="el" then vn$="e1"
  if vn$="er" then vn$="e2"
  if vn$="fn" then vn$="f1"
  if vn$="if" then vn$="i1"
  return
  stop


.get_filename
'------------
  bank 4:ba=dec("ff00")
  if peek(ba+0)=asc("s") and peek(ba+1)=asc("k") then begin
    verbose=peek(dec("ff07"))and16
    f$="":a=ba+16:do while peek(a)<>0:f$=f$+chr$(peek(a)):a=a+1:loop
    if peek(dec("ff07")) and 1 then return
    print "filename? "+f$:print "{up}";
  bend
  input "filename";a$
  if a$="" then print "no filename set":end
  poke ba,asc("s"):poke ba+1,asc("k")
  for r=1 to 16:poke ba+8+r-1,asc(mid$(a$,r,1)):next r
  f$=a$
  return

.save_filename
'-------------
  rem --- save filename
  ad=dec("ff30"):bank 4
  for r=0 to 16:poke ad+r,0:next r
  if of$<>"" then tf$=of$ : else tf$="eleven.out"
  for r=1 to len(tf$):poke ad+r-1,asc(mid$(tf$,r,1)):next r
  return

.chain_editor
'------------
  rem chain editor
  get a$:if a$<>"" then input zz:if zz=1 then adfsdf
  print"{home}{home}{clr}{down}{down}edma 0,$d400,$8000000,$2001:new restore{down}{down}":print"run{home}";:rem load '11.edit' from cache
  bank 128
  poke 208,2     : rem no of chars in keyboard buffer
  poke 688,13,13 : rem return
  end

.is_s$_defined
'-------------
  rem --- define in s$ exists?
  inside_ifdef = 1
  for k=0 to element_cnt(4)
  if var_table$(4,k) = s$ then inside_ifdef=0
  next k
  return

.read_in_struct_details
'----------------------
  rem --- read in struct details
  curr_src_line$=mid$(curr_src_line$,9):gosub read_next_token :rem get next token in s$
  if s$="" then print "error: no struct name found":sleep 1:return
  struct_name$(struct_idx) = s$
  struct_vars$(struct_idx) = curr_src_line$
  struct_idx=struct_idx+1
  return

.read_next_token
'---------------
  rem --- read next token from curr_src_line$ into s$
  s$=curr_src_line$:gosub strip_tr$_from_beginning :gosub strip_characters :curr_src_line$=s$
  sf$=" ":sf=0
  if left$(s$,1)=quote$ then sf$=quote$+", ":sf=2
  a=instr(curr_src_line$,sf$)
  if a<>0 then s$=mid$(curr_src_line$,1,instr(curr_src_line$,sf$)+sf-1):curr_src_line$=mid$(curr_src_line$,instr(curr_src_line$,sf$)+sf+1)
  if a=0 then s$=curr_src_line$:curr_src_line$=""
  return

.read_next_line
'--------------
' read next line into curr_src_line$
  rem --- read next line
  bank 0
  cur_line_len=peek(curr_addr):curr_src_line$=left$(blank_line$,cur_line_len):ptr=pointer(curr_src_line$):curr_addr=curr_addr+1
  b=$10000+peek(ptr+1)+256*peek(ptr+2)
  if cur_line_len<>0 then edma 0,cur_line_len,curr_addr,b : curr_addr=curr_addr+l
  cur_src_lineno=cur_src_lineno+1
  tr$=whitespace$:s$=curr_src_line$:gosub strip_tr$_from_beginning :curr_src_line$=s$
  quote_flag = 0 : rem quotes on
  cut_tail_idx = 0 : rem cut chars from tail
  bank 1
  return


.check_for_creation_of_struct_object
'-----------------------------------
  rem --- check for creation of struct object
  co$=s$:curr_src_line$=s$:zz=-1:rem preserve original string
  gosub read_next_token :rem read next token from curr_src_line$ into s$
  for zi=0 to struct_idx
    if s$=struct_name$(zi) then zz=zi:zi=struct_idx
  next zi
  if zz=-1 then curr_src_line$=co$:s$=co$:return
  gosub read_next_token :sk$=s$:rem get struct object name
  bl=instr(sk$,"(")
  rem *** found it, so make dim's for each member var
  s$=struct_vars$(zz):gosub parse_arguments :rem parse args into args$(), arg_cnt
  zz=1:sz=0
  for zi=0 to arg_cnt
    if args$(zi)<>"" then begin
      print "args$(";zi;")=";args$(zi)
      if bl=0 then s$=sk$+"{CBM-P}"+args$(zi):print "struct: ";s$:p$=s$:gosub parse_declared_var 
      if bl<>0 then s$=left$(sk$,bl-1)+"{CBM-P}"+args$(zi)+mid$(sk$,bl):print "struct: ";s$:p$=s$:gosub parse_declared_var :struct_fields$(sz)=p$:sz=sz+1
      rem if next_line$<>"" then delete_line_flag=0:curr_src_line$="^^"+next_line$:else delete_line_flag=1
    bend
  next zi
  s$=next_line$:gosub safe_add_to_current_or_next_line :next_line$="":rem safe add curr_dest_line$+s$ to current dest_line$(dest_line_idx)
  gosub read_next_token :if s$<>"=" then curr_src_line$="":return
  gosub read_next_token :sz=0:sr=0:sm=0:rem read next token from curr_src_line$ into s$
  do while s$<>""
      if s$="_" then src_lineno=src_lineno+1:gosub read_next_line :goto cfcoso_skip :rem read next line
    if sm=0 and s$<>"[" then print "error: expected [":sleep 1:stop
    if sm=0 and s$="[" then sm=1:goto cfcoso_skip
    if sm=1 and s$<>"[" and s$<>"]" then print "error: expected [ or ]":sleep 1:stop
    if sm=2 then begin
      if left$(s$,1)="]" then sr=sr+1:sz=0:sm=1:s$="":goto cfcoso_nextrow :rem next row
  rem if left$(s$,1)=quote$ then ss$=s$:tr$="":do while right$(ss$,2)<>(quote$+","):gosub read_next_token :ss$=ss$+s$:print ss$:loop:s$=ss$:tr$=whitespace$:stop
      if right$(s$,1)="," then s$=left$(s$,len(s$)-1)
      s$=struct_fields$(sz)+"("+str$(sr)+")="+s$:gosub replace_vars_and_labels :gosub safe_add_to_current_or_next_line :s$="":sz=sz+1:rem safe add to dest_line$(dest_line_idx)

.cfcoso_nextrow
    bend
    if sm=1 and s$="[" then sm=2:sz=0
    if sm=1 and s$="]" then sm=0

.cfcoso_skip
    gosub read_next_token :rem read next token from curr_src_line$ into s$
  loop
  s$="":curr_src_line$="":next_line$=""::zz$="z"
  return

.safe_add_to_current_or_next_line
'--------------------------------
  rem --- safe add curr_dest_line$+s$ to current or next dest_line$(dest_line_idx)
  if len(curr_dest_line$)+len(s$)+len(str$(dest_line_idx))>=159 then next_line_flag=1
  if zz$<>"" then s$="":zz$="":return:rem force s$ to empty
  if next_line_flag=1 then begin
    dest_line$(dest_line_idx)=curr_dest_line$:curr_dest_line$=s$
    map_dest_to_src_lineno%(dest_line_idx)=src_lineno:
    dest_line_idx=dest_line_idx+1 : next_line_flag=0
  bend : else begin rem -- add to curr_dest_line$
    if curr_dest_line$<>"" and cont_next_line_flag=0 and right$(curr_dest_line$,1)<>":" then curr_dest_line$=curr_dest_line$+":"
    curr_dest_line$=curr_dest_line$+s$
  bend
  if verbose then print "<<"dest_line_idx;s$
  return

.dump_vars
'---------
  rem --- dump vars
  for ty = 0 to 3
    for id = 0 to element_cnt(ty)
      gosub  generate_varname : rem get vn$ (optimised var name)
      print#1, str$(dst_idx)+" rem " + var_table$(ty,id) + " = :"+vn$+type_ident$(ty)+":"
      dst_idx=dst_idx+1
    next id
  next ty
  return

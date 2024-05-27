#output "11.parse-new"

    1 clr

#declare quote$, parser_file$, t$, blank_line$, type_ident$
#declare current_line$, next_line_flag, next_line$

    2 parser_file$="11.parse":mk$="@~":quote$=chr$(34):h=34:y=39
    4 key on
    5 print "{rvon}ELEVEN preprocessor v0.5.0{rvof}":print
    6 key7,"scratch"+quote$+parser_file$+quote$+":dsave"+chr$(34)+parser_file$+quote$+":dverify"+quote$+parser_file$
    8 t$="                                                                               ":blank_line$=t$+t$+t$:t$=""
   10 rw$(0)=" print input if then else do loop while until gosub goto open close dopen dclose for next getkey hex$ dim peek poke wait dec chr$ asc sgn sqr str$"
   20 rw$(0)=rw$(0)+" graphic clr screen def begin bend len mid$ right$ left$ instr for next step trap border and foreground "
   30 rw$(1)=" background set abs sin cos tan log fre cursor pixel window rwindow line box circle ellipse palette restore data err$ er el cursor on off"
   35 rw$(1)=rw$(1)+" val scratch return rnd stop bank ti do or st if el er on to pen get end int not ds run using dot "
   70 rw$(2)=" append atn auto backup bload boot bsave bump bverify catalog change char cmd collision color concat cont copy wpoke wpeek setbit clrbit "
   71 rw$(3)=" dclear deffn delete fn dir disk dload dma dmode dpat dsave dverify edma envelope erase exit exp fast filter find go64 header help highlight "
   72 rw$(4)=" joy list load locate lpen mod monitor mouse movspr new paint play pointer polygon pos pot pudef "
   73 rw$(5)=" rcolor rdot read record rem rename resume rgr rmouse rplay rreg rspcolor rsppos rsprite save scnclr sleep slow sound spc sprcolor "
   75 rw$(6)=" sprite sprsav sys tab tempo troff tron type usr verify vol xor key vsync rcursor t@& c@& rgraphic fread pointer "
   85 dc$=" bload bsave dload to save dir collect dopen dclose backup fread get "
   90 gosub get_filename
  100 bank 128:poke 0,65
  110 type_ident$(0)="":type_ident$(1)="%":type_ident$(2)="$":type_ident$(3)="&"
  111 dim bin_conv(16) : rem bit shifter's fast binary conversion. thanks a lot!
  112 bin_conv(0)=1:for i=1 to 16:bin_conv(i)=bin_conv(i-1)+bin_conv(i-1):next i
  115 dim ln%(1000)             : rem src file line nr <-> internal idx mapping
  120 dim dest_line$(1000)             : rem post processed lines
  121 dim element_cnt(4)                 : rem element count per type
  122 dim var_table$(4,200)            : rem variable table per type
  123 dim define_val$(200)              : rem define values table
  125 dim label_name$(200),label_lineno(200):label_cnt=0 : rem label table & count
  126 dim args$(32)               : rem argument list
  127 dim struct_name$(30),struct_fields$(30)       : rem struct names, fields
  128 dim struct_vars$(30)               : rem struct vars (each entry has string of vars)
  129 struct_idx=0                      : rem index to next free struct definition
  130 :
  131 rem ------------------------- pass 1 ------------------------------------
  135 :
  138 next_line_flag = 0
  140 whitespace$=chr$(32)+chr$(160)+"{rght}{ensh}" : rem whitespace
  150 rem cleanup temporary files
  185 src_lineno=0 : rem source code line counter
  190 print "pass 1 ";:rl=0
  195 clr ti: rem keep start time for timing
  198 cb=$8030000:ca=cb:tl=peek(ca)+256*peek(ca+1):ca=ca+2
  200 do while rl<>tl : rem until target lines is reached
  207   gosub read_next_line
  422   ct=instr(current_line$,"'"): if ct=0 then goto 580
  423   if instr(current_line$,chr$(34))=0 then goto 540
  424   ct=0
  425   bank 0:l=peek(p)-1:a=peek(p+1)+256*peek(p+2):bank 1
  440   for r= . to l:c=peek(a+r):if c=h then q=abs(q-1):else if c=y and q=0 then ct=r+1:r=999
  520   next
  540   if ct then current_line$=left$(current_line$,ct-1)
  560   rem strip whitespace from end
  580   s$=current_line$:gosub strip_characters :current_line$=s$
  585   if current_line$<>"" then begin
  586     dl=0 : rem delete line flag
  590     if vb then print ">>"ln;src_lineno;current_line$
  600     if left$(current_line$,1)="." then next_line_flag=1:gosub add_to_label_table : rem label
  601     if left$(current_line$,1)="#" then begin
  602       cc=1
  603       if instr(current_line$,"ifdef")=2 then s$=mid$(current_line$,8):gosub is_s$_defined :dl=1
  604       if instr(current_line$,"endif")=2 then sh=0:dl=1
  605       if instr(current_line$,"define")=2 then df=1:gosub declare_s$_var :df=0
  606       if instr(current_line$,"declare")=2 then df=0:gosub declare_s$_var 
  607       if instr(current_line$,"output")=2 then gosub set_output_file
  608       if instr(current_line$,"struct")=2 then gosub read_in_struct_details :dl=1
  650     bend
  653     if sh=1 then goto 750
  654     if left$(current_line$,4)="data" or right$(current_line$,5)="begin" then next_line_flag=1
  655     if dl=0 then begin
  656       if vb=0 then print ".";
  660       s$=current_line$
  670       gosub replace_vars_and_labels :rem replace vars & labels in s$
  671       gosub check_for_creation_of_struct_object
  672       if right$(l$,1)="_" then l$=left$(l$,len(l$)-1):next_line_flag=0:cn=1:else cn=0
  675       gosub safe_add_to_current_or_next_line :rem safe add l$+s$ to current or next dest_line$(ln)
  732       if right$(s$,4)="bend" or right$(s$,6)="return" or left$(s$,2)="if" then next_line_flag=1
  735     bend : rem endif dl=0
  740   bend : rem endif c$<>""
  750   src_lineno=src_lineno+1 : rem increase source code line (for error msgs...)
  755   if vb then print "src_lineno=";src_lineno:getkey z$
  760 loop
  765 if l$<>"" then dest_line$(ln)=l$:ln=ln+1
  780 close 1
  782 gosub save_filename : rem set output filename
  785 scratch "11temp":scratch "11tokenized"
  786 rem ------------------------- pass 2 ------------------------------------
  787 :
  788 open 1,1,5,"11temp,s,w"
  790 print chr$(13)"{down}pass 2 ";
  795 lp=0   : rem current line pointer
  798 ol$="" : rem current optimized line
  800 for si=0 to ln-1
  810 s$=dest_line$(si)  : if vb then print si;"{yel}=> "s$:elseprint ".";
  812   do while instr(s$,mk$)<>0
  814      s1=instr(s$,mk$):s2=instr(s$,mk$,s1+2)
  815      c$=mid$(s$,s1+2,s2-s1-2)
  820      gosub check_c$_is_label
  830      s$=left$(s$,s1-1)+c$+mid$(s$,s2+2)
  860   loop
  865 if vb then print "<= ";str$(si)+s$
  875   print #1,str$(si)+" "+s$
  876 next si
  880 gosub dump_vars :rem --- dump vars
  881 for r=0 to 10:print #1,str$(32000+r):next r
  886 f$="dC:dS"+quote$+"11tokenized"+quote$+":ifds<>0then?"+quote$+"disc error: "+quote$+";ds$:else?"+quote$+"{home}{home}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}"+quote$+":dL"+quote$+"11.post"
  890 print #1,f$
  900 close 1
  905 print"{down}"
  910 et=ti-bt:print "elapsed time:";et;"ticks"
  920 print"{home}{home}{clr}{down}{down}edma 0,$1fff,$8022000,$2001:new restore{down}{down}":print"run{home}";:rem load '11.tokenize' from cache
  925 bank 128
  930 poke 208,2   : rem no of chars in keyboard buffer
  940 poke 688,13,13:rem return
  950 end
  999 :

.declare_s$_var
'--------------
 1000 rem declare var(s) in s$
 1010 s$=mid$(current_line$,10-df):d$=",;":ib=1:gosub parse_arguments :ib=0: rem split parameters
 1012 rem (ib=ignore brackets)
 1015 next_line$="" : rem new line if dimensioning...
 1020 if ac<0 then pe$="?declare parameter missing in line "+str$(src_lineno):goto return_to_editor_with_error
 1021 for i=0 to ac
 1022   p$=args$(i):gosub parse_declared_var
 1023 next i
 1024 if next_line$<>"" then dl=0:current_line$="^^"+next_line$:else dl=1
 1025 return

.parse_declared_var
'------------------
 1030 rem --- parse declared var
 1031 : di$="" : vl$=""
 1032 : b1=instr(p$,"("):b2=instr(p$,")"):eq=instr(p$,"=")
 1035 : if eq<>0 then begin : rem --- assignment
 1036 :   vl$=mid$(p$,eq+1):p$=left$(p$,eq-1):tr$=whitespace$:s$=p$:gosub strip_characters :p$=s$
 1040 :   s$=vl$:gosub strip_tr$_from_beginning :gosub strip_characters :vl$=s$
 1045 :   if left$(vl$,1)="$"then hx$=mid$(vl$,2):gosub convert_hex :vl$=c$
 1046 :   if left$(vl$,1)="%"then bi$=mid$(vl$,2):gosub convert_binary :vl$=c$
 1048 : bend
 1050 : if b1<>0 and b2<>0 then begin : rem --- dimension
 1052 :   di$=mid$(p$,b1+1,b2-b1-1) : pp$=left$(p$,b1-1)
 1054 :   s$=di$:dz=1:gosub replace_vars_and_labels :dz=0:di$=s$:p$=pp$: rem check for define tokens
 1058 :   dl=0
 1060 : bend
 1062 : ty=0 : rem var type
 1064 : t$=right$(p$,1): rem type (if any) in t$
 1066 : if vb then print "adding {rvon}";
 1068 : if instr("%&$",t$)=0 then t$="":ty=0
 1069 : if df=1 then ty=4
 1070 : if t$="%" then ty=1
 1072 : if t$="$" then ty=2
 1073 : if t$="&" then ty=3
 1074 : var_table$(ty,element_cnt(ty)) = p$
 1076 : if di$<>"" then begin
 1078 :   id=element_cnt(ty):gosub generate_varname : rem fetch varname in vn$
 1084 :   if df=0 then next_line$=next_line$+"dim "+vn$+t$+"("+di$+"):"
 1085 : bend
 1088 : if vl$<>"" then begin
 1089 :   id=element_cnt(ty):gosub generate_varname 
 1090 :   if df=0 then next_line$=next_line$+vn$+t$+"="+vl$+":"
 1092 : bend
 1093 : if df=1 then define_val$(element_cnt(ty))=vl$
 1099 : if vb then print p$;"{rvof}: ";element_cnt(ty)
 1100 : element_cnt(ty)=element_cnt(ty)+1
 1120 return

.set_output_file
'---------------
 1200 s$=mid$(current_line$,8):d$=",;":gosub parse_arguments
 1210 if ac<>0 then print "?invalid parameters in line ";ln%(src_lineno):end
 1220 s$=args$(0):tr$=quote$:gosub strip_tr$_from_beginning :gosub strip_characters : rem trim quotes left & right
 1230 if vb then print "setting output file to {rvon}"+s$+"{rvof}"
 1240 of$=s$
 1250 dl=1 : rem disable passthrough
 1260 return
 1485 :
 1490 rem add to label table
 1495 :

.add_to_label_table
'------------------
 1500 if vb then print "label ";current_line$;" at line ";ln
 1505 dl=1
 1506 label_name$(label_cnt)=mid$(current_line$,2)
 1510 label_lineno(label_cnt)=ln+1
 1520 label_cnt=label_cnt+1 : rem increase label count
 1530 return

.return_to_editor_with_error
'---------------------------
 1800 rem return to editor with error
 1805 bank 4 : rem set error mailbox flag
 1807 for r=1 to len(pe$):poke $4ff30+r-1,asc(mid$(pe$,r,1)):next r
 1808 poke $4ff30+r-1,0
 1810 poke dec("ff09"),mod(src_lineno,256):poke dec("ff0a"),src_lineno/256
 1820 poke dec("ff07"),peek(dec("ff07"))or2 : rem set autojump flag
 1830 dclose
 1840 goto chain_editor


.strip_tr$_from_beginning
'------------------------
 2000 rem -- strip tr$ from beginning of string in s$ --
 2010 do while instr(tr$,(left$(s$,1)))
 2020   s$=mid$(s$,2)
 2030 loop
 2040 return


.strip_characters
'----------------
 2050 rem -- strip charcters in tr$ from end of s$ --
 2060 do while instr(tr$,right$(s$,1))
 2070   s$=left$(s$,len(s$)-1)
 2080 loop
 2090 return


.parse_arguments
'---------------
 2100 rem -- parse arguments --
 2101 rem     in: s$ = string, d$=delimiter
 2102 rem    out: args$(x)=argument list, ac=argument count
 2103 rem         args$(0)=first arg, args$(1)=second arg...
 2104 rem

 2110 ac=0:al$=s$:al=len(s$):ig=0
 2112 if al=0 then ac=-1:return : rem no string
 2115 for ai=0 to 31:args$(ai)="":next ai
 2120 for ai=1 to al
 2125 b$=mid$(al$,ai,1)
 2126 if b$="(" and ib=1 then ig=1
 2127 if b$=")" and ib=1 then ig=0
 2130 if instr(d$,b$)<>0 and ig=0 then begin
 2132     s$=args$(ac):tr$=" ":gosub strip_tr$_from_beginning :gosub strip_characters :args$(ac)=s$
 2140     ac=ac+1
 2150   bend : else begin
 2160     args$(ac)=args$(ac)+b$
 2170   bend
 2180 next ai
 2182 s$=args$(ac):tr$=" ":gosub strip_tr$_from_beginning :gosub strip_characters :args$(ac)=s$
 2185 s$=al$ : rem restore s$
 2190 return


.replace_vars_and_labels
'-----------------------
 3000 rem -- replace vars & labels in source string --
 3001 rem    in:   s$ = source string
 3002 rem    out:  s$ = dest string with replaced items

 3007 if left$(s$,2)="^^" then s$=right$(s$,len(s$)-2):return
 3010 q=0:a$="":c$="":struct_idx=0:tg=0
 3012 cx$="?<>=+-#*/^,.:;() " : d$=cx$
 3020 for ii=1 to len(s$):b$=mid$(s$,ii,1)
 3025 if b$=":" and q=0 then struct_idx=0:ri=0
 3030 if struct_idx and b$="(" then ri=0 : d$=cx$
 3035 if struct_idx and b$=")" then ri=-1 : d$=cx$ + "dpub"
 3040   if b$=quote$ then begin : q=abs(q-1)
 3042   if q=1 then gosub check_token_for_subbing :a$=a$+c$:c$="":else a$=a$+b$:b$=""
 3044 bend
 3050 if q=1 then a$=a$+b$:goto rval_skip
 3060 if instr(d$,b$)<>0 then begin
 3070 gosub check_token_for_subbing :a$=a$+c$:c$="":if b$=" "then b$=""
 3080 a$=a$+b$
 3085 bend : else c$=c$+b$

.rval_skip
 3200 next
 3202 gosub check_token_for_subbing 
 3210 s$=a$+c$
 3220 return
 3900 end

.check_token_for_subbing
'-----------------------
 4000 if c$="" or c$="_" then return
 4001 if val(c$)<>0 then tg=0: return     : rem never change numbers
 4002 if c$="0" then c$=".":return        : rem stupid ms basic optimization
 4005 if tg and dz=0 then gosub check_marked_c$_is_label :tg=0:return   : rem replace label
 4006 if c$="goto" then next_line_flag=1
 4007 if c$="goto" or c$="gosub" or c$="trap" then tg=1
 4008 dr=0 : rem did replace flag
 4009 if left$(c$,1)="$" then hx$=mid$(c$,2):gosub convert_hex :return
 4010 if left$(c$,1)="%" then bi$=mid$(c$,2):gosub convert_binary :return
 4011 p$=" "+c$+" "
 4012 for t=0 to 6:if instr(rw$(t),p$)<>0 then lc$=c$:gosub check_if_command_triggers_shitty_syntax :return
 4013 next
 4014 t$=right$(c$,1):ty=0
 4015 if t$="%" then ty=1
 4016 if t$="$" then ty=2
 4017 if t$="&" then ty=3
 4020 for id=0 to element_cnt(ty)
 4025 if c$=var_table$(ty,id) then gosub generate_varname :c$=vn$+type_ident$(ty):id=element_cnt(ty):dr=1
 4030 next id
 4070 if dr=1 then return
 4071 for id=0 to element_cnt(4):rem check defines table too
 4072   if c$=var_table$(4,id) then c$=define_val$(id):return
 4073 next id
 4074 ci=-1
 4075 for id=0 to struct_idx-1:rem check struct names
 4076   if c$=struct_name$(id) then gosub check_for_creation_of_struct_object :ci=id:id=struct_idx-1:rem create new struct object
 4077 next id
 4078 if ci<>-1 then return:else if asc(c$)=222 then return:rem pi
 4079 if instr(dc$, lc$)<>0 and (c$="r" or c$="p" or c$="u8" or c$="w") then return
.unresolved_c$
 4080 pe$="?unresolved identifier: '"+c$+"' in line "+str$(src_lineno):sleep 1:goto return_to_editor_with_error
 4090 return


.check_if_command_triggers_shitty_syntax
'---------------------------------------
 4100 rem check if command triggers shitty syntax mode
 4110 rem todo: compare ubik's logic here versus my own 4079 lc$="dopen" logic
 4120 return

.check_marked_c$_is_label
'------------------------
 4500 c$=mk$+c$+mk$ : return : rem mark label
.check_c$_is_label
 4505 dr=0
 4510 for id=0 to label_cnt-1
 4520 if c$=label_table$(id) then c$=str$(label_lineno(id)):id=label_cnt:dr=1
 4530 next id
 4540 if dr then return
 4550 pe$="?unresolved label: '"+c$+"' in line"+str$(ln%(si-1)):sleep 1:goto return_to_editor_with_error
 4567 return

.convert_binary
'--------------
 4800 rem --- convert binary
 4810 br=0 : rem result
 4820 for b=0 to len(bi$)-1
 4830 bc$=mid$(bi$,len(bi$)-b,1)
 4840 if bc$<>"1" and bc$<>"0" then bank4:poke dec("ff08"),132:goto unresolved_c$
 4850 if bc$="1" then br=br+bin_conv(b)
 4860 next b
 4870 c$=mid$(str$(br),2)
 4880 return
 4899 stop

.convert_hex
'-----------
 4900 rem --- convert hex
 4905 trap illegal_hex_handler
 4910 vl=dec(hx$)
 4920 c$=mid$(str$(vl),2)
 4925 trap
 4930 return

.illegal_hex_handler
'-------------------
 4940 trap:bank 4:poke dec("ff08"),131 : rem set illegal hex
 4950 goto unresolved_c$ : rem jump into error handler


.generate_varname
'----------------
 4990 :
 4995 rem generate varname from index
 4998 :
 5000 if id<26 then vn$=chr$(65+id) : return
 5010 n2=mod(id,26):n1=int(id/26)-1
 5020 vn$=chr$(65+n1)+chr$(65+n2)
 5021 if vn$="do" then vn$="d1":rem avoid any basic terms as var names
 5022 if vn$="go" then vn$="g1"
 5023 if vn$="to" then vn$="t1"
 5024 if vn$="ds" then vn$="d2"
 5025 if vn$="dt" then vn$="d3"
 5026 if vn$="el" then vn$="e1"
 5027 if vn$="er" then vn$="e2"
 5028 if vn$="fn" then vn$="f1"
 5029 if vn$="if" then vn$="i1"
 5030 return
 5900 stop
 6996 :
 6997 :
 6998 :
 7000 rem get filename
 7010 :


.get_filename
'------------
 7020 bank 4:ba=dec("ff00")
 7030 if peek(ba+0)=asc("s") and peek(ba+1)=asc("k") then begin
 7040   vb=peek(dec("ff07"))and16
 7050   f$="":a=ba+16:do while peek(a)<>0:f$=f$+chr$(peek(a)):a=a+1:loop
 7060   if peek(dec("ff07")) and 1 then return
 7070   print "filename? "+f$:print "{up}";
 7080 bend
 7090 input "filename";a$
 7100 if a$="" then print "no filename set":end
 7110 poke ba,asc("s"):poke ba+1,asc("k")
 7120 for r=1 to 16:poke ba+8+r-1,asc(mid$(a$,r,1)):next r
 7130 f$=a$
 7140 return

.save_filename
'-------------
 7150 rem --- save filename
 7160 ad=dec("ff30"):bank 4
 7170 for r=0 to 16:poke ad+r,0:next r
 7180 if of$<>"" then tf$=of$ : else tf$="eleven.out"
 7190 for r=1 to len(tf$):poke ad+r-1,asc(mid$(tf$,r,1)):next r
 7200 return

.chain_editor
'------------
 7210 rem chain editor
 7220 get a$:if a$<>"" then input zz:if zz=1 then adfsdf
 7225 print"{home}{home}{clr}{down}{down}edma 0,$d400,$8000000,$2001:new restore{down}{down}":print"run{home}";:rem load '11.edit' from cache
 7230 bank 128
 7240 poke 208,2     : rem no of chars in keyboard buffer
 7250 poke 688,13,13 : rem return
 7260 end

.is_s$_defined
'-------------
 9210 rem --- define in s$ exists?
 9220 sh = 1
 9230 for k=0 to element_cnt(4)
 9240 if var_table$(4,k) = s$ then sh=0
 9250 next k
 9260 return

.read_in_struct_details
'----------------------
 9300 rem --- read in struct details
 9305 current_line$=mid$(current_line$,9):gosub read_next_token :rem get next token in s$
 9307 if s$="" then print "error: no struct name found":sleep 1:return
 9310 struct_name$(struct_idx) = s$
 9320 struct_vars$(struct_idx) = current_line$
 9325 struct_idx=struct_idx+1
 9330 return

.read_next_token
'---------------
 9400 rem --- read next token from current_line$ into s$
 9410 s$=current_line$:gosub strip_tr$_from_beginning :gosub strip_characters :current_line$=s$
 9411 sf$=" ":sf=0
 9412 if left$(s$,1)=quote$ then sf$=quote$+", ":sf=2
 9415 a=instr(current_line$,sf$)
 9420 if a<>0 then s$=mid$(current_line$,1,instr(current_line$,sf$)+sf-1):current_line$=mid$(current_line$,instr(current_line$,sf$)+sf+1)
 9430 if a=0 then s$=current_line$:current_line$=""
 9440 return

.read_next_line
'--------------
' read next line into current_line$
 9500 rem --- read next line
 9510 bank 0
 9520 l=peek(ca):current_line$=left$(blank_line$,l):p=pointer(current_line$):ca=ca+1
 9530 b=$10000+peek(p+1)+256*peek(p+2)
 9540 if l<>0 then edma 0,l,ca,b : ca=ca+l
 9550 rl=rl+1
 9560 tr$=whitespace$:s$=current_line$:gosub strip_tr$_from_beginning :current_line$=s$
 9570 q = 0 : rem quotes on
 9580 ct = 0 : rem cut chars from tail
 9590 bank 1
 9595 return


.check_for_creation_of_struct_object
'-----------------------------------
 9600 rem --- check for creation of struct object
 9610 co$=s$:current_line$=s$:zz=-1:rem preserve original string
 9620 gosub read_next_token :rem read next token from current_line$ into s$
 9630 for zi=0 to struct_idx
 9640   if s$=struct_name$(zi) then zz=zi:zi=struct_idx
 9650 next zi
 9660 if zz=-1 then current_line$=co$:s$=co$:return
 9662 gosub read_next_token :sk$=s$:rem get struct object name
 9664 bl=instr(sk$,"(")
 9670 rem *** found it, so make dim's for each member var
 9680 s$=struct_vars$(zz):gosub parse_arguments :rem parse args into args$(), ac
 9685 zz=1:sz=0
 9690 for zi=0 to ac
 9691   if args$(zi)<>"" then begin
 9692     print "args$(";zi;")=";args$(zi)
 9693     if bl=0 then s$=sk$+"{CBM-P}"+args$(zi):print "struct: ";s$:p$=s$:gosub parse_declared_var 
 9694     if bl<>0 then s$=left$(sk$,bl-1)+"{CBM-P}"+args$(zi)+mid$(sk$,bl):print "struct: ";s$:p$=s$:gosub parse_declared_var :struct_fields$(sz)=p$:sz=sz+1
 9695     rem if next_line$<>"" then dl=0:current_line$="^^"+next_line$:else dl=1
 9696   bend
 9700 next zi
 9701 s$=next_line$:gosub safe_add_to_current_or_next_line :next_line$="":rem safe add l$+s$ to current dest_line$(ln)
 9702 gosub read_next_token :if s$<>"=" then current_line$="":return
 9703 gosub read_next_token :sz=0:sr=0:sm=0:rem read next token from current_line$ into s$
 9704 do while s$<>""
 9705     if s$="_" then src_lineno=src_lineno+1:gosub read_next_line :goto 9717:rem read next line
 9706   if sm=0 and s$<>"[" then print "error: expected [":sleep 1:stop
 9707   if sm=0 and s$="[" then sm=1:goto cfcoso_skip
 9708   if sm=1 and s$<>"[" and s$<>"]" then print "error: expected [ or ]":sleep 1:stop
 9709   if sm=2 then begin
 9710     if left$(s$,1)="]" then sr=sr+1:sz=0:sm=1:s$="":goto cfcoso_nextrow :rem next row
 9711 rem if left$(s$,1)=quote$ then ss$=s$:tr$="":do while right$(ss$,2)<>(quote$+","):gosub read_next_token :ss$=ss$+s$:print ss$:loop:s$=ss$:tr$=whitespace$:stop
 9712     if right$(s$,1)="," then s$=left$(s$,len(s$)-1)
 9713     s$=struct_fields$(sz)+"("+str$(sr)+")="+s$:gosub replace_vars_and_labels :gosub safe_add_to_current_or_next_line :s$="":sz=sz+1:rem safe add to dest_line$(ln)
.cfcoso_nextrow
 9714   bend
 9715   if sm=1 and s$="[" then sm=2:sz=0
 9716   if sm=1 and s$="]" then sm=0

.cfcoso_skip
 9717   gosub read_next_token :rem read next token from current_line$ into s$
 9720 loop
 9730 s$="":current_line$="":next_line$=""::zz$="z"
 9790 return

.safe_add_to_current_or_next_line
'--------------------------------
 9800 rem --- safe add l$+s$ to current orclosenext dest_line$(ln)
 9810 if len(l$)+len(s$)+len(str$(ln))>=159 then next_line_flag=1
 9815 if zz$<>"" then s$="":zz$="":return:rem force s$ to empty
 9820 if next_line_flag=1 then begin
 9830   dest_line$(ln)=l$:l$=s$
 9840   ln%(ln)=src_lineno:
 9850   ln=ln+1 : next_line_flag=0
 9860 bend : else begin rem -- add to l$
 9870   if l$<>"" and cn=0 and right$(l$,1)<>":" then l$=l$+":"
 9880   l$=l$+s$
 9890 bend
 9892 if vb then print "<<"ln;s$
 9895 return

.dump_vars
'---------
10000 rem --- dump vars
10010 for ty = 0 to 3
10020   for id = 0 to element_cnt(ty)
10030     gosub  generate_varname : rem get vn$ (optimised var name)
10040     print#1, str$(si)+" rem " + var_table$(ty,id) + " = :"+vn$+type_ident$(ty)+":"
10045     si=si+1
10050   next id
10060 next ty
10070 return

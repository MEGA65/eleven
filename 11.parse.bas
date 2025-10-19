    1 clr
    2 pn$="11.parse":mk$="@~":q$=chr$(34):h=34:y=39
    4 key on
    5 print "{rvon}ELEVEN preprocessor v0.5.0{rvof}":print
    6 key7,"scratch"+q$+pn$+q$+":dsave"+chr$(34)+pn$+q$+":dverify"+q$+pn$
    8 t$="                                                                               ":bl$=t$+t$+t$:t$=""
   10 rw$(0)=" print input if then else do loop while until gosub goto open close dopen dclose for next getkey hex$ dim peek poke wait dec chr$ asc sgn sqr str$"
   20 rw$(0)=rw$(0)+" graphic clr screen def begin bend len mid$ right$ left$ instr for next step trap border and foreground "
   30 rw$(1)=" background set abs sin cos tan log fre cursor pixel window rwindow line box circle ellipse palette restore data err$ er el cursor on off"
   35 rw$(1)=rw$(1)+" val scratch return rnd stop bank ti do or st if el er on to pen get end int not ds ds$ run using dot "
   70 rw$(2)=" append atn auto backup bload boot bsave bump bverify catalog change char cmd collision color concat cont copy wpoke wpeek setbit clrbit "
   71 rw$(3)=" dclear deffn delete fn dir disk dload dma dmode dpat dsave dverify edma envelope erase exit exp fast filter find go go64 header help highlight "
   72 rw$(4)=" hasbit joy let list load locate log10 log2 lpen mod monitor mouse movspr new paint play pointer polygon pos pot pudef "
   73 rw$(5)=" rcolor rdisk rdot read record rem rename resume rgr rmouse rplay rpt$ rreg rspcolor rsppos rsprite save scnclr sleep slow sound spc sprcolor "
   75 rw$(6)=" sprite sprsav sys tab tempo troff tron type usr verify vol xor key vsync rcursor t@& c@& rgraphic fread pointer chardef "
   85 dc$=" bload bsave dload to save dir collect dopen dclose backup fread get "
   90 gosub7020: rem get filename
  100 bank128:poke 0,65
  110 pf$(0)="":pf$(1)="%":pf$(2)="$":pf$(3)="&"
  111 dim b(16) : rem bit shifter's fast binary conversion. thanks a lot!
  112 b(0)=1:for i=1 to 16:b(i)=b(i-1)+b(i-1):next i
  115 dim ln%(1000)             : rem src file line nr <-> internal idx mapping
  120 dim li$(1000)             : rem post processed lines
  121 dim ec(4)                 : rem element count per type
  122 dim vt$(4,200)            : rem variable table per type
  123 dim df$(200)              : rem define values table
  125 dim lb$(200),ll(200):lc=0 : rem label table & count
  126 dim al$(32)               : rem argument list
  127 dim sn$(30),sk$(30)       : rem struct names, fields
  128 dim sv$(30)               : rem struct vars (each entry has string of vars)
  129 ss=0                      : rem index to next free struct definition
  130 :
  131 rem ------------------------- pass 1 ------------------------------------
  135 :
  138 nl = 0
  140 wh$=chr$(32)+chr$(160)+"{rght}{ensh}" : rem whitespace
  150 rem cleanup temporary files
  185 sl=0 : rem source code line counter
  190 print "pass 1 ";:rl=0
  195 clr ti: rem keep start time for timing
  198 cb=$8030000:ca=cb:tl=peek(ca)+256*peek(ca+1):ca=ca+2
  200 do while rl<>tl : rem until target lines is reached
  207   gosub 9500:rem read next line into cl$
  422   ct=instr(cl$,"'"): if ct=0 thengoto580
  423   if instr(cl$,chr$(34))=0 thengoto540
  424   ct=0
  425   bank0:l=peek(p)-1:a=peek(p+1)+256*peek(p+2):bank1
  440   forr=.tol:c=peek(a+r):ifc=hthenq=abs(q-1):elseifc=yandq=0thenct=r+1:r=999
  520   next
  540   if ct thencl$=left$(cl$,ct-1)
  560   rem strip whitespace from end
  580   s$=cl$:gosub2050:cl$=s$
  585   if cl$<>"" thenbegin
  586     dl=0 : rem delete line flag
  590     if vb thenprint ">>"ln;sl;cl$
  600     if left$(cl$,1)="." thennl=1:gosub1500: rem label
  601     if left$(cl$,1)="#" thenbegin
  602       cc=1
  603       if instr(cl$,"ifdef")=2 then s$=mid$(cl$,8):gosub 9210:dl=1
  604       if instr(cl$,"endif")=2 then sh=0:dl=1
  605       if instr(cl$,"define")=2 thendf=1:gosub1000:df=0
  606       if instr(cl$,"declare")=2 thendf=0:gosub1000
  607       if instr(cl$,"output")=2 thengosub1200
  608       if instr(cl$,"struct")=2 then gosub9300:dl=1
  650     bend
  653     if sh=1 then goto 750
  654     if left$(cl$,4)="data" or right$(cl$,5)="begin" then nl=1
  655     if dl=0 thenbegin
  656       if vb=0 thenprint ".";
  660       s$=cl$
  670       gosub3007:rem replace vars & labels in s$
  671       gosub9600:rem check for creation of struct object
  672       if right$(l$,1)="_" then l$=left$(l$,len(l$)-1):nl=0:cn=1:else cn=0
  675       gosub 9800:rem safe add l$+s$ to current or next li$(ln)
  732       if right$(s$,4)="bend" or right$(s$,6)="return" or left$(s$,2)="if" thennl=1
  735     bend : rem endif dl=0
  740   bend : rem endif c$<>""
  750   sl=sl+1 : rem increase source code line (for error msgs...)
  755   if vb then print "sl=";sl:getkey z$
  760 loop
  765 if l$<>"" thenli$(ln)=l$:ln=ln+1
  780 close 1
  782 gosub7150: rem set output filename
  785 scratch "11temp":scratch "11tokenized"
  786 rem ------------------------- pass 2 ------------------------------------
  787 :
  788 open 1,1,5,"11temp,s,w"
  790 print chr$(13)"{down}pass 2 ";
  795 lp=0   : rem current line pointer
  798 ol$="" : rem current optimized line
  800 for si=0 to ln-1
  810 s$=li$(si)  : if vb thenprint si;"{yel}=> "s$:elseprint ".";
  812   do while instr(s$,mk$)<>0
  814      s1=instr(s$,mk$):s2=instr(s$,mk$,s1+2)
  815      c$=mid$(s$,s1+2,s2-s1-2)
  820      gosub4505
  830      s$=left$(s$,s1-1)+c$+mid$(s$,s2+2)
  860   loop
  865 if vb thenprint "<= ";str$(si)+s$
  875   print#1,str$(si)+" "+s$
  876 next si
  880 gosub 10000:rem --- dump vars
  881 for r=0 to 10:print#1,str$(32000+r):nextr
  886 f$="dC:dS"+q$+"11tokenized"+q$+":ifds<>0then?"+q$+"disc error: "+q$+";ds$:else?"+q$+"{home}{home}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}{down}"+q$+":dL"+q$+"11.post"
  890 print#1,f$
  900 close 1
  905 print"{down}"
  910 et=ti-bt:print "elapsed time:";et;"ticks"
  920 print"{home}{home}{clr}{down}{down}edma 0,$1fff,$8022000,$2001:new restore{down}{down}":print"run{home}";:rem load '11.tokenize' from cache
  925 bank 128
  930 poke 208,2   : rem no of chars in keyboard buffer
  940 poke 688,13,13:rem return
  950 end
  999 :
 1000 rem declare var(s) in s$
 1010 s$=mid$(cl$,10-df):d$=",;":ib=1:gosub2100:ib=0: rem split parameters
 1012 rem (ib=ignore brackets)
 1015 nl$="" : rem new line if dimensioning...
 1020 if ac<0 then pe$="?declare parameter missing in line "+str$(sl):goto1800
 1021 for i=0 to ac
 1022   p$=al$(i):gosub 1030:rem parse declared var
 1023 next i
 1024 if nl$<>"" thendl=0:cl$="^^"+nl$:else dl=1
 1025 return
 1030 rem --- parse declared var
 1031 : di$="" : vl$=""
 1032 : b1=instr(p$,"("):b2=instr(p$,")"):eq=instr(p$,"=")
 1035 : if eq<>0 thenbegin : rem --- assignment
 1036 :   vl$=mid$(p$,eq+1):p$=left$(p$,eq-1):tr$=wh$:s$=p$:gosub2050:p$=s$
 1040 :   s$=vl$:gosub2000:gosub2050:vl$=s$
 1045 :   if left$(vl$,1)="$"thenhx$=mid$(vl$,2):gosub4900:vl$=c$
 1046 :   if left$(vl$,1)="%"thenbi$=mid$(vl$,2):gosub4800:vl$=c$
 1048 : bend
 1050 : if b1<>0 and b2<>0 thenbegin : rem --- dimension
 1052 :   di$=mid$(p$,b1+1,b2-b1-1) : pp$=left$(p$,b1-1)
 1054 :   s$=di$:dz=1:gosub 3000:dz=0:di$=s$:p$=pp$: rem check for define tokens
 1058 :   dl=0
 1060 : bend
 1062 : ty=0 : rem var type
 1064 : t$=right$(p$,1): rem type (if any) in t$
 1066 : if vb thenprint "adding {rvon}";
 1068 : if instr("%&$",t$)=0 thent$="":ty=0
 1069 : if df=1 thenty=4
 1070 : if t$="%" thenty=1
 1072 : if t$="$" thenty=2
 1073 : if t$="&" thenty=3
 1074 : vt$(ty,ec(ty)) = p$
 1076 : if di$<>"" thenbegin
 1078 :   id=ec(ty):gosub5000: rem fetch varname in vn$
 1084 :   if df=0 then nl$=nl$+"dim "+vn$+t$+"("+di$+"):"
 1085 : bend
 1088 : if vl$<>"" thenbegin
 1089 :   id=ec(ty):gosub5000
 1090 :   if df=0 then nl$=nl$+vn$+t$+"="+vl$+":"
 1092 : bend
 1093 : if df=1 then df$(ec(ty))=vl$
 1099 : if vb thenprint p$;"{rvof}: ";ec(ty)
 1100 : ec(ty)=ec(ty)+1
 1120 return
 1200 s$=mid$(cl$,8):d$=",;":gosub2100
 1210 if ac<>0 thenprint "?invalid parameters in line ";ln%(sl):end
 1220 s$=al$(0):tr$=q$:gosub2000:gosub2050: rem trim quotes left & right
 1230 if vb thenprint "setting output file to {rvon}"+s$+"{rvof}"
 1240 of$=s$
 1250 dl=1 : rem disable passthrough
 1260 return
 1485 :
 1490 rem add to label table
 1495 :
 1500 if vb thenprint "label ";cl$;" at line ";ln
 1505 dl=1
 1506 lb$(lc)=mid$(cl$,2)
 1510 ll(lc)=ln+1
 1520 lc=lc+1 : rem increase label count
 1530 return
 1800 rem return to editor with error
 1805 bank 4 : rem set error mailbox flag
 1807 for r=1 to len(pe$):poke $4ff30+r-1,asc(mid$(pe$,r,1)):next r
 1808 poke $4ff30+r-1,0
 1810 poke dec("ff09"),mod(sl,256):poke dec("ff0a"),sl/256
 1820 poke dec("ff07"),peek(dec("ff07"))or2 : rem set autojump flag
 1830 dclose
 1840 goto7210
 1999 :
 2000 rem -- strip tr$ from beginning of string in s$ --
 2010 do while instr(tr$,(left$(s$,1)))
 2020   s$=mid$(s$,2)
 2030 loop
 2040 return
 2045 :
 2050 rem -- strip charcters in tr$ from end of s$ --
 2060 do while instr(tr$,right$(s$,1))
 2070   s$=left$(s$,len(s$)-1)
 2080 loop
 2090 return
 2095 :
 2100 rem -- parse arguments --
 2101 rem     in: s$ = string, d$=delimiter
 2102 rem    out: al$(x)=argument list, ac=argument count
 2103 rem         al$(0)=first arg, al$(1)=second arg...
 2104 rem
 2109 :
 2110 ac=0:al$=s$:al=len(s$):ig=0
 2112 if al=0 thenac=-1:return : rem no string
 2115 forai=0to31:al$(ai)="":nextai
 2120 for ai=1 to al
 2125 b$=mid$(al$,ai,1)
 2126 if b$="(" and ib=1 thenig=1
 2127 if b$=")" and ib=1 thenig=0
 2130 if instr(d$,b$)<>0 and ig=0 thenbegin
 2132     s$=al$(ac):tr$=" ":gosub2000:gosub2050:al$(ac)=s$
 2140     ac=ac+1
 2150   bend : elsebegin
 2160     al$(ac)=al$(ac)+b$
 2170   bend
 2180 next ai
 2182 s$=al$(ac):tr$=" ":gosub2000:gosub2050:al$(ac)=s$
 2185 s$=al$ : rem restore s$
 2190 return
 2995 :
 3000 rem -- replace vars & labels in source string --
 3001 rem    in:   s$ = source string
 3002 rem    out:  s$ = dest string with replaced items
 3006 :
 3007 if left$(s$,2)="^^" thens$=right$(s$,len(s$)-2):return
 3010 q=0:a$="":c$="":ss=0:tg=0
 3012 cx$="?<>=+-#*/^,.:;() " : d$=cx$
 3020 for ii=1 to len(s$):b$=mid$(s$,ii,1)
 3025 if b$=":" and q=0 then ss=0:ri=0
 3030 if ss and b$="(" then ri=0 : d$=cx$
 3035 if ss and b$=")" then ri=-1 : d$=cx$ + "dpub"
 3040   if b$=q$ thenbegin : q=abs(q-1)
 3042   if q=1 thengosub4000:a$=a$+c$:c$="":elsea$=a$+b$:b$=""
 3044 bend
 3050 if q=1 thena$=a$+b$:goto3200
 3060 if instr(d$,b$)<>0 thenbegin
 3070 gosub4000:a$=a$+c$:c$="":ifb$=" "thenb$=""
 3080 a$=a$+b$
 3085 bend : elsec$=c$+b$
 3200 next
 3202 gosub4000
 3210 s$=a$+c$
 3220 return
 3900 end
 4000 if c$="" or c$="_" thenreturn
 4001 if val(c$)<>0 thentg=0: return     : rem never change numbers
 4002 if c$="0" thenc$=".":return        : rem stupid ms basic optimization
 4005 if tg and dz=0 thengosub4500:tg=0:return   : rem replace label
 4006 if c$="goto" thennl=1
 4007 if c$="goto" or c$="gosub" or c$="trap" or c$="restore" thentg=1
 4008 dr=0 : rem did replace flag
 4009 if left$(c$,1)="$" thenhx$=mid$(c$,2):gosub4900:return
 4010 if left$(c$,1)="%" thenbi$=mid$(c$,2):gosub4800:return
 4011 p$=" "+c$+" "
 4012 for t=0 to 6:if instr(rw$(t),p$)<>0 then lc$=c$:gosub 4100:return
 4013 next
 4014 t$=right$(c$,1):ty=0
 4015 if t$="%" thenty=1
 4016 if t$="$" thenty=2
 4017 if t$="&" thenty=3
 4020 for id=0 to ec(ty)
 4025 if c$=vt$(ty,id) thengosub5000:c$=vn$+pf$(ty):id=ec(ty):dr=1
 4030 next id
 4070 if dr=1 thenreturn
 4071 for id=0 to ec(4):rem check defines table too
 4072   if c$=vt$(4,id) then c$=df$(id):return
 4073 next id
 4074 ci=-1
 4075 for id=0 to ss-1:rem check struct names
 4076   if c$=sn$(id) then gosub 9600:ci=id:id=ss-1:rem create new struct object
 4077 next id
 4078 if ci<>-1 then return:else if asc(c$)=222 then return:rem pi
 4079 if instr(dc$, lc$)<>0 and (c$="r" or c$="p" or c$="u8" or c$="w" or c$="l" or c$="u") then return
 4080 pe$="?unresolved identifier: '"+c$+"' in line "+str$(sl):sleep 1:goto 1800
 4090 return
 4091 :
 4092 :
 4100 rem check if command triggers shitty syntax mode
 4110 rem todo: compare ubik's logic here versus my own 4079 lc$="dopen" logic
 4120 return
 4500 c$=mk$+c$+mk$ : return : rem mark label
 4505 dr=0
 4510 for id=0 to lc-1
 4520 if c$=lb$(id) thenc$=str$(ll(id)):id=lc:dr=1
 4530 next id
 4540 if dr thenreturn
 4550 pe$="?unresolved label: '"+c$+"' in line"+str$(ln%(si-1)):sleep 1:goto1800
 4567 return
 4800 rem --- convert binary
 4810 br=0 : rem result
 4820 for b=0 to len(bi$)-1
 4830 bc$=mid$(bi$,len(bi$)-b,1)
 4840 if bc$<>"1" and bc$<>"0" thenbank4:pokedec("ff08"),132:goto4082
 4850 if bc$="1" thenbr=br+b(b)
 4860 next b
 4870 c$=mid$(str$(br),2)
 4880 return
 4899 stop
 4900 rem --- convert hex
 4905 trap4940
 4910 vl=dec(hx$)
 4920 c$=mid$(str$(vl),2)
 4925 trap
 4930 return
 4940 trap:bank 4:poke dec("ff08"),131 : rem set illegal hex
 4950 goto4082: rem jump into error handler
 4990 :
 4995 rem generate varname from index
 4998 :
 5000 if id<26 thenvn$=chr$(65+id) : return
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
 7020 bank 4:ba=dec("ff00")
 7030 if peek(ba+0)=asc("s") and peek(ba+1)=asc("k") thenbegin
 7040   vb=peek(dec("ff07"))and16
 7050   f$="":a=ba+16:dowhilepeek(a)<>0:f$=f$+chr$(peek(a)):a=a+1:loop:
 7060   if peek(dec("ff07"))and1 thenreturn
 7070   print "filename? "+f$:print"{up}";
 7080 bend
 7090 input "filename";a$
 7100 if a$="" thenprint "no filename set":end
 7110 poke ba,asc("s"):poke ba+1,asc("k")
 7120 forr=1to16:poke ba+8+r-1,asc(mid$(a$,r,1)):nextr
 7130 f$=a$
 7140 return
 7150 rem --- save filename
 7160 ad=dec("ff30"):bank 4
 7170 forr=0to16:pokead+r,0:nextr
 7180 if of$<>"" thentf$=of$ : elsetf$="eleven.out"
 7190 forr=1tolen(tf$):pokead+r-1,asc(mid$(tf$,r,1)):nextr
 7200 return
 7210 rem chain editor
 7220 get a$:if a$<>"" then input zz:if zz=1 then adfsdf
 7225 print"{home}{home}{clr}{down}{down}edma 0,$d400,$8000000,$2001:new restore{down}{down}":print"run{home}";:rem load '11.edit' from cache
 7230 bank 128
 7240 poke 208,2     : rem no of chars in keyboard buffer
 7250 poke 688,13,13 : rem return
 7260 end
 9100 rem --- copy attic ram to line buffer
 9110 cb=$8030000 : c=cb
 9120 nl=peek(c)+256*peek(c+1):c=c+2
 9130 cl=0
 9140 do while cl<>nl
 9150   l=peek(c):li$(cl)=left$(bl$,l):p=pointer(li$(cl))
 9155   b=$10000+peek(p+1)+256*peek(p+2):c=c+1
 9165   if l<>0 then edma 0,l,c,b : c=c+l
 9180   cl=cl+1
 9190 loop
 9200 return
 9210 rem --- define in s$ exists?
 9220 sh = 1
 9230 fork=0toec(4)
 9240 if vt$(4,k) = s$ then sh=0
 9250 nextk
 9260 return
 9300 rem --- read in struct details
 9305 cl$=mid$(cl$,9):gosub 9400:rem get next token in s$
 9307 if s$="" then print "error: no struct name found":sleep 1:return
 9310 sn$(ss) = s$
 9320 sv$(ss) = cl$
 9325 ss=ss+1
 9330 return
 9400 rem --- read next token from cl$ into s$
 9410 s$=cl$:gosub 2000:gosub 2050:cl$=s$
 9411 sf$=" ":sf=0
 9412 if left$(s$,1)=q$ then sf$=q$+", ":sf=2
 9415 a=instr(cl$,sf$)
 9420 if a<>0 then s$=mid$(cl$,1,instr(cl$,sf$)+sf-1):cl$=mid$(cl$,instr(cl$,sf$)+sf+1)
 9430 if a=0 then s$=cl$:cl$=""
 9440 return
 9500 rem --- read next line
 9510 bank 0
 9520 l=peek(ca):cl$=left$(bl$,l):p=pointer(cl$):ca=ca+1
 9530 b=$10000+peek(p+1)+256*peek(p+2)
 9540 if l<>0 then edma 0,l,ca,b : ca=ca+l
 9550 rl=rl+1
 9560 tr$=wh$:s$=cl$:gosub2000:cl$=s$
 9570 q = 0 : rem quotes on
 9580 ct = 0 : rem cut chars from tail
 9590 bank 1
 9595 return
 9600 rem --- check for creation of struct object
 9610 co$=s$:cl$=s$:zz=-1:rem preserve original string
 9620 gosub 9400:rem read next token from cl$ into s$
 9630 for zi=0 to ss
 9640   if s$=sn$(zi) then zz=zi:zi=ss
 9650 next zi
 9660 if zz=-1 then cl$=co$:s$=co$:return
 9662 gosub 9400:sk$=s$:rem get struct object name
 9664 bl=instr(sk$,"(")
 9670 rem *** found it, so make dim's for each member var
 9680 s$=sv$(zz):gosub 2100:rem parse args into al$(), ac
 9685 zz=1:sz=0
 9690 for zi=0 to ac
 9691   if al$(zi)<>"" then begin
 9692     print "al$(";zi;")=";al$(zi)
 9693     if bl=0 then s$=sk$+"{CBM-P}"+al$(zi):print "struct: ";s$:p$=s$:gosub 1030
 9694     if bl<>0 then s$=left$(sk$,bl-1)+"{CBM-P}"+al$(zi)+mid$(sk$,bl):print "struct: ";s$:p$=s$:gosub 1030:sk$(sz)=p$:sz=sz+1
 9695     rem if nl$<>"" then dl=0:cl$="^^"+nl$:else dl=1
 9696   bend
 9700 next zi
 9701 s$=nl$:gosub 9800:nl$="":rem safe add l$+s$ to current li$(ln)
 9702 gosub 9400:if s$<>"=" then cl$="":return
 9703 gosub 9400:sz=0:sr=0:sm=0:rem read next token from cl$ into s$
 9704 do while s$<>""
 9705     if s$="_" then sl=sl+1:gosub 9500:goto 9717:rem read next line
 9706   if sm=0 and s$<>"[" then print "error: expected [":sleep 1:stop
 9707   if sm=0 and s$="[" then sm=1:goto 9717
 9708   if sm=1 and s$<>"[" and s$<>"]" then print "error: expected [ or ]":sleep 1:stop
 9709   if sm=2 then begin
 9710     if left$(s$,1)="]" then sr=sr+1:sz=0:sm=1:s$="":goto 9714:rem next row
 9711 rem if left$(s$,1)=q$ then ss$=s$:tr$="":do while right$(ss$,2)<>(q$+","):gosub9400:ss$=ss$+s$:printss$:loop:s$=ss$:tr$=wh$:stop
 9712     if right$(s$,1)="," then s$=left$(s$,len(s$)-1)
 9713     s$=sk$(sz)+"("+str$(sr)+")="+s$:gosub 3007:gosub 9800:s$="":sz=sz+1:rem safe add to li$(ln)
 9714   bend
 9715   if sm=1 and s$="[" then sm=2:sz=0
 9716   if sm=1 and s$="]" then sm=0
 9717   gosub 9400:rem read next token from cl$ into s$
 9720 loop
 9730 s$="":cl$="":nl$=""::zz$="z"
 9790 return
 9800 rem --- safe add l$+s$ to current orclosenext li$(ln)
 9810 if len(l$)+len(s$)+len(str$(ln))>=159 thennl=1
 9815 if zz$<>"" then s$="":zz$="":return:rem force s$ to empty
 9820 if nl=1 thenbegin
 9830   li$(ln)=l$:l$=s$
 9840   ln%(ln)=sl:
 9850   ln=ln+1 : nl=0
 9860 bend : elsebegin rem -- add to l$
 9870   if l$<>"" and cn=0 and right$(l$,1)<>":" thenl$=l$+":"
 9880   l$=l$+s$
 9890 bend
 9892 if vb then print "<<"ln;s$
 9895 return
10000 rem --- dump vars
10010 for ty = 0 to 3
10020   for id = 0 to ec(ty)
10030     gosub 5000: rem get vn$ (optimised var name)
10040     print#1, str$(si)+" rem " + vt$(ty,id) + " = :"+vn$+pf$(ty)+":"
10045     si=si+1
10050   next id
10060 next ty
10070 for id = 0 to lc
10080   print#1, str$(si) + " rem l"+mid$(str$(ll(id)),2) + " = ." + lb$(id)
10085   si=si+1
10090 next id
10100 return

    0 goto130
    1 a$=mid$(a$,xi):b=len(a$):o=b>80:print"{rvof}";:ifothenb=79
    2 p=pointer(a$):bank0:m=peek(p+1)+256*peek(p+2):c=m+b-1:ifb=0then6
    3 if iv=1 then print "{rvon}";
    4 bank1:fora=mtoc:printa$(peek(a));:next
    5 bank128:ifothenforeground hl:printtab(79);"$";:foreground fg
    6 return
  100 :
  110 rem --- general initializations ---
  120 :
  130 trap8560
  140 poke 0,65             : rem m65 fast mode
  150 gosub7940: rem install m/l
  160 pf$="11.edit"
  170 key 7,"scratch "+chr$(34)+pf$+chr$(34)+":dsave"+chr$(34)+pf$+chr$(34)+":dverify"+chr$(34)+pf$+chr$(34)
  180 if rwindow(2)<>80 thenprint chr$(27)+"x";:rem force 80cols mode
  190 gosub7040:rem read defaults
  200 background bg: border bo: foreground fg : print"{home}{home}{clr}{swlc}"+chr$(27)+"m";
  210 gosub5490:print"{rvon} ee v0.4.6{rvof}";
  220 key off               : rem enable raw fn keys
  230 :
  240 mx=2000               : rem max number of lines
  242 es%=0                 : rem escape flag
  245 t$="                                                                               ":bl$=t$+t$+t$:t$=""
  250 q$=chr$(34)           : rem fast access to quote char
  260 fo$=chr$(27)+chr$(27) : rem escape flash etc.
  270 cc$="{home}{clr}{f2}{$84}{left}{up}{down}{rght}{f1}{f3}{f5}{f7}{f4}{CTRL-Z}{CTRL-P}{$83}{CTRL-W}{CTRL-U}{inst}{blu}"+chr$(22)+chr$(20)+chr$(148)+chr$(13) : rem ctrl chars
  280 hm$="{left}{rght}"+chr$(20)     : rem horizontal movement characters
  290 ch$(0)=" ":ch$(1)="*" : rem for file changed indicator
  300 cm$(0)="    ":        : rem for control char mode indicator
  310 cm$(1)="{CTRL-O}ctrl"+fo$
  320 r$(1)="success: ":r$(0)="error: " : rem disc operation result
  330 co(1)=sb:co(0)=hl                 : rem disc operation result colors
  340 df$="11.defaults"                 : rem defaults file name
  350 dim li$(mx)           : rem line buffer
  355 dim cb$(50)          : rem clipboard of lines
  360 dim mk$(72),mk(72)   : rem marker table
  370 ee$(0)="variable not declared"
  380 ee$(1)="variable missing"
  390 ee$(2)="unresolved reference"
  400 ee$(3)="illegal hex number"
  410 ee$(4)="illegal binary number"
  420 :
  430 rem --- build char translation table ---
  440 :
  450 dim a$(255)           : rem char translation table
  460 fora=1to31:a$(a)="{rvon}"+chr$(64+a)+"{rvof}":a$(128+a)="{rvon}"+chr$(192+a)+"{rvof}":nexta
  470 fora=32to127:a$(a)=chr$(a):a$(a+128)=chr$(a+128):nexta
  480 a$(34)=chr$(34)+fo$
  490 a$(160)="{CBM-+}" : rem make shift+space visible
  500 :
  510 :
  520 xi=1 : rem horizontal scroll index in current line
  530 bank 128
  540 poke 248,peek(248)or2^6    : rem disable screen editor line linking
  550 xc=0:yc=0:sl=23 : rem cursor pos, number of screen lines
  560 if cf$<>"" thenbegin
  565   if peek($4ff70)=55 and peek($4ff71)=77 then begin
  566     gosub 9100
  567   bend:else begin
  570     gosub5010: rem load file if set
  572     gosub 9000: rem keep copy of file in attic ram
  575     poke $4ff70,55,77
  580     if de<>0 thencf$="" : rem delete current filename if file not there
  585   bend
  590 bend
  600 :
  610 rem uncomment this for output performance test
  620 rem ts=ti:forte=0to10:gosub2080:nextte:print"{down}{down}{down}"ti-ts:stop
  630 :
  640 gosub5150: rem redraw screen
  650 gosub5540: rem build status bar
 2000 :
 2010 rem **********************
 2020 rem ** main editor loop **
 2030 rem **********************
 2040 :
 2050 bank128:sysv4 : rem enable vic iv
 2060 if jl<>-1 thenbegin
 2070   ly=jl:cl=5:gosub6600: rem bring line in context
 2080   if ee<>0 thenbegin
 2090     e$=""
 2100     if ee>=128 thene$=ee$(ee-128) : elsee$=er$(ee)
 2110     if e$<>"" thengosub5490:print"{rvon}";e$+" in "+str$(jl);"{rvof}";:ns=1
 2120   bend
 2130   jy=-1:cl=0             : rem clear jump line
 2140 bend
 2150 do
 2160   if yc-ct<0 thenct=0:yc=0
 2170   if ns<>1 thenpo$="      {left}{left}{left}{left}{left}{left}"+mid$(str$(xc),2)+":"+mid$(str$(yc),2):gosub5400
 2180   cl$=li$(yc)
 2185   if (yc-ct)>24 then yc=ct+24
 2190   cursor xc,yc-ct
 2200   foreground sb:cursor on: getkey t$ : cursor off:foreground fg
 2201   if mf=1 then gosub 10050:goto 2160:rem mark-mode handling
 2210   if ns=1 thengosub5540: bank128:sysv4: ns=0
 2220   bank 128:ak = peek(54801) and 16 : rem check alt key
 2230   if t$=chr$(22) or (ak and t$="c") thencm=abs(cm-1) : goto2170
 2240     co=(instr(cc$,t$) and cm=0) or ak
 2250    if co thenbegin
 2260     if t$="g" thent$="{CTRL-P}" : rem workaround for challenged keyboards
 2270     if t$="s" thent$="{CTRL-Z}"
 2280     if t$=chr$(13) thench=1: gosub5650: rem insert return
 2290     if instr(hm$,t$)=0 and xi>1 thenbegin
 2300        xi=1:cursor 0,yc-ct:cl$=li$(yc):a$=cl$:gosub1
 2310        if t$=chr$(13) thencursor 0,yc-ct-1:a$=li$(yc-1):gosub1
 2320     bend
 2321     if t$="{CTRL-W}" then gosub 9510:rem ctrl-w = next word
 2322     if t$="{CTRL-U}" then gosub 9700:rem ctrl-u = previous word
 2323     if t$="{inst}" then gosub 9900:rem shift-del = delete current char
 2324     if t$="{blu}" then poke $4ff07,peek($4ff07) xor 8:if peek($4ff07)and8 then play "t0o5sg"
 2325     if t$="p" thengosub9300:rem post current file to pc
 2330     if t$="f" thengosub8000:fr%=0: rem find
 2340     if t$="r" thengosub8000:fr%=1: rem find and replace
 2350     if t$="{f3}" thengosub5990: rem save
 2360     if t$="{f4}" thensa=1:gosub5990: rem save as
 2370     if t$="{f1}" thengosub6260: rem load
 2380     if t$="{$83}" thengosub7360: rem stop
 2390     if t$="{f2}" thengosub7230: rem new
 2400     if t$="{f7}" thengosub7430: rem labels
 2410     if t$="{CTRL-P}" thengosub6710: rem goto line
 2420     if t$="{$84}" thengosub6790: rem help
 2430     if t$="{home}" thenbegin           : rem home/end
 2440       if ak=0 thenly=0:gosub6600: elsely=nl:gosub6600
 2450     bend
 2460     if t$="{rght}" thenbegin : rem -------- cursor right
 2470       if ak thenbegin
 2480          if 79+xi<len(cl$) thenxi=xi+1:cursor0,yc-ct:a$=cl$:gosub1
 2490       bend : elsexc=xc+1
 2500       if xc>79 thenbegin
 2510         xc=79:if xc+xi<len(cl$) thenxi=xi+1
 2520         cursor 0,yc-ct:a$=cl$:gosub1
 2530       bend
 2540     bend
 2550     if t$="{left}" thenbegin
 2560       if xc>0 thenxc=xc-1 : elseif yc>0 thenyc=yc-1:xc=len(li$(yc)):if xc>79 thenxc=79
 2570     bend
 2580     if t$="{down}" thenbegin
 2590       if ak thenly=yc+sl:gosub6600:elseyc=yc+1
 2600       if yc-ct>20 thengosub5250: if yc-ct>sl thenyc=yc-1
 2610     bend
 2620     if t$="{up}" thenbegin
 2630       if ak thenly=yc-sl:gosub6600:elseyc=yc-1
 2640       if yc-ct<0 thengosub5330: if yc-ct<0 thenyc=yc+1
 2650     bend
 2660     if t$="{f5}" thengosub7790: rem --- f5 - compile
 2670     if t$=chr$(20) thenbegin:rem delete
 2680       ch=1
 2690       if xc=0 or ak thengosub5840:elsebegin : rem merge with last line
 2700       cl$=left$(cl$,xc+xi-2)+mid$(cl$,xc+xi)
 2710       li$(yc)=cl$
 2720       cursor 0,yc-ct:a$=cl$:gosub1:iflen(cl$)<=79thenprint" ";
 2730       if xc>0 thenxc=xc-1
 2740     bend
 2750   bend
 2760   if t$="{CTRL-Z}" thenbegin : rem --- f14 - save defaults
 2770     gosub5490:print"{rvon}saving defaults...{rvof}";
 2780     scratch (df$):bsave (df$),b(4),p(dec("ff00")) to p(dec("ff80"))
 2785     de=ds : de$=ds$
 2790     foreground co(abs(de=0))
 2800     gosub5490:print "{rvon}";r$(abs(de=0));de$;" ";:foreground sb
 2810     ns=1
 2820   bend
 2830 bend:else begin:if t$=chr$(27) then begin: rem -- check escape flag
 2831   t$="":if es%=0 then es%=1:else es%=0
 2832 bend:else begin:if es%=1 then begin : rem -- check escape characters
 2833   if t$="k" then xc=len(cl$):if xc>78 then xi=xc-78:xc=78:gosub 9970
 2834   if t$="j" then xc=0:if xi>1 then xi=1:gosub 9970
 2835   if t$="d" then ak=1:gosub 5840:ch=1
 2836   if t$="m" then mf=1:ms=yc:me=yc:gosub 9970
 2837   if t$="p" then gosub 10200
 2838   es%=0
 2839 bend:elsebegin : rem -- insert char
 2840     ch=1
 2850     if xc=79 thenxi=xi+1
 2860     do while len(cl$)<xc:cl$=cl$+" ":loop: rem fill up spaces if needed
 2870     cl$=left$(cl$,xc+xi-1)+t$+mid$(cl$,xc+xi)
 2880     li$(yc)=cl$
 2890     cursor 0,yc-ct:a$=cl$:gosub1
 2900     xc=xc+1 : if xc>79 thenxc=79
 2910   bend:rem endif es%=1
 2912   bend:rem endif t$=chr$(27)
 2914   bend:rem endif co (ctrl-char)
 2920 loop until qu=1
 2930 stop
 2940 :
 5000 rem ==================== subroutines =====================
 5010 rem --- read in f$
 5020 open 1,1,5,cf$+",s,r":de=ds:de$=ds$
 5030 if de<>0 thencf$=of$:close 1:return
 5040 fora=0tomx:li$(a)="":nexta:lc=0
 5050 nl=0:ct=0:yc=0:xc=0
 5060 do
 5070   l$="":line input#1,l$
 5080   li$(nl)=l$
 5090   nl=nl+1
 5100 loop while st=0
 5110 nl=nl-1:ch=0
 5120 close 1
 5130 return
 5140 :
 5150 rem ---  redraw screen
 5160 foreground fg : print "{rvof}";
 5170 if ct<0 thenct=0
 5180 for i=0 to sl
 5190   cursor 0,i
 5200   a$=li$(ct+i)
 5210   print chr$(27)+"q";:gosub10000:gosub1
 5220 next i
 5230 return
 5240 :
 5250 rem --- scroll up
 5260 if ct+sl>nl-1 thenreturn
 5270 ct=ct+1 : rem current top
 5280 s$=li$(ct+sl)
 5290 cursor 0,0:print chr$(27)+"v";
 5300 cursor 0,sl:a$=s$:gosub1
 5310 return
 5320 :
 5330 rem --- scroll down
 5340 if ct<=0 thenreturn
 5350 ct=ct-1
 5360 s$=li$(ct)
 5370 cursor0,0:print chr$(27)+"w";:a$=s$:gosub1
 5380 return
 5390 :
 5400 rem --- display status in st$
 5410 foreground sb
 5420 print"{home}{home}";:cursor 20,24:print"{rvon}"+po$;
 5430 cursor 75,24:printcm$(cm);
 5440 cursor 0,24:print"{rvon}"+ch$(ch)+"{rvof}";
 5450 window 0,0,79,sl
 5460 foreground fg
 5470 return
 5480 :
 5490 dma 3,80,160,0,3968,0 : rem  --- clear status bar
 5500 dma 3,80,sb,0,65408,1
 5510 print"{home}{home}";:cursor 0,24
 5520 foreground sb
 5530 return
 5540 rem --- clear status
 5550 dma 3,80,160,0,3968,0
 5560 dma 3,80,sb,0,65408,1
 5570 foreground sb
 5580 print"{home}{home}";:cursor 18,24:print"{rvon}{SHIFT--}       {SHIFT--} {rvof}f1{rvon}load {rvof}f3{rvon}save {rvof}f5{rvon}compile {rvof}f7{rvon}lbls {rvof}f9{rvon}go {rvof}f11{rvon}cmode {SHIFT--}";
 5590 cursor 2,24:if cf$<>"" thenprint cf$;:elseprint "<untitled>";
 5600 window 0,0,79,sl
 5610 foreground fg
 5620 return
 5630 :
 5640 rem --- insert return
 5650 yc=yc+1
 5660 nl=nl+1
 5670 if yc>nl thennl=yc
 5680 fora=nl+1toyc+1step-1:li$(a)=li$(a-1):nexta:li$(yc)=""
 5690 li$(yc)=mid$(li$(yc-1),xc+xi)
 5700 li$(yc-1)=left$(li$(yc-1),xc+xi-1)
 5710 xc=0:sc=1:pl$=li$(yc-1)
 5720 do while mid$(pl$,sc,1)=" " : rem --- autoindent
 5730   if li$(yc)<>"" thenli$(yc)=" "+li$(yc)
 5740   sc=sc+1:xc=xc+1
 5750 loop
 5760 if yc-ct>sl thengosub5250: rem scroll up
 5770 cursor xc,yc-ct:print chr$(27)+"i";
 5780 a$=li$(yc):gosub1
 5790 cursor xc-sc+1,yc-ct-1:print chr$(27)+"q";
 5800 a$=li$(yc-1):gosub1
 5810 return
 5820 :
 5830 rem --- merge lines
 5840 if ak=0 and yc<1 thenreturn
 5850 if ak thensx=xc:xc=0:yc=yc-1:goto 5880
 5860 yc=yc-1:xc=len(li$(yc))
 5870 li$(yc)=li$(yc)+li$(yc+1)
 5880 fora=yc+1tonl-1:li$(a)=li$(a+1):nexta
 5890 if yc<=nl thenli$(nl)=""
 5900 if nl>0 thennl=nl+1
 5910 :
 5920 if ak=0 then cursor 0,yc-ct:a$=li$(yc):gosub1
 5930 cursor 0,yc-ct+1:printchr$(27)+"d";
 5940 cursor 0,sl:a$=li$(ct+sl):gosub1
 5950 if ct+sl>nl-1 thencursor 0,23:print chr$(27)+"q";
 5960 if ak thenxc=sx:yc=yc+1
 5970 return
 5980 :
 5990 rem --- save file
 6000 if bu and cf$<>"" and sa=0 thenbegin
 6010   gosub5490:print"{rvon}backing up old file...";
 6020   b$=left$(cf$,12)+".bak"
 6030   delete (b$)
 6040   copy (cf$) to (b$)
 6050 bend
 6060 if cf$="" or sa=1 thensa=0 : gosub6520: rem save as
 6070 if cf$="" thengosub5540: return
 6080 gosub5490:print"{rvon}saving...";
 6090 delete (cf$)
 6100 i=mx:do while li$(i)="":i=i-1:loop
 6110 nl=i
 6120 open 1,1,3,cf$+",s,w":de=ds:de$=ds$
 6130 for a=0 to nl
 6140   print#1,li$(a)
 6150 next a
 6155 if ds<>0 then de=ds
 6160 close 1
 6170 gosub5490
 6180 if de=0 thench=0
 6190 foreground co(abs(de=0))
 6200 print r$(abs(de=0));de$;" ";:foreground sb
 6210 gosub7720: rem save filename in mailbox ram
 6220 ns=1
 6230 return
 6240 :
 6250 :
 6260 rem --- load file
 6270 gosub5490
 6280 of$=cf$
 6290 if ch=1 thenbegin
 6300   print "{rvon}current file "+cf$+" not saved. really proceed? (y/n) {CTRL-O}{CBM-+}"+fo$;
 6310   do:getkey t$:loop until instr("yn",t$)
 6320   gosub5490
 6330   if t$="n" thengosub5540:return
 6340 bend
 6350 gosub5490:print "{rvon}load file name ($ for directory, return to cancel) "+chr$(27)+"t{clr}";
 6360 nf$=""
 6370 line input "";nf$
 6380 if nf$="$" thenwindow0,0,79,23:print"{clr}";:foreground fg:dir:goto6350
 6390 print"{home}{home}";:gosub5490
 6400 if nf$="" thengosub5540:gosub5150:return
 6410 cf$=nf$:print "{rvon}loading "+cf$+"...";
 6420 gosub5010
 6430 print"{home}{home}{clr}"+fo$;
 6440 gosub5150: rem redraw screen
 6450 if de=0 thengosub7720: rem persist filename
 6460 gosub5490:foreground co(abs(de=0))
 6470 print "{rvon}";r$(abs(de=0));de$;" ";:foreground sb
 6480 if de=0 thenprint"{rvon}; read ";nl; "lines.";
 6490 ns=1
 6500 return
 6510 :
 6520 rem --- query save file name
 6530 gosub5490:print "{rvon}save file name (return to cancel) "+chr$(27)+"t{clr}";
 6540 nf$=""
 6550 line input "";nf$
 6560 print"{home}{home}";:gosub5490
 6570 cf$=nf$
 6580 return
 6590 :
 6600 rem --- make line ly the current line
 6610 ct=ly : yc=ly
 6620 if ct>cl thenct=ct-cl
 6630 goto6690
 6640 if ct<0 thenct=0
 6650 if yc<0 thenyc=0
 6660 if yc>nl thenyc=nl : ct=yc-sl+3
 6670 if ct>cl thenct=ct-cl : rem context lines
 6680 if ct<0 thenct=0
 6690 gosub5150
 6700 return
 6710 rem --- query line no
 6720 gosub5490:print "{rvon}jump to line # (return to cancel) "+chr$(27)+"t{clr}";
 6730 nn$="":line input nn$
 6740 nn=val(nn$):print"{home}{home}";:gosub5490
 6750 if nn>nl thenreturn
 6760 ly=nn:cl=8:gosub6600
 6770 gosub5540
 6780 return
 6790 rem --- help
 6800 print"{home}{home}{clr}-- cursor movement --",,"-- editor functions --{down}"
 6810 print"{rvon}cursor keys  {rvof} move the cursor"
 6820 print"{rvon}alt + up     {rvof} scroll up 1 line"
 6830 print"{rvon}alt + down   {rvof} scroll down 1 line"
 6840 print"{rvon}alt + left {$a0} {rvof} scroll left 1 char(*)"
 6850 print"{rvon}alt + right  {rvof} scroll right 1 char(*)"
 6860 print"{down}{rvon}shift + clr  {rvof} go to top of file"
 6870 print"{rvon}alt + clr    {rvof} go to end of file"
 6880 print"{down}{rvon}alt + delete {rvof} delete current line"
 6890 print"{down}{rvon}alt + return {rvof} to start of next line"
 6900 print"{down}*) only in lines with >80 characters"
 6910 print"{home}{down}{down}",,,,"{rvon} f1 {rvof} load file"
 6920 print,,,,"{rvon} f2 {rvof} new file"
 6930 print,,,,"{rvon} f3 {rvof} save file"
 6940 print,,,,"{rvon} f4 {rvof} save file as"
 6950 print,,,,"{down}{rvon} f5 {rvof} compile + run"
 6960 print,,,,"{rvon} f6 {rvof} just compile"
 6970 print,,,,"{down}{rvon} f7 {rvof} go to label"
 6980 print,,,,"{rvon} f9 {rvof} go to line nr"
 6990 print,,,,"{down}{rvon}f11 {rvof} toggle ctrl mode"
 7000 print,,,,"{down}{rvon}f14 {rvof} update defaults"
 7010 print,,,,"{down}{rvon}help{rvof} this screen"
 7020 gosub5490:print"{rvon}ee cheat sheet -- press any key to return to editor{rvof}";
 7030 getkey t$:gosub5150:gosub5540:return
 7040 rem --- set filename from mailbox ram
 7050 bank 4:t$=chr$(peek(dec("ff00")))+chr$(peek(dec("ff01")))
 7060 if t$<>"sk" thenbo=6:fg=5:bg=0:sb=14:hl=2:jl=-1:return
 7070 jl=-1 : rem jump line
 7080 al=peek(dec("ff07"))and1 : rem autoload flag?
 7090 if al=0 thengoto7170
 7100 tf$="":i=dec("ff10"):do while peek(i)<>0
 7110 tf$=tf$+chr$(peek(i)):i=i+1:loop
 7120 cf$=tf$
 7130 if peek(dec("ff07"))and2 thenbegin : rem autojump flag
 7140 poke dec("ff07"),peek(dec("ff07"))and(255 xor 2)
 7150 jl=peek(dec("ff09"))+256*peek(dec("ff0a")):ee=peek(dec("ff08"))
 7160 bend
 7170 bo=peek(dec("ff02")):bg=peek(dec("ff03")):fg=peek(dec("ff04"))
 7180 hl=peek(dec("ff05")):sb=peek(dec("ff06"))
 7190 bu=peek(dec("ff07"))and4 : rem backup flag
 7200 bank 128
 7210 return
 7220 :
 7230 rem ------ new
 7240 if ch thenbegin
 7250 :  gosub5490:print"{rvon}Current file was modified! Really start over (y/n)? {CTRL-O}{CBM-+}";
 7260 :  do:getkey t$:loop until t$="y" or t$="n"
 7270 :  if t$="n" thengosub5540:    return
 7280 bend
 7290 fora=0tomx:li$(a)="":nexta:lc=0
 7300 cf$=""
 7310 print"{home}{home}{clr}":gosub5150
 7320 gosub5540
 7330 ch=0:xc=0:yc=0:ct=0
 7340 gosub5490:print"{rvon}** New file **{rvof}";:ns=1
 7350 return
 7360 rem ------ new
 7370 gosub5490:print"{rvon}really quit ee? {CTRL-O}{CBM-+}"+chr$(27)+"o";
 7380 do:getkey t$:loop until t$="y" or t$="n"
 7390 if t$="n" thengosub5540:return
 7400 bank 128:poke248,0:key on:palette restore
 7410 print"{home}{home}{clr}tschuessn...!"
 7420 end
 7430 rem --- labels
 7440 bank 1
 7450 gosub5490:print"{rvon}Collecting labels...";
 7460 fora=0to72:mk$(a)="":mk(a)=0:nexta:mn=0
 7470 fora=0tonl:t$=li$(a):lb$="":ifleft$(t$,1)<>"."then7490
 7480 mk$(mn)=mid$(t$,1,38):mk(mn)=a:mn=mn+1
 7490 next
 7500 gosub5490
 7510 if mn<>0 thenprint"{rvon}Label list. Choose label with cursor keys + RETURN, escape with ESC{rvof}";
 7520 if mn=0 thenprint"{rvon}{CTRL-O}No labels found{rvof}"+fo$;:ns=1:return
 7530 window 0,0,79,23:print"{clr}"
 7540 fora=0tomn
 7550 cursor 40*int(a/24),mod(a,24):print mk$(a);:next a
 7560 mp=0
 7570 do
 7580   sx=40*int(mp/24): sy=mod(mp,24):cursor 0,10
 7590   cursor sx,sy:print "{rvon}";mk$(mp);"{rvof}";
 7600   getkey t$
 7610   cursor sx,sy:print mk$(mp);
 7620   if t$="{down}" thenmp=mp+1
 7630   if t$="{rght}" thenmp=mp+24
 7640  if t$="{up}" thenmp=mp-1
 7650  if t$="{left}" thenmp=mp-24
 7660  if mp>mn-1 thenmp=mn-1
 7670   if mp<0 thenmp=0
 7680 loop until t$=chr$(13) or t$=chr$(27)
 7690 if t$=chr$(13) thenly=mk(mp):gosub6600:elsegosub5150
 7700 gosub5540: rem redraw status bar
 7710 return
 7720 rem --- save filename in mailbox ram
 7730 gosub 9000 : rem save to attic
 7740 for i=1 to len(cf$):poke $4ff10+i-1,asc(mid$(cf$,i,1)):next i
 7750 poke $4ff10+i-1,0
 7760 poke $4ff00,asc("s"),asc("k")
 7770 poke $4ff07,peek($4ff07)or1
 7780 bank128:return
 7790 rem --- compile
 7800 ab=0
 7810 if cf$="" thenreturn : rem now filename, no cigar
 7820 if ch=1 thenbegin
 7830   gosub5490: foreground hl
 7840   print "{rvon}current file not saved - press any key";:getkey t$
 7850   foreground fg: gosub5540: ab=1
 7860 bend
 7870 if ab=1 thenreturn
 7880 print"{home}{home}{clr}"+chr$(27)+"l";
 7885 rem gosub9000 : rem save line buffer
 7890 print"{home}{home}{clr}{down}{down}edma 0,$3fff,$8005000,$2001:new restore{down}{down}":print"run{home}";
 7900 bank 128
 7910 poke 208,2     : rem no of chars in keyboard buffer
 7920 poke 688,13,13 : rem 2x return
 7930 end
 7940 rem --- m/l helper for vic-iv mode
 7950 bank128:restore7970
 7960 for i=0 to 12:read a$:poke dec("1600")+i,dec(a$):next i
 7970 data 78,a2,47,a0,53,8e,2f,d0,8c,2f,d0,58,60
 7980 v4=dec("1600")
 7990 return
 8000 rem ================
 8010 rem find and replace
 8020 rem ================
 8030 :
 8040 gosub5490:print "{rvon}find:"+chr$(27)+"t{clr}";
 8050 fs$="":rs$="":sd=1 : rem find string, replace string, search dir
 8060 ra%=0 : rem replace all
 8070 open 1,0,0:line input#1,fs$:close 1
 8080 if fr%=0 then8110
 8090   print"{home}{home}";:gosub5490:print"{rvon}replace:"+chr$(27)+"t{clr}";
 8100   open 1,0,0:line input#1,rs$:close 1
 8110 print"{home}{home}";:gosub5490
 8120 if fs$="" or fs$=" " thenprint"{rvon}find cancelled.";:ns=1:return
 8130 s=yc
 8140 gosub5490:print"searching...";
 8150 do while s>=0 and s<=nl
 8160   fp=instr(li$(s),fs$) : rem found pos
 8170   if fp=0 then8240
 8180      ly=s:cl=8: gosub6600
 8190      bank 128
 8195      rem mark found line
 8200      for rv=0 to 79
 8205        i=((yc-ct)*80)+rv
 8210        poke 2048+i,peek(2048+i) xor 128
 8220      nextrv
 8222      rem flash found string
 8225      for rv=0 to len(fs$)-1
 8226        i=((yc-ct)*80)+fp+rv-1
 8227        poke $d800+i,peek($d800+i) or 16
 8228      next rv
 8230      if fr%=0 thengosub8310: elsegosub8390
 8240   s=s+sd
 8250 loop
 8260 if fd$="q" thengosub5540: elsegosub5490:print"{rvon}end of found";:ns=1
 8270 cursor xc,yc-ct:print chr$(27)+"q";
 8280 foreground fg
 8290 a$=li$(yc):gosub1: rem redraw current
 8300 return
 8310 rem --- find next
 8320 bank 128
 8330 gosub5490:print "{rvon}find n)ext p)rev q)uit";
 8340 do:getkey fd$:loop until instr("npq",fd$)
 8350 if fd$="n" thensd=1
 8360 if fd$="p" thensd=-1
 8370 if fd$="q" thens=nl+1
 8380 return
 8390 rem --- replace
 8400 bank 128
 8410 gosub5490:print "{rvon}r)eplace a)ll n)ext p)rev q)uit";
 8420 do:getkey fd$:loop until instr("ranpq",fd$)
 8430 if fd$="n" thensd=1
 8440 if fd$="p" thensd=-1
 8450 if fd$="q" thens=nl+1
 8460 if fd$="r" thengosub8480
 8470 return
 8480 ts$=li$(s)
 8490 ns$=left$(ts$,fp-1)+rs$+mid$(ts$,fp+len(fs$),255)
 8500 li$(s)=ns$
 8510 cursor xc,yc-ct:print chr$(27)+"q";
 8520 a$=li$(s):gosub1
 8530 s=s-1
 8540 return
 8550 return
 8560 rem --- error handler
 8570 if er=30 thenresume
 8580 trap
 8590 bank 128:poke248,0:key on:palette restore
 8600 print chr$(27)+"q"+chr$(27)+"l{home}{home}{lgrn}";err$(er),el
 8610 end
 9000 rem --- copy line buffer to attic ram
 9020 cb=$8010000 : c=cb
 9025 wpoke c,nl+1 : c=c+2 : rem store no of lines
 9030 for a=0 to nl
 9040   p=pointer(li$(a)):l=len(li$(a)):b=$10000+wpeek(p+1)
 9050   poke c,l : c=c+1
 9060   if l<>0 thenedma 0,l,b,c:c=c+l
 9070 next
 9080 return
 9100 rem --- copy attic ram to line buffer
 9110 cb=$8010000 : c=cb
 9120 nl=wpeek(c):c=c+2
 9130 cl=0
 9140 do while cl<>nl
 9150   l=peek(c):li$(cl)=left$(bl$,l):p=pointer(li$(cl))
 9155   b=$10000+wpeek(p+1):c=c+1
 9165   if l<>0 then edma 0,l,c,b : c=c+l
 9180   cl=cl+1
 9190 loop
 9200 return
 9300 bload "b65support.bin":sys $1600
 9310 a=usr(0):rem set issue# to zero
 9320 l$="/file:"+cf$:gosub 9450:rem post filename
 9330 dopen#2,(cf$)
 9340 l$=""
 9350 get#2,c$
 9360 l$=l$+c$
 9370 if len(l$)=128 then gosub 9450:rem post chunk
 9380 print n$;
 9390 if not st and 64 then 9350
 9400 dclose#2
 9410 if len(l$)<>0 then gosub 9450:rem post remaining chunk
 9420 gosub 9450:rem post empty chunk to declare we've finished
 9430 print "finished posting!"
 9440 return
 9450 rem *** post chunk ***
 9460 l$=chr$(len(l$)+1) + l$:rem prepend the length of string+1
 9470 a=usr("/"+l$)
 9480 print "posting chunk..."
 9490 l$=""
 9500 return
 9505 rem move to next word
 9510 do while mid$(cl$,xc+xi,1) <> " "
 9520   gosub 9600
 9530   if ef then exit
 9540 loop
 9550 do while mid$(cl$,xc+xi,1) = " "
 9560   gosub 9600
 9570   if ef then exit
 9580 loop
 9590 return
 9600 rem move to next char
 9610 ef=0:xc=xc+1
 9620 if xc>len(cl$) then begin
 9630   if yc< nl then xc=0:yc=yc+1:cl$=li$(yc):if yc-ct>20 then gosub 5250
 9635   ef=1
 9640 bend
 9650 return
 9700 rem move to previous word
 9705 gosub 9800:rem prev char
 9710 do while mid$(cl$,xc+xi,1) = " "
 9720   gosub 9800
 9730   if ef then exit
 9740 loop
 9750 do while mid$(cl$,xc+xi,1) <> " "
 9760   gosub 9800
 9770   if ef then exit
 9780 loop
 9785 gosub 9600:rem next char
 9790 return
 9800 rem move to prev char
 9810 ef=0:xc=xc-1
 9820 if xc<0 then begin
 9830   if yc>0 then yc=yc-1:cl$=li$(yc):xc=len(cl$)-1:if yc-ct<0then gosub 5330
 9835   if xc<0 then xc=0
 9840   ef=1
 9850 bend
 9855 if ef=1 and len(cl$) and yc>0 then goto 9810
 9860 return
 9900 rem delete current char
 9910 if xc<len(cl$) then begin
 9920   ch=1
 9930   cl$=left$(cl$,xc+xi-1)+mid$(cl$,xc+xi+1)
 9940   li$(yc)=cl$
 9945   gosub9970
 9950 bend
 9960 return
 9970 rem *** redraw line ***
 9975 iv=0:gosub 10000
 9980 cursor 0,yc-ct:a$=cl$:gosub 1:iflen(cl$)<=79thenprint" ";
 9985 iv=0
 9990 return
10000 rem *** invert if marked line
10010 if mf=0 then return
10020 if ms<me and ms<=yc and yc<=me then iv=1:return
10030 if me<=yc and yc<=ms then iv=1
10040 return
10050 rem *** mark-mode handling ***
10060 if t$="{down}" then me=yc+1:cl$=li$(yc):gosub 9970:yc=yc+1:cl$=li$(yc):gosub 9970:return
10070 if t$="{up}" then me=yc-1:cl$=li$(yc):gosub 9970:yc=yc-1:cl$=li$(yc):gosub 9970:return
10080 if t$="x" or t$="c" then begin
10090   if ms>me then iv=ms:ms=me:me=iv
10100   for iv=me to ms step -1
10110     cb$(iv-ms) = li$(iv)
10120     if t$="x" then yc=iv:ak=1:gosub 5840:ch=1
10130   next iv
10140   iv=0:mf=0:cs=me-ms+1:ms=0:me=0
10150 bend
10155 gosub 5150
10160 return
10200 rem *** paste ***
10210 for iv=cs-1 to 0 step -1
10220   rem insert a line at yc
10230   mf=yc:rem temp
10240   ch=1:gosub 5650
10250   yc=mf:mf=0
10260   li$(yc)=cb$(iv)
10270 next iv
10280 gosub 5150
10290 return

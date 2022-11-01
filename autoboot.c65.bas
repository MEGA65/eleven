  100 rem =================
  110 rem eleven bootloader
  120 rem =================
  130 :
  140 if rwindow(2)<>80 thenprint chr$(27)+"x";
  150 print"{home}{home}{clr}"
  160 lp=0 : rem load prefs flag
  170 mv=920371  : rem minimum rom version
  180 bd$="20-jun-2022"
  190 vn$="0.4.6"
  200 df$="11.defaults"
  210 x$=chr$(27)+chr$(27)
  220 bank 128:poke 0,65
  230 gosub830
  240 poke 248,peek(248)or2^6    : rem disable screen editor line linking
  250 border 6: background 6:foreground 1:print"{home}{home}{clr}"+chr$(142)
  260 w=6
  270 for s=0 to w
  280 cursor79-(w*2)+(s*2)-9,s
  290 print"{rvof}{CBM-*}{rvon}        ";
  300 forl=0to(12-s*2):print" ";:nextl
  310 next s
  320 gosub 940
  330 foreground 1
  340 print"{home}{rvon}eleven{rvof} development environment"
  350 print"{home}{down}{down}version ";vn$;spc(3);bd$
  360 print"{home}{down}{down}{down}written by stephan kleinert"
  370 print"{home}{down}{down}{down}{down}copyright (c) 2021-2022 7 turtles software"
  380 rem check correct rom version
  390 bank 2:for i=0 to 5:v$=v$+chr$(peek(dec("17")+i)):next
  400 v=val(v$)
  410 if v<mv thengosub860: rem version mismatch
  420 poke $bfffff2,$60
  430 gosub 1360: rem check attic ram
  440 clr ti
  450 print "{down}press '{rvon}p{rvof}' during boot to set preferences"
  460 print
  470 print "cleaning up"
  480 scratch "11temp":scratch "11tokenized"
  490 gosub830
  500 print "loading defaults" : gosub570
  505 poke $4ff70,0,0 : rem force load from disc on startup
  510 gosub830
  520 if lp=0 thentb=ti:do:gosub830:loop while ti-tb<.1
  530 bank 0
  540 do:gett$:loopwhilet$<>""
  550 gosub 1030: rem load modules
  555 key on:key16,"edma 0,$4fff,$8000000,$2001:new restore"+chr$(13)+"run"+chr$(13)
  560 if lp=0 then goto 1160: else goto 1300: rem load editor or prefs
  570 rem --- load settings to mailbox ram
  580 t$=chr$(peek($4ff00))+chr$(peek($4ff01))
  590 if t$="sk" thenreturn : rem settings already loaded
  600 trap820
  610 bload (df$),b(4),p($ff00)
  620 trap
  630 if ds<>0 thenprint "could not load defaults"
  640 bank 4:t$=chr$(peek(dec("ff00")))+chr$(peek(dec("ff01")))
  650 rt=0
  660 if t$<>"sk" thenbegin
  670   bg=0:fg=5:bo=6:hl=2:sb=14 : rem standard colours
  680   cf$="" : bu=1 : rt=1
  690   poke $4ff00,asc("s"):poke $4ff01,asc("k")
  700   poke $4ff00,asc("s"):poke $4ff01,asc("k")
  710   poke $4ff02,bo : poke $4ff03,bg : poke $4ff04,fg
  720   poke $4ff05,hl : poke $4ff06,sb
  730   poke $4ff10,0  : rem filename
  740 bend
  750 if rt=1 thenreturn
  760 tf$="":i=$4ff10:do while peek(i)<>0
  770 tf$=tf$+chr$(peek(i)):i=i+1:loop
  780 bo=peek($4ff02):bg=peek($4ff03):fg=peek($4ff04)
  790 hl=peek($4ff05):sb=peek($4ff06)
  800 cf$=tf$:bank 128
  810 return
  820 resumenext
  830 fort=0to100:gett$
  840 ift$="p"thenlp=1
  850 nextt:return
  860 rem --- version mismatch
  870 print "{down}{down}{down}{rvon}{CTRL-O}rom version too old{rvof}"+chr$(27)+chr$(27)
  880 print "{down}rom version >= {rvon}";:print using"######";mv;:print "{rvof} ";
  890 print "required, but version {rvon}";v$;"{rvof} installed!"
  900 print "{down}sorry for the inconvenience. please update ";
  910 print "your mega65 and try again."
  920 do:loop
  930 return
  940 rem --- palette
  950 for r=0 to 7
  960 palette color 16+r,15,15-(r*2),r
  970 next r
  980 bank 1:ba=dec("f828"):a=80
  990 for r=0 to 7
 1000 dma 3,40,ror64,0,ba+(r*a),1
 1010 next
 1020 return
 1030 rem --- load modules
 1040 :
 1050 print"installing editor..."
 1060 bload "11.edit",p($8000000)
 1070 print"installing parser..."
 1080 bload "11.parse",p($8005000)
 1090 print"installing prefs..."
 1100 bload "11.settings",p($800a000)
 1110 print"installing tokenizer..."
 1120 bload "11.tokenize",p($800c000)
 1130 print"installing post compiler..."
 1140 bload "11.post",p($800e000)
 1150 return
 1160 rem --- chain load editor
 1170 :
 1180 print"{home}{home}{clr}{down}{down}edma 0,$4fff,$8000000,$2001:new restore":print"{down}{down}run{home}";
 1190 bank 128
 1200 :
 1210 rem hack: load next module in direct mode
 1220 rem this will go away once we have the "load at address" command
 1230 rem (unless folks start whining about compatiblity with the five
 1240 rem already existing basic10 programs again...)
 1250 :
 1260 bank 128
 1270 poke 208,2     : rem no of chars in keyboard buffer
 1280 poke 688,13,13 : rem return
 1290 end
 1300 rem --- chain load prefs
 1310 print"{home}{home}{clr}{down}{down}edma 0,$1fff,$800a000,$2001:new restore":print"{down}{down}run{home}";
 1320 bank 128
 1330 poke 208,2      : rem no of chars in keyboard buffer
 1340 poke 688,13,13  : rem return
 1350 end
 1360 rem --- attic ram check
 1370 ha&=1:ta=$8000000
 1380 for t=0 to 32
 1390   ra=ta+(t*$1000)
 1400   o=peek(ra)
 1410   poke ra,t+15
 1420   if peek(ra)<>t+15 then ha&=0
 1430   poke ra,o
 1440 next t
 1450 if ha& thenreturn
 1460 border 0:background 0
 1470 print "{red}{clr}error: 11 needs at least "
 1480 print "128kb of attic ram installed.
 1490 print "{down}sorry for the inconvenience.{lblu}"
 1500 do:loop

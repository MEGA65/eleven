  100 rem =================
  110 rem eleven bootloader
  120 rem =================
  130 :
  140 if rwindow(2)<>80 thenprint chr$(27)+"x";
  150 print"{home}{home}{clr}":poke 0,65
  160 bank 4:cf$=chr$(peek(dec("ff00")))+chr$(peek(dec("ff01")))
  170 cb = cf$<>"sk" : rem cb=1 if cold booting
  180 lp=0 : rem load prefs flag
  190 mv=920371  : rem minimum rom version
  200 rem check correct rom version first
  210 bank 2:for i=0 to 5:v$=v$+chr$(peek(dec("17")+i)):next
  220 v=val(v$)
  230 if v<mv thengosub940: rem version mismatch
  240 bd$="24-jun-2022"
  250 vn$="0.5.0"
  260 df$="11.defaults"
  270 x$=chr$(27)+chr$(27)
  280 bank 128
  290 gosub900: rem check for 'p' keypress
  300 poke 248,peek(248)or2^6    : rem disable screen editor line linking
  310 border 6: background 6:foreground 1:print"{home}{home}{clr}"+chr$(142)
  320 w=6
  330 for s=0 to w
  340 cursor79-(w*2)+(s*2)-9,s
  350 print"{rvof}{CBM-*}{rvon}        ";
  360 forl=0to(12-s*2):print" ";:nextl
  370 next s
  380 gosub 1020
  390 foreground 1
  400 print"{home}{rvon}eleven{rvof} development environment"
  410 print"{home}{down}{down}version ";vn$;spc(3);bd$
  420 print"{home}{down}{down}{down}written by stephan kleinert"
  430 print"{home}{down}{down}{down}{down}copyright (c) 2021-2022 7 turtles software"
  440 gosub 1450: rem check attic ram
  450 if cb then print "{down}** cold boot **":gosub 1610: rem check attic ram state
  460 clr ti
  470 print "{down}press '{rvon}p{rvof}' during boot to set preferences"
  480 print "press '{rvon}c{rvof}' during boot to force cold boot"
  490 print
  500 print "cleaning up"
  510 scratch "11temp":scratch "11tokenized"
  520 gosub900
  530 print "loading defaults" : gosub640
  540 poke $4ff70,0,0 : rem force load from disc on startup
  550 poke $4ff08,0   : rem clear external error on startup
  560 gosub900
  570 if lp=0 thentb=ti:do:gosub900:loop while ti-tb<.1
  580 bank 0
  590 do:gett$:loopwhilet$<>""
  600 gosub 1110: rem load modules
  610 key on:key16,"{home}{home}{clr}edma 0,$3fff,$8001000,$2001:new restore"+chr$(13)+"run"+chr$(13)
  620 bank128:poke $d61a,peek($d61a)or16
  625 if sm then print chr$(27)+"5";:rem switch to 80x50 if settings say so
  630 if lp=0 then goto 1250: else goto 1390: rem load editor or prefs
  640 rem --- load settings to mailbox ram
  650 t$=chr$(peek($4ff00))+chr$(peek($4ff01))
  660 if t$="sk" then goto 830 : rem settings already loaded
  670 trap890
  680 bload (df$),b(4),p($ff00)
  690 trap
  700 if ds<>0 thenprint "could not load defaults"
  710 bank 4:t$=chr$(peek(dec("ff00")))+chr$(peek(dec("ff01")))
  720 rt=0
  730 if t$<>"sk" thenbegin
  740   bg=0:fg=5:bo=6:hl=2:sb=14 : rem standard colours
  750   cf$="" : bu=1 : rt=1
  760   poke $4ff00,asc("s"):poke $4ff01,asc("k")
  770   poke $4ff00,asc("s"):poke $4ff01,asc("k")
  780   poke $4ff02,bo : poke $4ff03,bg : poke $4ff04,fg
  790   poke $4ff05,hl : poke $4ff06,sb
  800   poke $4ff10,0  : rem filename
  810 bend
  820 if rt=1 thenreturn
  830 tf$="":i=$4ff10:do while peek(i)<>0
  840 tf$=tf$+chr$(peek(i)):i=i+1:loop
  850 bo=peek($4ff02):bg=peek($4ff03):fg=peek($4ff04)
  860 hl=peek($4ff05):sb=peek($4ff06)
  865 sm = peek($4ff07)and2*5
  870 cf$=tf$:bank 128
  880 return
  890 resumenext
  900 fort=0to400:gett$
  910 ift$="p"thenlp=1
  920 ift$="c"thencb=-1
  930 nextt:return
  940 rem --- version mismatch
  950 print "{down}{down}{down}{rvon}{CTRL-O}rom version too old{rvof}"+chr$(27)+chr$(27)
  960 print "{down}rom version >= {rvon}";:print using"######";mv;:print "{rvof} ";
  970 print "required, but version {rvon}";v$;"{rvof} installed!"
  980 print "{down}sorry for the inconvenience. please update ";
  990 print "your mega65 and try again."
 1000 do:loop
 1010 return
 1020 rem --- palette
 1030 for r=0 to 7
 1040 palette color 16+r,15,15-(r*2),r
 1050 next r
 1060 bank 1:ba=$ff80800:a=80
 1070 for r=0 to 7
 1080 edma 3,80,ror64,ba+(r*a)
 1090 next
 1100 return
 1110 rem --- load modules
 1120 if cb then begin
 1130 print"installing editor..."
 1140 bload "11.edit",p($8001000)
 1150 print"installing parser..."
 1160 bload "11.parse",p($8005000)
 1170 print"installing prefs..."
 1180 bload "11.settings",p($800a000)
 1190 print"installing tokenizer..."
 1200 bload "11.tokenize",p($800c000)
 1210 print"installing post compiler..."
 1220 bload "11.post",p($800e000)
 1230 bend
 1240 return
 1250 rem --- chain load editor
 1260 :
 1270 print"{home}{home}{clr}{down}{down}edma 0,$3fff,$8001000,$2001:new restore":print"{down}{down}run{home}";
 1280 bank 128
 1290 :
 1300 rem hack: load next module in direct mode
 1310 rem this will go away once we have the "load at address" command
 1320 rem (unless folks start whining about compatiblity with the five
 1330 rem already existing basic10 programs again...)
 1340 :
 1350 bank 128
 1360 poke 208,2     : rem no of chars in keyboard buffer
 1370 poke 688,13,13 : rem return
 1380 end
 1390 rem --- chain load prefs
 1400 print"{home}{home}{clr}{down}{down}edma 0,$1fff,$800a000,$2001:new restore":print"{down}{down}run{home}";
 1410 bank 128
 1420 poke 208,2      : rem no of chars in keyboard buffer
 1430 poke 688,13,13  : rem return
 1440 end
 1450 rem --- attic ram check
 1460 ha&=1:ta=$8000000
 1470 for t=0 to 32
 1480   ra=ta+(t*$1000)
 1490   o=peek(ra)
 1500   poke ra,t+15
 1510   if peek(ra)<>t+15 then ha&=0
 1520   poke ra,o
 1530 next t
 1540 if ha& thenreturn
 1550 border 0:background 0
 1560 print "{red}{clr}error: 11 needs at least "
 1570 print "128kb of attic ram installed.
 1580 print "{down}sorry for the inconvenience.{lblu}"
 1590 do:loop
 1600 :
 1610 rem --- quick and dirty attic ram sanity check
 1620 print "checking attic ram sanity";
 1630 a=$30000:b=$8000000
 1640 edma 0,$10000,a,b
 1650 for c=0 to $fffe step 7
 1660   if wpeek(a+c)<>wpeek(b+c) then goto 1710: rem fail
 1670   if mod(c,$400)=0 then print".";
 1680 next
 1690 print:print "attic ram seems stable. phew."
 1700 return
 1710 print"{home}{home}{clr}{rvon}{CTRL-O}attic ram sanity test failure{rvof}"+chr$(27)+"o"
 1720 print
 1730 print"eleven's initial test has determined that attic ram seems to be on the fritz
 1740 print"(again). please install the most recent stable mega65 core on your machine.     if the problem ";
 1750 print"persists, please contact the mega65 team."
 1760 print"{down}{down}error detail:"
 1770 print"expected $";hex$(wpeek(a+c));" at address $";hex$(b+c);
 1780 print " but got $";hex$(wpeek(b+c))
 1790 print"{down}{down}sorry for the inconvenience"
 1800 do:loop

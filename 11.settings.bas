  100 df$="11.defaults"
  110 sb$="{rvon}ELEVEN development environment version 0.1 alpha"
  120 gosub 820: rem get settings from mailbox ram
  130 key off
  140 print"{home}{home}{clr}{swlc}"
  150 dl$=chr$(27)+"q": rem delete line
  160 bu$(0)="disabled":bu$(1)="enabled "
  170 sm$(0)="80x25":sm$(1)="80x50"
  180 gosub 1220: rem read defaults, set screen mode
  190 do
  200 foreground fg: background bg: border bo
  210 print"{home}{rvon}  ELEVEN preferences  {rvof}"
  220 cursor 0,3:print " {rvon} F1 {rvof} working file name : ";dl$;cf$;
  230 if cf$="" then print "<not defined>";
  240 print:print " {rvon} F2 {rvof} automatic backup  : ";bu$(bu)
  250 cursor 0,5
  260 print "{down} {rvon} F3 {rvof} border colour     : ";
  270 foreground bo: print "{rvon}          {rvof}":foreground fg
  280 print " {rvon} F4 {rvof} background colour : ";
  290 foreground bg: print "{rvon}          {rvof}":foreground fg
  300 print " {rvon} F5 {rvof} foreground colour : ";
  310 foreground fg: print "{rvon}          {rvof}":foreground fg
  320 print " {rvon} F6 {rvof} highlight colour  : ";
  330 foreground hl: print "{rvon}          {rvof}":foreground fg
  340 print " {rvon} F7 {rvof} status bar colour : ";
  350 foreground sb: print "{rvon}          {rvof}":foreground fg
  360 print
  370 print " {rvon} F9 {rvof} screen mode       : ";sm$(sm)
  380 cursor 0,16:foreground hl
  390 print" {rvon} S {rvof}  Save as defaults"
  400 print" {rvon} L {rvof}  Load defaults from disc"
  410 print" {rvon} R {rvof}  Reset defaults"
  420 print"{down}{down} {rvon}TAB{rvof}  Exit preferences & launch editor";
  430 cursor 0,24:foreground sb
  440 if sb$="" then sb$="{rvon}ELEVEN development environment version 0.5"
  450 gosub 1130
  460 getkey t$ : sb$=""
  470 if t$="{f3}" then bo=bo+1:if bo>15 then bo=0
  480 if t$="{f4}" then bg=bg+1:if bg>15 then bg=0
  490 if t$="{f5}" then fg=fg+1:if fg>15 then fg=0
  500 if t$="{f6}" then hl=hl+1:if hl>15 then hl=0
  510 if t$="{f7}" then sb=sb+1:if sb>15 then sb=0
  520 if t$="{f2}" then bu=abs(bu-1)
  530 if t$="{CTRL-P}" then begin
  540   sm = abs(sm-1)
  550   gosub 1220: rem set screenmode
  560 bend
  570 if t$="r" then rs=1:gosub820
  580 if t$="s" then begin
  590   sb$="saving defaults...":gosub 1130
  600   scratch (df$)
  610   bsave (df$),b(4),p(dec("ff00")) to p(dec("ff80"))
  620   dma 3,80,160,0,3968,0 : rem  --- clear status bar
  630   if ds=0 then sb$="{rvon}defaults saved.": else sb$="{rvon}disc error "+ds$
  640   gosub 1130
  650 bend
  660 if t$="l" then begin
  670   sb$="loading defaults...":gosub 1130
  680   bload (df$),b(4),p(dec("ff00"))
  690   gosub 820
  700   if ds=0 then sb$="{rvon}defaults loaded.": else sb$="{rvon}disc error "+ds$
  710   gosub 1130:gosub 1220
  720 bend
  730 gosub 990
  740 loop until t$="{ensh}"
  750 clr
  760  print"{home}{home}{clr}{down}{down}edma 0,$d400,$8000000,$2001:new restore{down}{down}":print"run{home}";:rem load '11.edit' from attic cache
  770  bank 128
  780  poke 208,2     : rem no of chars in keyboard buffer
  790  poke 688,13,13 : rem return
  800  end
  810 stop
  820 rem --- load settings to mailbox ram
  830 t$=chr$(peek($4ff00))+chr$(peek($4ff01))
  840 rt=0
  850 if t$<>"sk"or rs=1 then begin
  860   bg=0:fg=5:bo=6:hl=2:sb=14 : rem standard colours
  870   cf$="" : bu=1 : rt=1 : rs=0 : sm=0
  880 bend
  890 if rt=1 then return : rem return if just initialized
  900 tf$="":i=$4ff10:do while peek(i)<>0
  910 tf$=tf$+chr$(peek(i)):i=i+1:loop
  920 bo=peek($4ff02):bg=peek($4ff03):fg=peek($4ff04)
  930 hl=peek($4ff05):sb=peek($4ff06)
  940 bu = abs((peek($4ff07)and4)<>0)
  950 sm = abs((peek($4ff07)and8)<>0)
  960 cf$=tf$:bank 128
  970 return
  980 :
  990 rem --- save settings from mailbox ram
 1000 bank 4
 1010 if cf$="" then goto 1040
 1020 for i=1 to len(cf$):poke dec("ff10")+i-1,asc(mid$(cf$,i,1)):next i
 1030 poke dec("ff10")+i-1,0
 1040 poke dec("ff07"),0 : fl=0 : rem delete flags
 1050 fl = fl or 1              : rem autoload flag
 1060 if bu=1 then fl=fl or 4   : rem autobackup flag
 1070 if sm=1 then fl=fl or 8   : rem 80x50 flag
 1080 poke $4ff07,fl
 1090 poke $4ff00,asc("s"):poke $4ff01,asc("k")
 1100 poke $4ff02,bo : poke $4ff03,bg : poke $4ff04,fg
 1110 poke $4ff05,hl : poke $4ff06,sb
 1120 bank128:return
 1130 rem --- status bar
 1140 if rwindow(1)=25 then cb=2048+(24*80)   : fb=$ff80800 + (24*80)
 1150 if rwindow(1)=50 then cb=$40800+(49*80) : fb=$ff80800 + (49*80)
 1160 edma 3,80,160,cb   : rem  --- clear status bar
 1170 edma 3,80,sb,fb    : rem  set status bar color
 1180 cursor 0,rwindow(1)-1
 1190 foreground sb : print sb$;
 1200 return
 1210 input t$
 1220 rem --- set screenmode
 1230 if sm then print chr$(27)+"5":else print chr$(27)+"8"
 1240 scnclr
 1250 return

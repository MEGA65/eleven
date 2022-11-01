   10 df$="11.defaults"
   15 sb$="{rvon}ELEVEN development environment version 0.1 alpha"
   20 gosub 5000 : rem get settings from mailbox ram
   30 key off
   45 print"{home}{home}{clr}{swlc}"
   50 dl$=chr$(27)+"q": rem delete line
   60 bu$(0)="disabled":bu$(1)="enabled "
   70 gosub 6100
  100 do
  101 foreground fg: background bg: border bo
  102 print"{home}{rvon}  ELEVEN preferences  {rvof}"
  105 cursor 0,3:print " {rvon} F1 {rvof} working file name : ";dl$;cf$;
  107 if cf$="" then print "<not defined>";
  108 print:print " {rvon} F2 {rvof} automatic backup  : ";bu$(bu)
  115 cursor 0,5
  120  print "{down} {rvon} F3 {rvof} border colour     : ";
  125 foreground bo: print "{rvon}          {rvof}":foreground fg
  130 print " {rvon} F4 {rvof} background colour : ";
  135 foreground bg: print "{rvon}          {rvof}":foreground fg
  140 print " {rvon} F5 {rvof} foreground colour : ";
  145 foreground fg: print "{rvon}          {rvof}":foreground fg
  150 print " {rvon} F6 {rvof} highlight colour  : ";
  155 foreground hl: print "{rvon}          {rvof}":foreground fg
  160 print " {rvon} F7 {rvof} status bar colour : ";
  165 foreground sb: print "{rvon}          {rvof}":foreground fg
  200 cursor 0,16:foreground hl
  210 print" {rvon} S {rvof}  Save as defaults"
  220 print" {rvon} L {rvof}  Load defaults from disc"
  230 print" {rvon} R {rvof}  Reset defaults"
  232 print"{down}{down} {rvon}TAB{rvof}  Exit preferences & launch editor";
  235 cursor 0,24:foreground sb
  239 dma 3,80,sb,0,65408,1
  240 getkey t$
  250 print"{rvon}ELEVEN development environment version 0.1 alpha";
  255 if t$="{f3}" then bo=bo+1:if bo>15 then bo=0
  260 if t$="{f4}" then bg=bg+1:if bg>15 then bg=0
  270 if t$="{f5}" then fg=fg+1:if fg>15 then fg=0
  280 if t$="{f6}" then hl=hl+1:if hl>15 then hl=0
  290 if t$="{f7}" then sb=sb+1:if sb>15 then sb=0
  300 if t$="{f2}" then bu=abs(bu-1)
  310 if t$="r" then rs=1:gosub5000
  320 if t$="s" then begin
  322   sb$="saving defaults...":gosub 6100
  330   scratch (df$)
  340   bsave (df$),b(4),p(dec("ff00")) to p(dec("ff80"))
  350   dma 3,80,160,0,3968,0 : rem  --- clear status bar
  360   if ds=0 then sb$="{rvon}defaults saved.": else sb$="{rvon}disc error "+ds$
  365   gosub 6100
  380 bend
  390 if t$="l" then begin
  391   sb$="loading defaults...":gosub 6100
  392   bload (df$),b(4),p(dec("ff00"))
  394   gosub 5000
  400   if ds=0 then sb$="{rvon}defaults loaded.": else sb$="{rvon}disc error "+ds$
  410   gosub 6100
  430 bend
  490 gosub 6000
  500 loop until t$="{ensh}"
  510 clr
  520  print"{home}{home}{clr}{down}{down}edma 0,$4fff,$8000000,$2001:run{home}";
  530  bank 128
  540  poke 208,1   : rem no of chars in keyboard buffer
  550  poke 688,13  : rem return
  560  end
  999 stop
 5000 rem --- load settings to mailbox ram
 5010 bank 4:t$=chr$(peek(dec("ff00")))+chr$(peek(dec("ff01")))
 5015 rt=0
 5020 if t$<>"sk"or rs=1 then begin
 5021   bg=0:fg=5:bo=6:hl=2:sb=14 : rem standard colours
 5022   cf$="" : bu=1 : rt=1 : rs=0
 5023 bend
 5025 if rt=1 then return
 5050 tf$="":i=dec("ff10"):do while peek(i)<>0
 5056 bu=abs((peek(dec("ff07"))and4)=1)
 5060 tf$=tf$+chr$(peek(i)):i=i+1:loop
 5062 bo=peek(dec("ff02")):bg=peek(dec("ff03")):fg=peek(dec("ff04"))
 5065 hl=peek(dec("ff05")):sb=peek(dec("ff06"))
 5068 bu = abs((peek(dec("ff07"))and4)<>0)
 5070 cf$=tf$:bank 128
 5080 return
 5090 :
 6000 rem --- save settings from mailbox ram
 6010 bank 4
 6015 if cf$="" then goto 6035
 6020 for i=1 to len(cf$):poke dec("ff10")+i-1,asc(mid$(cf$,i,1)):next i
 6030 poke dec("ff10")+i-1,0
 6035 poke dec("ff07"),0 : fl=0 : rem delete flags
 6036 fl = fl or 1              : rem autoload flag
 6037 if bu=1 then fl=fl or 4   : rem autobackup flag
 6040 poke dec("ff07"),fl
 6050 poke dec("ff00"),asc("s"):poke dec("ff01"),asc("k")
 6060 poke dec("ff02"),bo : poke dec("ff03"),bg : poke dec("ff04"),fg
 6070 poke dec("ff05"),hl : poke dec("ff06"),sb
 6080 bank128:return
 6100 dma 3,80,160,0,3968,0 : rem  --- clear status bar
 6110  cursor 0,24
 6120 print sb$;
 6130 return

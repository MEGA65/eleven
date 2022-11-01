   10 rem post-compile
   15 print"{home}{home}{clr}compiling successful."
   20 bank 4:ad=dec("ff30")
   30 do while peek(ad)<>0:f$=f$+chr$(peek(ad)):ad=ad+1:loop
   40 if f$="" or len(f$)>16 then print "??fatal error determining outfile":stop
   45 scratch("11temp"):scratch(f$)
   50 rename ("11tokenized") to (f$)
   60 print"{home}{down}{down}{down}{down}run"+chr$(34)+f$:print"{home}{down}{down}";
   65 key on:key16,"edma 0,$4fff,$8000000,$2001:new restore"+chr$(13)+"run"+chr$(13)
   66 bank 128
   68 poke 208,1:poke 688,13
   70 new

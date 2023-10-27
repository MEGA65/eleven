32000 bank128:for x=0to5:reada$:pokedec("1600")+x,dec(a$):nextx
32001 poke 248,0 : rem restore row link mode
32002 print:print "pass 3: tokenizing":print"{down}{down}{down}{up}{up}{up}"+chr$(27)+"t"+"{down}{down}{down}{left}"+chr$(27)+"b";
32003 open 1,1,5,"11temp,s,r":sysdec("1600") : rem stdin = 11temp
32004 rem fill keyboard buffer
32005 poke208,4:poke 688,asc("r"):poke689,asc("u"):poke690,asc("n"):poke 691,13
32006 data a2,01,20,c6,ff,60

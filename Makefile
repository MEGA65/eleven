all:
	# cp /c/Users/phuon/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 .
	c1541 -attach 11.D81 -read 11.defaults -read 11.edit -read 11.parse -read 11.post -read 11.settings -read autoboot.c65 -read readme,s
	petcat -65 -o 11.defaults.bas -- 11.defaults
	petcat -65 -o 11.edit.bas -- 11.edit
	petcat -65 -o 11.parse.bas -- 11.parse
	petcat -65 -o 11.post.bas -- 11.post
	petcat -65 -o 11.settings.bas -- 11.settings
	petcat -65 -o autoboot.c65.bas -- autoboot.c65
	# c1541 -attach MA110.D81 -read gurce.asm,s

FORCE: ;

11.parse.prg: FORCE 11.parse.asm
	acme --cpu m65 -o 11.parse.prg -f cbm -l 11.parse.sym -r 11.parse.rep 11.parse.asm
	c1541 -attach /c/Users/phuon/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 -delete 11.parse -write 11.parse.prg 11.parse

test: 11.parse.asm 11.parse.test.asm
	./tests_autogen.sh
	acme -DRUN_TESTS=1 --cpu m65 -o 11.parse.prg -f cbm -l 11.parse.sym -r 11.parse.rep 11.parse.asm
	c1541 -attach /c/Users/phuon/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 -delete 11.parse -write 11.parse.prg 11.parse
	# /c/projs/xemu/build/bin/xmega65.native -rom /c/projs/mega65-rom/newrom.bin -hdosvirt -uartmon :4510 -prg 11.parse.prg &> /dev/null &

xemu:
	/c/projs/xemu/build/bin/xmega65.native -rom /c/projs/mega65-rom/newrom.bin -hdosvirt -uartmon :4510 -8 /C/Users/phuon/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 &> /dev/null &

oldparse:
	c1541 -attach /c/Users/phuon/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 -delete 11.parse -write 11.parse 

tod81:
	cp /c/Users/gurcei/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 .
	c1541 -attach /c/Users/gurcei/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 -delete 11.defaults -write 11.defaults
	c1541 -attach /c/Users/gurcei/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 -delete 11.edit -write 11.edit 
	c1541 -attach /c/Users/gurcei/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 -delete 11.parse -write 11.parse 
	c1541 -attach /c/Users/gurcei/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 -delete 11.post  -write 11.post 
	c1541 -attach /c/Users/gurcei/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 -delete 11.settings  -write 11.settings 
	c1541 -attach /c/Users/gurcei/AppData/Roaming/xemu-lgb/mega65/hdos/11.D81 -delete autoboot.c65 -write autoboot.c65

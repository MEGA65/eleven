all:
	c1541 -attach 11dist_80x50.d81 -read 11.defaults -read 11.edit -read 11.parse -read 11.post -read 11.settings -read autoboot.c65
	petcat -65 -o 11.defaults.bas -- 11.defaults
	petcat -65 -o 11.edit.bas -- 11.edit
	petcat -65 -o 11.parse.bas -- 11.parse
	petcat -65 -o 11.post.bas -- 11.post
	petcat -65 -o 11.settings.bas -- 11.settings
	petcat -65 -o autoboot.c65.bas -- autoboot.c65
	# c1541 -attach MA110.D81 -read gurce.asm,s

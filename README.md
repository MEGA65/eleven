# eleven
Eleven IDE - improving upon the BASIC 65 experience

Note that presently:
- Eleven v2.3 works well with the 92xxxx ROMs
- Eleven v2.4 works well with the 99xxxx ROMs

My current tweaks are based on Eleven v2.3, however, in time, I'll aim to get Eleven working with both series of ROMs.

Current tweaks:
- ALT+P = Added support for `m65postbox` tool, to post current source across serial to your pc
          (to allow you to version control your work on your pc)
    - see https://github.com/MEGA65/mega65-tools/blob/m65postbox/src/tools/m65postbox.c
      (run on your pc with: `./m65postbox -l <devicename>`
- Support for WPOKE, WPEEK, SETBIT, EDMA, RPLAY
- ESC,J = go to start of line
- ESC,K = go to end of line
- ESC,D = delete current line
- CTRL+W = move to next word
- CTRL+U = move to previous word
- Shift-InstDel = delete current character
- CTRL+= will toggle compilation debugging on/off
- ESC,M = turn 'mark-mode' on
  - When 'mark-mode' is on, use cursor up/down to select lines
  - Then press x to cut or c to copy
  - Then use ESC-P to paste clipboard contents at the current cursor location
- Added support for "#define CONSTNAME = val"
  (for occasions where you just want to hide magic numbers in your source)
- To continue a line onto the next line, put a '<-' (left arrow char) on the end of the line
- added #ifdef and #endif

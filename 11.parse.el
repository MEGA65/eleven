#OUTPUT "11.PARSE-NEW"' ����4 �EMORY USAGE  (AKA '�AILBOX ���')' ------------------' 4.��00  "SK" MAGIC SIGNATURE (�TEPHAN �LEINERT / UBIK'S INITIALS)' 4.��02  BORDER COLOUR' 4.��03  BACKGROUND COLOUR' 4.��04  FOREGROUND COLOUR' 4.��05  HIGHLIGHT COLOUR' 4.��06  STATUS BAR COLOUR' 4.��07  MISC. FLAGS'         - BIT0: AUTOLOAD SOURCE ENABLE FLAG'         - BIT1: AUTOJUMP TO LINE FLAG'         - BIT2: AUTOBACKUP FLAG'         - BIT3: VERBOSE FLAG'         - BIT4: TEXT-MODE (0=80X25, 1=80X50)'         - BIT5-7: RESERVED' 4.��08  ERROR NUMBER TO BE DISPLAYED (>128 ARE PREPROCESSOR ERRORS)' 4.��09  (2 BYTES) LINE NUMBER FOR AUTOJUMP'' 4.��10  CURRENT FILE NAME' 4.��20  OUTPUT FILE NAME'' 4.��30- RESERVED' 4.��7�'' �TTIC ��� CACHE USAGE' =====================' $800,0000 +---------------+'           ! 11.����       ! (MAX 53KB)' $801,0000 +---------------+'           ! 11.�����      ! (MAX 53KB)' $802,0000 +---------------+'           ! 11.��������   ! (8KB)' $802,2000 +---------------+'           ! 11.��������   ! (8KB)' $802,4000 +---------------+'           ! 11.����       ! (8KB)' $802,6000 +---------------+' ' $803,0000 +---------------+'           ! ������ ������ !'       ... +---------------+' ������ ������ ������� ������:' ----------------------------' $803,0000  ����       �OTAL �INES' $803,0002  ����       �INE 1 LENGTH' $803,0003  ����[���]  �HARS FOR �INE 1' $803,00XX  ����       �INE 2 LENGTH' $803,00YY  ����[���]  �HARS FOR �INE 2' ...'-------.DEFINES'-------#DEFINE ��Я���� = 0#DEFINE ��Я��� = 1#DEFINE ��Я��� = 2#DEFINE ��Я���� = 3#DEFINE ��Я��� = 4  CLR#DECLARE DBL�QUOTE$, PARSER�FILE$, T$, BLANK�LINE$, TYPE�IDENT$#DECLARE CURR�SRC�LINE$, NEXT�LINE�FLAG, NEXT�LINE$#DECLARE MK$, DBL�QUOTE�CHAR, SNGL�QUOTE�CHAR#DECLARE TOKENS$(10), DUMB�CMDS$, I#DECLARE BIN�CONV(16)  ' BIT SHIFTER'S FAST BINARY CONVERSION#DECLARE MAP�DEST�TO�SRC�LINENO%(1000) ' DEST FILE LINE NR --> SRC FILE LINE NR#DECLARE DEST�LINE$(1000)              ' POST PROCESSED LINES#DECLARE ELEMENT�CNT(4)                ' ELEMENT COUNT PER TYPE#DECLARE VAR�TABLE$(4,200)             ' VARIABLE TABLE PER TYPE#DECLARE DEFINE�VAL$(200)              ' DEFINE VALUES TABLE#DECLARE LABEL�NAME$(200)              ' LABEL TABLE#DECLARE LABEL�LINENO(200)#DECLARE LABEL�CNT=0#DECLARE ARGS$(32)                     ' ARGUMENT LIST#DECLARE STRUCT�NAME$(30)#DECLARE STRUCT�FIELDS$(30)#DECLARE STRUCT�VARS$(30)              ' STRUCT VARS (EACH ENTRY HAS STRING OF VARS)#DECLARE STRUCT�IDX=0                  ' INDEX TO NEXT FREE STRUCT DEFINITION#DECLARE WHITESPACE$, SRC�LINENO, CUR�SRC�LINENO, CB#DECLARE CURR�ATTIC�ADDR, TOTAL�LINES, S$, DELETE�LINE�FLAG#DECLARE VERBOSE, DEST�LINENO, INSIDE�IFDEF, Z$, CURR�DEST�LINE$#DECLARE CUR�DEST�LINENO, S1, S2, CUR�TOK$, R, F$, DEFINE�FLAG, DELIM$#DECLARE IGNORE�BRACKETS, ARG�CNT, PARSER�ERROR$, VAR�NAME$#DECLARE DIMENSION$, VALUE$, BKT�OPEN�IDX, BKT�CLOSE�IDX, EQUALS�IDX#DECLARE TR$, HX$, BI$, DONT�MARK�LABEL, TY, ID, GEN�VARNAME$, OF$#DECLARE ARGS�LIST$, ARGS�LIST�LEN, IGNORE�DELIM�FLAG, ARGS�IDX, CUR�CH$#DECLARE QUOTE�FLAG, DID�REPLACE�FLAG#DECLARE A$, EXPECTING�LABEL, DEFAULT�DELIM$, RV�IDX#DECLARE T, LC$, FOUND�STRUCT�IDX, BR, B, BC$, VL#DECLARE N1, N2, BA, A, AD, TF$, ZZ, K, SF$, SF#DECLARE CUR�LINE�LEN, SRC�LINE�PTR, SRC�LINEBUFF�PTR, CUT�TAIL�IDX#DECLARE CUR�LINE�LEN�MINUS�ONE, CUR�LINEBUFF�ADDR, CHR#DECLARE CO$, ZI, SK$, BL, SZ, SR, SM, ZZ$#DECLARE CONT�NEXT�LINE�FLAG, TOK�NAME$, CLEAN�VARNAME$'----.MAIN'----  PARSER�FILE$ = "11.PARSE"  MK$ = "@~"  ' @ AND PI  DBL�QUOTE$ = CHR$(34)  DBL�QUOTE�CHAR = 34  SNGL�QUOTE�CHAR = 39  KEY ON  PRINT "������ PREPROCESSOR V0.6.0�"  PRINT  KEY 7, "SCRATCH" + DBL�QUOTE$ + PARSER�FILE$ + DBL�QUOTE$ + ":DSAVE" + DBL�QUOTE$ + PARSER�FILE$ + DBL�QUOTE$ + ":DVERIFY" + DBL�QUOTE$+PARSER�FILE$  T$="                                                                               "  BLANK�LINE$ = T$ + T$ + T$  T$ = ""TOKENS$(0)=" PRINT INPUT IF THEN ELSE DO LOOP WHILE UNTIL GOSUB GOTO OPEN CLOSE DOPEN DCLOSE FOR NEXT GETKEY HEX$ DIM PEEK POKE WAIT DEC CHR$ ASC SGN SQR STR$"  TOKENS$(0) = TOKENS$(0) + " GRAPHIC CLR SCREEN DEF BEGIN BEND LEN MID$ RIGHT$ LEFT$ INSTR FOR NEXT STEP TRAP BORDER AND FOREGROUND "  TOKENS$(1) = " BACKGROUND SET ABS SIN COS TAN LOG FRE CURSOR PIXEL WINDOW RWINDOW LINE BOX CIRCLE ELLIPSE PALETTE RESTORE DATA ERR$ ER EL CURSOR ON OFF"  TOKENS$(1) = TOKENS$(1) + " VAL SCRATCH RETURN RND STOP BANK TI DO OR ST IF EL ER ON TO PEN GET END INT NOT DS RUN USING DOT "  TOKENS$(2) = " APPEND ATN AUTO BACKUP BLOAD BOOT BSAVE BUMP BVERIFY CATALOG CHANGE CHAR CMD COLLISION COLOR CONCAT CONT COPY WPOKE WPEEK SETBIT CLRBIT "  TOKENS$(3) = " DCLEAR DEFFN DELETE FN DIR DISK DLOAD DMA DMODE DPAT DSAVE DVERIFY EDMA ENVELOPE ERASE EXIT EXP FAST FILTER FIND GO64 HEADER HELP HIGHLIGHT "  TOKENS$(4) = " JOY LIST LOAD LOCATE LPEN MOD MONITOR MOUSE MOVSPR NEW PAINT PLAY POINTER POLYGON POS POT PUDEF "  TOKENS$(5) = " RCOLOR RDOT READ RECORD REM RENAME RESUME RGR RMOUSE RPLAY RREG RSPCOLOR RSPPOS RSPRITE SAVE SCNCLR SLEEP SLOW SOUND SPC SPRCOLOR "  TOKENS$(6) = " SPRITE SPRSAV SYS TAB TEMPO TROFF TRON TYPE USR VERIFY VOL XOR KEY VSYNC RCURSOR T@& C@& RGRAPHIC FREAD POINTER "  DUMB�CMDS$ = " BLOAD BSAVE DLOAD TO SAVE DIR COLLECT DOPEN DCLOSE BACKUP FREAD GET "  GOSUB GET�FILENAME  BANK 128  TYPE�IDENT$(��Я����)=""  TYPE�IDENT$(��Я���)="%"  TYPE�IDENT$(��Я���)="$"  TYPE�IDENT$(��Я����)="&"  BIN�CONV(0) = 1  FOR I = 1 TO 16    BIN�CONV(I) = BIN�CONV(I - 1) + BIN�CONV(I - 1)  NEXT I' ------------------------- PASS 1 ------------------------------------'------.PASS�1'------  NEXT�LINE�FLAG = 0  WHITESPACE$ = CHR$(32) + CHR$(160) + "	"  ' CLEANUP TEMPORARY FILES  SRC�LINENO = 0  PRINT "PASS 1 ";  CUR�SRC�LINENO=0  CLR TI  ' KEEP START TIME FOR TIMING  CB = $8030000   ' ELEVEN SOURCE TO BE COMPILED IS LOCATED HERE IN ATTIC RAM  CURR�ATTIC�ADDR = CB  TOTAL�LINES = WPEEK(CURR�ATTIC�ADDR)  CURR�ATTIC�ADDR = CURR�ATTIC�ADDR + 2  DO WHILE CUR�SRC�LINENO <> TOTAL�LINES  ' UNTIL TARGET LINES IS REACHED    GOSUB READ�NEXT�LINE    GOSUB SINGLE�QUOTE�COMMENT�TRIM    ' STRIP WHITESPACE FROM END    S$ = CURR�SRC�LINE$    GOSUB STRIP�TR$�FROM�END    CURR�SRC�LINE$ = S$    IF CURR�SRC�LINE$ <> "" THEN BEGIN      DELETE�LINE�FLAG = 0      IF VERBOSE THEN PRINT ">> ����:" DEST�LINENO;", ���: "; SRC�LINENO;": "; CURR�SRC�LINE$      IF LEFT$(CURR�SRC�LINE$, 1) = "." THEN BEGIN        NEXT�LINE�FLAG = 1        GOSUB ADD�TO�LABEL�TABLE      BEND      IF LEFT$(CURR�SRC�LINE$, 1) = "#" THEN BEGIN        GOSUB PARSE�PREPROCESSOR�DIRECTIVE      BEND      IF INSIDE�IFDEF = 1 THEN GOTO PARSER�LOOP�SKIP      IF LEFT$(CURR�SRC�LINE$, 4) = "DATA" OR RIGHT$(CURR�SRC�LINE$, 5) = "BEGIN" THEN BEGIN        NEXT�LINE�FLAG = 1      BEND      IF DELETE�LINE�FLAG = 0 THEN BEGIN        IF VERBOSE = 0 THEN PRINT ".";        S$ = CURR�SRC�LINE$        GOSUB REPLACE�VARS�AND�LABELS        GOSUB CHECK�FOR�CREATION�OF�STRUCT�OBJECT        GOSUB CHECK�FOR�CONTINUE�ONTO�NEXT�LINE        ' SAFE ADD CURR�DEST�LINE$+S$ TO CURRENT OR NEXT DEST�LINE$(DEST�LINENO)        GOSUB SAFE�ADD�TO�CURRENT�OR�NEXT�LINE        IF RIGHT$(S$, 4) = "BEND" OR RIGHT$(S$, 6) = "RETURN" _           OR LEFT$(S$, 2) = "IF" THEN BEGIN          NEXT�LINE�FLAG = 1        BEND      BEND  ' ENDIF DELETE�LINE�FLAG = 0    BEND  ' ENDIF CURR�SRC�LINE$ <> "".PARSER�LOOP�SKIP    ' INCREASE SOURCE CODE LINE (FOR ERROR MSGS...)    SRC�LINENO = SRC�LINENO + 1    IF VERBOSE THEN BEGIN      PRINT "SRC�LINENO="; SRC�LINENO      GET KEY Z$    BEND  LOOP    IF CURR�DEST�LINE$ <> "" THEN BEGIN    DEST�LINE$(DEST�LINENO) = CURR�DEST�LINE$    DEST�LINENO = DEST�LINENO + 1  BEND  CLOSE 1  GOSUB SAVE�FILENAME  ' SET OUTPUT FILENAME  SCRATCH "11TEMP"  SCRATCH "11TOKENIZED"' ------------------------- PASS 2 ------------------------------------'------.PASS�2'------  OPEN 1,1,5,"11TEMP,S,W"  PRINT CHR$(13) "PASS 2 ";  FOR CUR�DEST�LINENO = 0 TO DEST�LINENO - 1    S$ = DEST�LINE$(CUR�DEST�LINENO)    IF VERBOSE THEN PRINT CUR�DEST�LINENO; "�=> " S$ : ELSE PRINT ".";    DO WHILE INSTR(S$, MK$) <> 0       S1 = INSTR(S$, MK$) : S2 = INSTR(S$, MK$, S1 + 2)       IF S2 = 0 THEN BEGIN         EXIT  ' EXIT LOOP FOR CASES WHERE A PI SYMBOL WAS USED IN THE SOURCE       BEND:ELSE BEGIN         CUR�TOK$ = MID$(S$, S1 + 2, S2 - S1 - 2)         GOSUB SWAP�OUT�LABEL�IN�CUR�TOK$�FOR�LINENO         S$ = LEFT$(S$, S1 - 1) + CUR�TOK$ + MID$(S$, S2 + 2)       BEND    LOOP    IF VERBOSE THEN PRINT "<= "; STR$(CUR�DEST�LINENO) + S$    PRINT #1, STR$(CUR�DEST�LINENO) + " " + S$  NEXT CUR�DEST�LINENO  GOSUB DUMP�VARS  FOR R = 0 TO 10    PRINT #1, STR$(32000 + R)  NEXT R  ' REMOVE 11.TOKENISE.BAS LINES?  F$ = "D�:D�" + DBL�QUOTE$ + "11TOKENIZED" + DBL�QUOTE$  F$ = F$ + ":IFDS<>0THEN?" + DBL�QUOTE$ + "DISC ERROR: "  F$ = F$ + DBL�QUOTE$ + ";DS$:ELSE?" + DBL�QUOTE$  F$ = F$ + ""  F$ = F$ + DBL�QUOTE$ + ":D�" + DBL�QUOTE$ + "11.POST"  PRINT #1, F$  CLOSE 1  PRINT""  PRINT "ELAPSED TIME:"; TI; "TICKS"  PRINT"�EDMA 0,$1FFF,$8022000,$2001:NEW RESTORE"  PRINT"RUN";  ' LOAD '11.TOKENIZE' FROM CACHE  BANK 128  POKE 208,2      ' NO# OF CHARS IN KEYBOARD BUFFER  POKE 688,13,13  ' RETURN CHARS  END'--------------.DECLARE�S$�VAR'--------------  ' DECLARE VAR(S) IN S$  S$ = MID$(CURR�SRC�LINE$, 10 - DEFINE�FLAG)  IGNORE�BRACKETS = 1  GOSUB PARSE�ARGUMENTS  IGNORE�BRACKETS = 0  ' SPLIT PARAMETERS  NEXT�LINE$ = ""  ' NEW LINE IF DIMENSIONING...  IF ARG�CNT < 0 THEN BEGIN    PARSER�ERROR$ = "?DECLARE PARAMETER MISSING IN LINE " + STR$(SRC�LINENO)    GOTO RETURN�TO�EDITOR�WITH�ERROR  BEND  FOR I = 0 TO ARG�CNT    VAR�NAME$ = ARGS$(I)    GOSUB PARSE�DECLARED�VAR  NEXT I  IF NEXT�LINE$ <> "" THEN BEGIN    DELETE�LINE�FLAG = 0    CURR�SRC�LINE$ = "^^" + NEXT�LINE$  BEND : ELSE BEGIN    DELETE�LINE�FLAG = 1  BEND  RETURN'------------------.PARSE�DECLARED�VAR'------------------  DIMENSION$ = ""  VALUE$ = ""  BKT�OPEN�IDX = INSTR(VAR�NAME$, "(")  BKT�CLOSE�IDX = INSTR(VAR�NAME$, ")")  EQUALS�IDX = INSTR(VAR�NAME$, "=")  IF EQUALS�IDX <> 0 THEN BEGIN  ' --- ASSIGNMENT    VALUE$ = MID$(VAR�NAME$, EQUALS�IDX + 1)    VAR�NAME$ = LEFT$(VAR�NAME$, EQUALS�IDX - 1)    TR$ = WHITESPACE$    S$ = VAR�NAME$    GOSUB STRIP�TR$�FROM�END    VAR�NAME$ = S$    S$ = VALUE$    GOSUB STRIP�TR$�FROM�BEGINNING    GOSUB STRIP�TR$�FROM�END    VALUE$ = S$    IF LEFT$(VALUE$, 1) = "$" THEN BEGIN      HX$ = MID$(VALUE$, 2)      GOSUB CHECK�HEX    BEND    IF LEFT$(VALUE$, 1) = "%" THEN BEGIN      BI$ = MID$(VALUE$, 2)      GOSUB CHECK�BINARY    BEND  BEND  IF BKT�OPEN�IDX <> 0 AND BKT�CLOSE�IDX <> 0 THEN BEGIN  ' --- DIMENSION    DIMENSION$ = MID$(VAR�NAME$, BKT�OPEN�IDX + 1, BKT�CLOSE�IDX - BKT�OPEN�IDX - 1)    CLEAN�VARNAME$ = LEFT$(VAR�NAME$, BKT�OPEN�IDX - 1)    S$ = DIMENSION$    DONT�MARK�LABEL = 1    GOSUB REPLACE�VARS�AND�LABELS    DONT�MARK�LABEL = 0    DIMENSION$ = S$    VAR�NAME$ = CLEAN�VARNAME$  ' CHECK FOR DEFINE TOKENS    DELETE�LINE�FLAG = 0  BEND  TY = ��Я����  ' VAR TYPE  T$ = RIGHT$(VAR�NAME$, 1)  ' TYPE (IF ANY) IN T$  IF VERBOSE THEN BEGIN    PRINT "ADDING ";  BEND  IF INSTR("%&$", T$) = 0 THEN BEGIN    T$ = ""    TY = ��Я����  BEND  IF DEFINE�FLAG = 1 THEN BEGIN    TY = ��Я���  BEND  IF T$ = "%" THEN BEGIN    TY = ��Я���  BEND  IF T$ = "$" THEN BEGIN    TY = ��Я���  BEND  IF T$ = "&" THEN BEGIN    TY = ��Я����  BEND  VAR�TABLE$(TY, ELEMENT�CNT(TY)) = VAR�NAME$  IF DIMENSION$ <> "" THEN BEGIN    ID = ELEMENT�CNT(TY)    GOSUB GENERATE�VARNAME  ' FETCH VARNAME IN GEN�VARNAME$    IF DEFINE�FLAG = 0 THEN BEGIN      NEXT�LINE$ = NEXT�LINE$ + "DIM " + GEN�VARNAME$ + T$ + "(" + DIMENSION$ + "):"    BEND  BEND  IF VALUE$ <> "" THEN BEGIN    ID = ELEMENT�CNT(TY)    GOSUB GENERATE�VARNAME     IF DEFINE�FLAG=0 THEN BEGIN      NEXT�LINE$ = NEXT�LINE$ + GEN�VARNAME$ + T$ + "=" + VALUE$ + ":"    BEND  BEND  IF DEFINE�FLAG = 1 THEN BEGIN    DEFINE�VAL$(ELEMENT�CNT(TY)) = VALUE$  BEND  IF VERBOSE THEN BEGIN    PRINT VAR�NAME$; "�: "; ELEMENT�CNT(TY)  BEND  ELEMENT�CNT(TY) = ELEMENT�CNT(TY) + 1  RETURN'---------------.SET�OUTPUT�FILE'---------------  S$ = MID$(CURR�SRC�LINE$, 8)  GOSUB PARSE�ARGUMENTS  IF ARG�CNT <> 0 THEN BEGIN    PRINT "?INVALID PARAMETERS IN LINE ";SRC�LINENO    END  BEND  S$ = ARGS$(0)  TR$ = DBL�QUOTE$  GOSUB STRIP�TR$�FROM�BEGINNING  GOSUB STRIP�TR$�FROM�END  ' QUOTES LEFT & RIGHT  IF VERBOSE THEN BEGIN    PRINT "SETTING OUTPUT FILE TO " + S$ + "�"  BEND  OF$ = S$  DELETE�LINE�FLAG = 1  ' DISABLE PASSTHROUGH  RETURN'------------------.ADD�TO�LABEL�TABLE'------------------  IF VERBOSE THEN BEGIN    PRINT "LABEL "; CURR�SRC�LINE$; " AT LINE "; DEST�LINENO  BEND  DELETE�LINE�FLAG = 1  LABEL�NAME$(LABEL�CNT) = MID$(CURR�SRC�LINE$, 2)  LABEL�LINENO(LABEL�CNT) = DEST�LINENO + 1  LABEL�CNT = LABEL�CNT + 1  ' INCREASE LABEL COUNT  RETURN'---------------------------.RETURN�TO�EDITOR�WITH�ERROR'---------------------------  BANK 4  ' SET ERROR MAILBOX FLAG  FOR R=1 TO LEN(PARSER�ERROR$)    POKE $4FF30 + R - 1, ASC(MID$(PARSER�ERROR$, R, 1))  NEXT R  POKE $4FF30 + R - 1, 0  POKE $FF09, MOD(SRC�LINENO, 256)  POKE $FF0A, SRC�LINENO / 256  POKE $FF07, PEEK($FF07) OR 2  ' SET AUTOJUMP FLAG  DCLOSE  GOTO CHAIN�EDITOR'------------------------.STRIP�TR$�FROM�BEGINNING'------------------------  ' -- STRIP TR$ FROM BEGINNING OF STRING IN S$ --  DO WHILE INSTR(TR$, (LEFT$(S$, 1)))    S$ = MID$(S$, 2)  LOOP  RETURN'------------------.STRIP�TR$�FROM�END'------------------  ' -- STRIP CHARACTERS IN TR$ FROM END OF S$ --  DO WHILE INSTR(TR$, RIGHT$(S$, 1))    S$ = LEFT$(S$, LEN(S$) - 1)  LOOP  RETURN'---------------.PARSE�ARGUMENTS'---------------  ' -- PARSE ARGUMENTS --  '     IN: S$ = STRING  '    OUT: ARGS$(X) = ARGUMENT LIST, ARG�CNT = ARGUMENT COUNT  '         ARGS$(0) = FIRST ARG, ARGS$(1) = SECOND ARG...    DELIM$ = ",;"  ARG�CNT = 0  ARGS�LIST$ = S$  ARGS�LIST�LEN = LEN(S$)  IGNORE�DELIM�FLAG = 0  IF ARGS�LIST�LEN = 0 THEN BEGIN    ARG�CNT = -1    RETURN  ' NO STRING  BEND  FOR ARGS�IDX = 0 TO 31    ARGS$(ARGS�IDX) = ""  NEXT ARGS�IDX  FOR ARGS�IDX = 1 TO ARGS�LIST�LEN    CUR�CH$ = MID$(ARGS�LIST$, ARGS�IDX, 1)    IF CUR�CH$ = "(" AND IGNORE�BRACKETS = 1 THEN IGNORE�DELIM�FLAG = 1    IF CUR�CH$ = ")" AND IGNORE�BRACKETS = 1 THEN IGNORE�DELIM�FLAG = 0    IF INSTR(DELIM$, CUR�CH$) = 0 OR IGNORE�DELIM�FLAG = 1 THEN BEGIN      ARGS$(ARG�CNT) = ARGS$(ARG�CNT) + CUR�CH$    BEND : ELSE BEGIN      S$ = ARGS$(ARG�CNT)      TR$ = " "      GOSUB STRIP�TR$�FROM�BEGINNING      GOSUB STRIP�TR$�FROM�END      ARGS$(ARG�CNT) = S$      ARG�CNT = ARG�CNT + 1    BEND  NEXT ARGS�IDX  S$ = ARGS$(ARG�CNT)  TR$=" "  GOSUB STRIP�TR$�FROM�BEGINNING  GOSUB STRIP�TR$�FROM�END  ARGS$(ARG�CNT) = S$  S$ = ARGS�LIST$  ' RESTORE S$  RETURN'-----------------------.REPLACE�VARS�AND�LABELS'-----------------------  ' -- REPLACE VARS & LABELS IN SOURCE STRING --  '    IN:   S$ = SOURCE STRING  '    OUT:  S$ = DEST STRING WITH REPLACED ITEMS    IF LEFT$(S$, 2) = "^^" THEN BEGIN    S$ = RIGHT$(S$,LEN(S$)-2)    RETURN  BEND  QUOTE�FLAG = 0  A$ = ""  CUR�TOK$ = ""  STRUCT�IDX = 0  EXPECTING�LABEL = 0  DEFAULT�DELIM$ = "?<>=+-#*/^,.:;() "  DELIM$ = DEFAULT�DELIM$  FOR RV�IDX = 1 TO LEN(S$)    CUR�CH$ = MID$(S$, RV�IDX, 1)    IF CUR�CH$ = ":" AND QUOTE�FLAG = 0 THEN BEGIN      STRUCT�IDX = 0    BEND    IF STRUCT�IDX AND CUR�CH$ = "(" THEN BEGIN      DELIM$ = DEFAULT�DELIM$    BEND    IF STRUCT�IDX AND CUR�CH$ = ")" THEN BEGIN      DELIM$ = DEFAULT�DELIM$ + "DPUB"    BEND    IF CUR�CH$ = DBL�QUOTE$ THEN BEGIN      QUOTE�FLAG = ABS(QUOTE�FLAG - 1)      IF QUOTE�FLAG = 1 THEN BEGIN        GOSUB CHECK�TOKEN�FOR�SUBBING        A$ = A$ + CUR�TOK$        CUR�TOK$ = ""      BEND : ELSE BEGIN        A$ = A$ + CUR�CH$        CUR�CH$ = ""      BEND    BEND    IF QUOTE�FLAG = 1 THEN BEGIN      A$ = A$ + CUR�CH$      GOTO RVAL�SKIP    BEND    IF INSTR(DELIM$, CUR�CH$) <> 0 THEN BEGIN      GOSUB CHECK�TOKEN�FOR�SUBBING      A$ = A$ + CUR�TOK$      CUR�TOK$ = ""      IF CUR�CH$ = " " THEN CUR�CH$ = ""      A$ = A$ + CUR�CH$    BEND : ELSE BEGIN      CUR�TOK$ = CUR�TOK$ + CUR�CH$    BEND.RVAL�SKIP  NEXT  GOSUB CHECK�TOKEN�FOR�SUBBING   S$ = A$ + CUR�TOK$  RETURN  END'-----------------------.CHECK�TOKEN�FOR�SUBBING'-----------------------  IF CUR�TOK$ = "" OR CUR�TOK$ = "_" THEN RETURN  ' DECIMAL NUMBER CHECK  ' - - - - - - - - - -  IF VAL(CUR�TOK$) <> 0 THEN BEGIN    EXPECTING�LABEL = 0    RETURN  ' NEVER CHANGE NUMBERS  BEND  IF CUR�TOK$ = "0" THEN BEGIN    CUR�TOK$="."    RETURN  ' STUPID MS BASIC OPTIMIZATION  BEND  ' CHECK IF WE SHOULD MARK THIS EXPECTED LABEL  ' - - - - - - - - - - - - - - - - - - - - - -  IF EXPECTING�LABEL AND DONT�MARK�LABEL = 0 THEN BEGIN    GOSUB MARK�CUR�TOK$�LABEL    EXPECTING�LABEL = 0    RETURN  ' REPLACE LABEL  BEND  IF CUR�TOK$ = "GOTO" THEN NEXT�LINE�FLAG = 1  ' ARE WE EXPECTING A LABEL NEXT?  ' - - - - - - - - - - - - - - -  IF CUR�TOK$ = "GOTO" OR CUR�TOK$ = "GOSUB" OR CUR�TOK$ = "TRAP" THEN BEGIN    EXPECTING�LABEL = 1  BEND  ' CHECK HEX VALUE  ' - - - - - - - -  IF LEFT$(CUR�TOK$, 1) = "$" THEN BEGIN    HX$ = MID$(CUR�TOK$, 2)    GOSUB CHECK�HEX    RETURN   BEND  ' CHECK BINARY VALUE  ' - - - - - - - - -  IF LEFT$(CUR�TOK$, 1) = "%" THEN BEGIN    BI$ = MID$(CUR�TOK$, 2)    GOSUB CHECK�BINARY    RETURN  BEND  TOK�NAME$ = " " + CUR�TOK$ + " "  ' IGNORE EXISTING VOCABULARY OF FUNCTIONS/COMMANDS  ' - - - - - - - - - - - - - - - - - - - - - - - -   FOR T = 0 TO 6    IF INSTR(TOKENS$(T), TOK�NAME$) <> 0 THEN BEGIN      LC$ = CUR�TOK$      GOSUB CHECK�IF�COMMAND�TRIGGERS�SHITTY�SYNTAX      RETURN    BEND  NEXT  ' CHECK TO SWAP OUT VARIABLES WITH SHORT NAMES  ' - - - - - - - - - - - - - - - - - - - - - -  DID�REPLACE�FLAG = 0  ' DID REPLACE FLAG  T$ = RIGHT$(CUR�TOK$, 1)  TY = ��Я����  IF T$ = "%" THEN TY = ��Я���  IF T$ = "$" THEN TY = ��Я���  IF T$ = "&" THEN TY = ��Я����  FOR ID = 0 TO ELEMENT�CNT(TY)    IF CUR�TOK$ = VAR�TABLE$(TY, ID) THEN BEGIN      GOSUB GENERATE�VARNAME      CUR�TOK$ = GEN�VARNAME$ + TYPE�IDENT$(TY)      ID = ELEMENT�CNT(TY)      DID�REPLACE�FLAG = 1    BEND  NEXT ID  IF DID�REPLACE�FLAG = 1 THEN RETURN  ' CHECK DEFINES TABLE TOO  ' - - - - - - - - - - - -  FOR ID = 0 TO ELEMENT�CNT(��Я���)    IF CUR�TOK$ = VAR�TABLE$(��Я���, ID) THEN BEGIN      CUR�TOK$ = DEFINE�VAL$(ID)      RETURN    BEND  NEXT ID  ' CHECK FOR STRUCT NAMES  ' ----------------------  FOUND�STRUCT�IDX = -1  FOR ID = 0 TO STRUCT�IDX - 1  ' CHECK STRUCT NAMES    IF CUR�TOK$ = STRUCT�NAME$(ID) THEN BEGIN      GOSUB CHECK�FOR�CREATION�OF�STRUCT�OBJECT      FOUND�STRUCT�IDX = ID      ID = STRUCT�IDX - 1  ' CREATE NEW STRUCT OBJECT    BEND  NEXT ID  IF FOUND�STRUCT�IDX <> -1 THEN BEGIN    RETURN  BEND  ' �� TOKEN CHECK  ' - - - - - - -  IF ASC(CUR�TOK$) = 222 THEN RETURN  ' CHECK FOR DUMB COMMANDS  ' - - - - - - - - - - - -  ' ����: � THINK LC$ BELOW SHOULD BE REPLACED WITH TOK�NAME$  ' ----  IF INSTR(DUMB�CMDS$, LC$) <> 0 AND _    (CUR�TOK$ = "R" OR CUR�TOK$ = "P"      OR CUR�TOK$ = "U8" OR CUR�TOK$ = "W") THEN BEGIN    RETURN  BEND.UNRESOLVED�CUR�TOK$  PARSER�ERROR$ = "?UNRESOLVED IDENTIFIER: '" + CUR�TOK$ + "' IN LINE " + STR$(SRC�LINENO)  SLEEP 1  GOTO RETURN�TO�EDITOR�WITH�ERROR  RETURN'---------------------------------------.CHECK�IF�COMMAND�TRIGGERS�SHITTY�SYNTAX'---------------------------------------  ' CHECK IF COMMAND TRIGGERS SHITTY SYNTAX MODE  ' TODO: COMPARE UBIK'S LOGIC HERE VERSUS MY OWN 4079 LC$="DOPEN" LOGIC  RETURN'-------------.MARK�CUR�TOK$�LABEL'-------------  CUR�TOK$ = MK$ + CUR�TOK$ + MK$  RETURN'-------------------------------.SWAP�OUT�LABEL�IN�CUR�TOK$�FOR�LINENO'-------------------------------  DID�REPLACE�FLAG = 0  FOR ID = 0 TO LABEL�CNT - 1    IF CUR�TOK$ = LABEL�NAME$(ID) THEN BEGIN      CUR�TOK$ = STR$(LABEL�LINENO(ID))      ID = LABEL�CNT      DID�REPLACE�FLAG = 1    BEND  NEXT ID  IF DID�REPLACE�FLAG THEN RETURN  PARSER�ERROR$ = "?UNRESOLVED LABEL: '" + CUR�TOK$ + "' IN LINE" + STR$(MAP�DEST�TO�SRC�LINENO%(CUR�DEST�LINENO - 1))  SLEEP 1  GOTO RETURN�TO�EDITOR�WITH�ERROR  RETURN'------------.CHECK�BINARY'------------  BR = 0  ' RESULT  FOR B = 0 TO LEN(BI$) - 1    BC$ = MID$(BI$, LEN(BI$) - B, 1)    IF BC$ <> "1" AND BC$ <> "0" THEN BEGIN      BANK 4      POKE $FF08, 132      GOTO UNRESOLVED�CUR�TOK$    BEND    IF BC$="1" THEN BR=BR+BIN�CONV(B)  NEXT B  RETURN'---------.CHECK�HEX'---------  TRAP ILLEGAL�HEX�HANDLER  VL = DEC(HX$)  TRAP  RETURN'-------------------.ILLEGAL�HEX�HANDLER'-------------------  TRAP  BANK 4  POKE $FF08, 131  ' SET ILLEGAL HEX  GOTO UNRESOLVED�CUR�TOK$  ' JUMP INTO ERROR HANDLER'----------------.GENERATE�VARNAME'----------------  ' GENERATE VARNAME FROM INDEX  IF ID < 26 THEN BEGIN    GEN�VARNAME$ = CHR$(65 + ID)    RETURN  BEND  N2 = MOD(ID, 26)  N1 = INT(ID / 26) - 1  GEN�VARNAME$ = CHR$(65 + N1) + CHR$(65 + N2)  IF GEN�VARNAME$="DO" THEN GEN�VARNAME$="D1"  ' AVOID ANY BASIC TERMS AS VAR NAMES  IF GEN�VARNAME$="GO" THEN GEN�VARNAME$="G1"  IF GEN�VARNAME$="TO" THEN GEN�VARNAME$="T1"  IF GEN�VARNAME$="DS" THEN GEN�VARNAME$="D2"  IF GEN�VARNAME$="DT" THEN GEN�VARNAME$="D3"  IF GEN�VARNAME$="EL" THEN GEN�VARNAME$="E1"  IF GEN�VARNAME$="ER" THEN GEN�VARNAME$="E2"  IF GEN�VARNAME$="FN" THEN GEN�VARNAME$="F1"  IF GEN�VARNAME$="IF" THEN GEN�VARNAME$="I1"  RETURN  STOP'------------.GET�FILENAME'------------  BANK 4  BA = $FF00  IF PEEK(BA+0) = ASC("S") AND PEEK(BA+1) = ASC("K") THEN BEGIN    VERBOSE = PEEK($FF07) AND 8    F$ = ""    A = BA + $10    DO WHILE PEEK(A) <> 0      F$ = F$ + CHR$(PEEK(A))      A = A + 1    LOOP    IF PEEK($FF07) AND 1 THEN RETURN    PRINT "FILENAME? " + F$    PRINT "�";  BEND  INPUT "FILENAME"; A$  IF A$ = "" THEN BEGIN    PRINT "NO FILENAME SET"    END  BEND  POKE BA, ASC("S")  POKE BA + 1, ASC("K")  FOR R = 1 TO 16    POKE BA + 8 + R - 1, ASC(MID$(A$, R, 1))  NEXT R  F$ = A$  RETURN'-------------.SAVE�FILENAME'-------------  AD = $FF30  BANK 4  FOR R = 0 TO 16    POKE AD + R, 0  NEXT R  IF OF$ <> "" THEN BEGIN    TF$ = OF$  BEND : ELSE BEGIN    TF$="ELEVEN.OUT"  BEND  FOR R = 1 TO LEN(TF$)    POKE AD + R - 1, ASC(MID$(TF$, R, 1))  NEXT R  RETURN'------------.CHAIN�EDITOR'------------  GET A$  IF A$ <> "" THEN BEGIN    INPUT ZZ    IF ZZ=1 THEN STOP  BEND  PRINT "�EDMA 0,$D400,$8000000,$2001:NEW RESTORE"  PRINT "RUN";  ' LOAD '11.EDIT' FROM CACHE  BANK 128  POKE 208, 2      ' NO# OF CHARS IN KEYBOARD BUFFER  POKE 688, 13, 13 ' RETURN CHARS  END'-------------.IS�S$�DEFINED'-------------  INSIDE�IFDEF = 1  FOR K = 0 TO ELEMENT�CNT(��Я���)    IF VAR�TABLE$(��Я���, K) = S$ THEN BEGIN      INSIDE�IFDEF = 0    BEND  NEXT K  RETURN'----------------------.READ�IN�STRUCT�DETAILS'----------------------  CURR�SRC�LINE$ = MID$(CURR�SRC�LINE$, 9)  GOSUB READ�NEXT�TOKEN  ' GET NEXT TOKEN IN S$  IF S$ = "" THEN BEGIN    PRINT "ERROR: NO STRUCT NAME FOUND"    SLEEP 1    RETURN  BEND  STRUCT�NAME$(STRUCT�IDX) = S$  STRUCT�VARS$(STRUCT�IDX) = CURR�SRC�LINE$  STRUCT�IDX = STRUCT�IDX + 1  RETURN'---------------.READ�NEXT�TOKEN'---------------  ' READ NEXT TOKEN FROM CURR�SRC�LINE$ INTO S$  S$ = CURR�SRC�LINE$  GOSUB STRIP�TR$�FROM�BEGINNING  GOSUB STRIP�TR$�FROM�END  CURR�SRC�LINE$ = S$  SF$ = " "  SF = 0  IF LEFT$(S$, 1) = DBL�QUOTE$ THEN BEGIN    SF$ = DBL�QUOTE$ + ", "    SF = 2  BEND  A = INSTR(CURR�SRC�LINE$, SF$)  IF A <> 0 THEN BEGIN    S$ = MID$(CURR�SRC�LINE$, 1, INSTR(CURR�SRC�LINE$, SF$) + SF - 1)    CURR�SRC�LINE$ = MID$(CURR�SRC�LINE$, INSTR(CURR�SRC�LINE$, SF$) + SF + 1)  BEND  IF A = 0 THEN BEGIN    S$ = CURR�SRC�LINE$    CURR�SRC�LINE$ = ""  BEND  RETURN'--------------.READ�NEXT�LINE'--------------  ' READ NEXT LINE INTO CURR�SRC�LINE$  BANK 0  CUR�LINE�LEN = PEEK(CURR�ATTIC�ADDR)  CURR�SRC�LINE$ = LEFT$(BLANK�LINE$, CUR�LINE�LEN)  SRC�LINE�PTR = POINTER(CURR�SRC�LINE$)  SRC�LINEBUFF�PTR = $10000 + WPEEK(SRC�LINE�PTR + 1)  CURR�ATTIC�ADDR = CURR�ATTIC�ADDR + 1  IF CUR�LINE�LEN <> 0 THEN BEGIN    EDMA 0, CUR�LINE�LEN, CURR�ATTIC�ADDR, SRC�LINEBUFF�PTR    CURR�ATTIC�ADDR = CURR�ATTIC�ADDR + CUR�LINE�LEN  BEND  CUR�SRC�LINENO = CUR�SRC�LINENO + 1  TR$ = WHITESPACE$  S$ = CURR�SRC�LINE$  GOSUB STRIP�TR$�FROM�BEGINNING  CURR�SRC�LINE$ = S$  QUOTE�FLAG = 0    ' QUOTES ON  CUT�TAIL�IDX = 0  ' CUT CHARS FROM TAIL  BANK 1  RETURN'-------------------------.SINGLE�QUOTE�COMMENT�TRIM'-------------------------  CUT�TAIL�IDX = INSTR(CURR�SRC�LINE$, "'")  ' SINGLE QUOTE  IF CUT�TAIL�IDX <> 0 THEN BEGIN    IF INSTR(CURR�SRC�LINE$, DBL�QUOTE$) <> 0 THEN BEGIN  ' DOUBLE QUOTE      CUT�TAIL�IDX = 0      BANK 0      CUR�LINE�LEN�MINUS�ONE = PEEK(SRC�LINE�PTR) - 1      CUR�LINEBUFF�ADDR = WPEEK(SRC�LINE�PTR + 1)      BANK 1      FOR R = 0 TO CUR�LINE�LEN�MINUS�ONE        CHR = PEEK(CUR�LINEBUFF�ADDR + R)        IF CHR = DBL�QUOTE�CHAR THEN BEGIN          QUOTE�FLAG = ABS(QUOTE�FLAG - 1)        BEND : ELSE BEGIN          IF CHR = SNGL�QUOTE�CHAR AND QUOTE�FLAG=0 THEN BEGIN            CUT�TAIL�IDX = R + 1            R = 999          BEND        BEND      NEXT    BEND    IF CUT�TAIL�IDX THEN BEGIN      CURR�SRC�LINE$ = LEFT$(CURR�SRC�LINE$, CUT�TAIL�IDX - 1)    BEND  BEND  RETURN'----------------------------.PARSE�PREPROCESSOR�DIRECTIVE'----------------------------  IF INSTR(CURR�SRC�LINE$, "IFDEF") = 2 THEN BEGIN    S$ = MID$(CURR�SRC�LINE$, 8)    GOSUB IS�S$�DEFINED    DELETE�LINE�FLAG = 1  BEND  IF INSTR(CURR�SRC�LINE$, "ENDIF") = 2 THEN BEGIN    INSIDE�IFDEF = 0    DELETE�LINE�FLAG = 1  BEND  IF INSTR(CURR�SRC�LINE$, "DEFINE") = 2 THEN BEGIN    DEFINE�FLAG = 1    GOSUB DECLARE�S$�VAR    DEFINE�FLAG = 0  BEND  IF INSTR(CURR�SRC�LINE$, "DECLARE") = 2 THEN BEGIN    DEFINE�FLAG = 0    GOSUB DECLARE�S$�VAR   BEND  IF INSTR(CURR�SRC�LINE$,"OUTPUT")=2 THEN BEGIN    GOSUB SET�OUTPUT�FILE  BEND  IF INSTR(CURR�SRC�LINE$,"STRUCT")=2 THEN BEGIN    GOSUB READ�IN�STRUCT�DETAILS    DELETE�LINE�FLAG = 1  BEND  RETURN'-----------------------------------.CHECK�FOR�CREATION�OF�STRUCT�OBJECT'-----------------------------------  CO$ = S$  CURR�SRC�LINE$ = S$  ZZ = -1  ' PRESERVE ORIGINAL STRING  GOSUB READ�NEXT�TOKEN  ' READ NEXT TOKEN FROM CURR�SRC�LINE$ INTO S$  FOR ZI = 0 TO STRUCT�IDX    IF S$=STRUCT�NAME$(ZI) THEN BEGIN      ZZ = ZI      ZI = STRUCT�IDX    BEND  NEXT ZI  IF ZZ = -1 THEN BEGIN    CURR�SRC�LINE$ = CO$    S$ = CO$    RETURN  BEND  GOSUB READ�NEXT�TOKEN  SK$ = S$  ' GET STRUCT OBJECT NAME  BL = INSTR(SK$, "(")  ' *** FOUND IT, SO MAKE DIM'S FOR EACH MEMBER VAR  S$ = STRUCT�VARS$(ZZ)  GOSUB PARSE�ARGUMENTS  ' PARSE ARGS INTO ARGS$(), ARG�CNT  ZZ = 1  SZ = 0  FOR ZI = 0 TO ARG�CNT    IF ARGS$(ZI) <> "" THEN BEGIN      PRINT "ARGS$(";ZI;")=";ARGS$(ZI)      IF BL = 0 THEN BEGIN        S$ = SK$ + "�" + ARGS$(ZI)        PRINT "STRUCT: "; S$        VAR�NAME$ = S$        GOSUB PARSE�DECLARED�VAR       BEND      IF BL <> 0 THEN BEGIN        S$ = LEFT$(SK$, BL - 1) + "�" + ARGS$(ZI) + MID$(SK$, BL)        PRINT "STRUCT: ";S$        VAR�NAME$ = S$        GOSUB PARSE�DECLARED�VAR        STRUCT�FIELDS$(SZ) = VAR�NAME$        SZ = SZ + 1      ' IF NEXT�LINE$<>"" THEN DELETE�LINE�FLAG=0:CURR�SRC�LINE$="^^"+NEXT�LINE$:ELSE DELETE�LINE�FLAG=1    BEND  NEXT ZI  S$ = NEXT�LINE$  GOSUB SAFE�ADD�TO�CURRENT�OR�NEXT�LINE  NEXT�LINE$ = ""  ' SAFE ADD CURR�DEST�LINE$+S$ TO CURRENT DEST�LINE$(DEST�LINENO)  GOSUB READ�NEXT�TOKEN  IF S$ <> "=" THEN BEGIN    CURR�SRC�LINE$=""    RETURN  BEND  GOSUB READ�NEXT�TOKEN  SZ=0  SR=0  SM=0  ' READ NEXT TOKEN FROM CURR�SRC�LINE$ INTO S$  DO WHILE S$ <> ""    IF S$ = "_" THEN BEGIN      SRC�LINENO = SRC�LINENO + 1      GOSUB READ�NEXT�LINE      GOTO CFCOSO�SKIP  ' READ NEXT LINE    BEND    IF SM = 0 AND S$ <> "[" THEN BEGIN      PRINT "ERROR: EXPECTED ["      SLEEP 1      STOP    BEND    IF SM = 0 AND S$ = "[" THEN BEGIN      SM = 1      GOTO CFCOSO�SKIP    BEND    IF SM = 1 AND S$ <> "[" AND S$ <> "]" THEN BEGIN      PRINT "ERROR: EXPECTED [ OR ]"      SLEEP 1      STOP    BEND    IF SM = 2 THEN BEGIN      IF LEFT$(S$, 1) = "]" THEN BEGIN        SR = SR + 1        SZ = 0        SM = 1        S$ = ""        GOTO CFCOSO�NEXTROW  ' NEXT ROW      BEND      ' IF LEFT$(S$, 1) = DBL�QUOTE$ THEN BEGIN      ' SS$ = S$      ' TR$ = ""      ' DO WHILE RIGHT$(SS$, 2) <> (DBL�QUOTE$ + ",")      '   GOSUB READ�NEXT�TOKEN      '   SS$ = SS$ + S$      '   PRINT SS$      ' LOOP      ' S$ = SS$      ' TR$ = WHITESPACE$      ' STOP      IF RIGHT$(S$, 1) = "," THEN BEGIN        S$ = LEFT$(S$, LEN(S$) - 1)      BEND      S$ = STRUCT�FIELDS$(SZ) + "(" + STR$(SR) + ")=" + S$      GOSUB REPLACE�VARS�AND�LABELS      GOSUB SAFE�ADD�TO�CURRENT�OR�NEXT�LINE      S$ = ""      SZ = SZ + 1  ' SAFE ADD TO DEST�LINE$(DEST�LINENO).CFCOSO�NEXTROW    BEND    IF SM = 1 AND S$ = "[" THEN BEGIN      SM = 2      SZ = 0    BEND    IF SM = 1 AND S$ = "]" THEN BEGIN      SM = 0    BEND.CFCOSO�SKIP    GOSUB READ�NEXT�TOKEN  ' READ NEXT TOKEN FROM CURR�SRC�LINE$ INTO S$  LOOP  S$ = ""  CURR�SRC�LINE$ = ""  NEXT�LINE$ = ""  ZZ$ = "Z"  RETURN'---------------------------------.CHECK�FOR�CONTINUE�ONTO�NEXT�LINE'---------------------------------  IF RIGHT$(CURR�DEST�LINE$, 1) = "_" THEN BEGIN    CURR�DEST�LINE$ = LEFT$(CURR�DEST�LINE$, LEN(CURR�DEST�LINE$) - 1)    NEXT�LINE�FLAG = 0    CONT�NEXT�LINE�FLAG = 1  BEND: ELSE BEGIN    CONT�NEXT�LINE�FLAG = 0  BEND  RETURN'--------------------------------.SAFE�ADD�TO�CURRENT�OR�NEXT�LINE'--------------------------------  ' --- SAFE ADD CURR�DEST�LINE$+S$ TO CURRENT OR NEXT DEST�LINE$(DEST�LINENO)  IF LEN(CURR�DEST�LINE$) + LEN(S$) + LEN(STR$(DEST�LINENO)) >= 159 THEN BEGIN    NEXT�LINE�FLAG=1  BEND  IF ZZ$ <> "" THEN BEGIN    S$ = ""    ZZ$ = ""    RETURN  ' FORCE S$ TO EMPTY  BEND  IF NEXT�LINE�FLAG = 1 THEN BEGIN    DEST�LINE$(DEST�LINENO) = CURR�DEST�LINE$    CURR�DEST�LINE$ = S$    MAP�DEST�TO�SRC�LINENO%(DEST�LINENO) = SRC�LINENO    DEST�LINENO = DEST�LINENO + 1    NEXT�LINE�FLAG = 0  BEND : ELSE BEGIN  ' -- ADD TO CURR�DEST�LINE$    IF CURR�DEST�LINE$ <> "" AND CONT�NEXT�LINE�FLAG = 0 AND RIGHT$(CURR�DEST�LINE$,1) <> ":" THEN BEGIN      CURR�DEST�LINE$ = CURR�DEST�LINE$ + ":"    BEND        CURR�DEST�LINE$ = CURR�DEST�LINE$ + S$  BEND  IF VERBOSE THEN PRINT "<<" DEST�LINENO; S$  RETURN'---------.DUMP�VARS'---------  FOR TY = 0 TO 3    FOR ID = 0 TO ELEMENT�CNT(TY)      GOSUB GENERATE�VARNAME  ' GET GEN�VARNAME$ (OPTIMISED VAR NAME)      PRINT#1, STR$(CUR�DEST�LINENO) + " REM " _        + VAR�TABLE$(TY,ID) + " = :" + GEN�VARNAME$ + TYPE�IDENT$(TY) + ":"      CUR�DEST�LINENO = CUR�DEST�LINENO + 1    NEXT ID  NEXT TY  RETURN
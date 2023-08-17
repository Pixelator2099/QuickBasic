DECLARE FUNCTION sran$ (top!)
DECLARE FUNCTION ran! (top!)

'**********************************************************************
'* FakeDos 5.0   - By Steven Hanov (hanovs@wchat.on.ca)               *
'*               - Released to public domain on 96/06/22              *
'**********************************************************************
'Use this program to scare your system administrator. Run it, and then
'type format c: (or the drive letter you want) and it will simulate a
'format. Also try DEL *.* and then DIR. You can change the files in
'the directory by modifying the data statements. Exit by typing EXIT.
'
'This program is VERY realistic. While I was developing it at school,
'the supervisor actually kicked me out of the room for being in DOS.
'When I used it on April Fools, one computer teacher gripped my
'shoulder, HARD. Another time a different teacher had a
'hilarious look on her face when she saw the computer formatting
'drive H:. BE CAREFUL! ALTHOUGH THIS PROGRAM DOES NOT AFFECT THE DRIVE,
'I AM NOT RESPONSIBLE FOR ANY BODILY HARM INFLICTED ON YOU BY
'PANICKED SYSTEM ADMINISTRATORS.
'If you make improvements or add additional commands, I'd appreciate a
'copy by e-mail.
'**********************************************************************

dr$ = "X": lab$ = "SYS": path$ = ":\COURSES"
SLEEP 1
PRINT
PRINT "IBM DOS Version 5.00"
PRINT "             (C)Copyright International April Fools Day 1981-1991."
PRINT "             (C)Copyright Microsoft Corp 1981-1991"
PRINT
Start:
a$ = ""
PRINT dr$; path$; ">";
LINE INPUT a$
IF a$ = "" THEN GOTO Start
IF a$ = "exit" THEN SYSTEM
IF LEFT$(a$, 2) = "cd" THEN path$ = ":" + UCASE$(MID$(a$, 3, LEN(a$))): v = 0: PRINT : GOTO Start
IF RIGHT$(a$, 1) = ":" AND LEN(a$) = 2 THEN dr$ = UCASE$(LEFT$(a$, 1)): path$ = ":\": PRINT : v = 0: GOTO Start
IF a$ = "cls" THEN CLS : GOTO Start
IF a$ = "dir" THEN GOTO DIR
IF a$ = "del *.*" THEN
   PRINT "All files in directory will be deleted!"
   INPUT "Are you sure (Y/N)"; s$
   IF s$ = "y" THEN SLEEP 1: PRINT : v = 1
   GOTO Start
   END IF
  
IF LEFT$(a$, 6) = "format" THEN
   d$ = UCASE$(RIGHT$(a$, 2)): fl = 0
   IF d$ = "B:" OR d$ = "A:" THEN fl = 1
   IF fl = 0 THEN
        PRINT
        PRINT "WARNING: ALL DATA ON NON-REMOVABLE DISK"
        PRINT "DRIVE "; d$; " WILL BE LOST!"
        INPUT "Proceed with Format (Y/N)"; c$
        IF UCASE$(c$) <> "Y" THEN PRINT : GOTO Start
   END IF

   IF fl = 1 THEN
        PRINT "Insert new diskette for drive "; d$
        PRINT "and press ENTER when ready..."
        DO WHILE INKEY$ <> CHR$(13): LOOP
   END IF
   
   PRINT
   PRINT "Checking existing disk format.": SLEEP 2
   m$ = "250M": b$ = "254,102,458"
   IF d$ = "A:" THEN m$ = "1.2M": b$ = "1,245,234"
   IF d$ = "B:" THEN m$ = "1.44M": b$ = "1,457,664"
   PRINT "Verifying "; m$
   l = CSRLIN: IF l = 24 THEN l = 23
   FOR s = 1 TO 100
      PRINT s; "% complete"
      SLEEP 2
      LOCATE l, 1
   NEXT
   LOCATE l, 1: PRINT "Format complete.          "
   PRINT
   INPUT "Volume Label (11 characters, ENTER for none)"; lab$
   PRINT
   PRINT TAB(13 - LEN(b$)); b$; TAB(15); "bytes total disk space"
   PRINT TAB(13 - LEN(b$)); b$; TAB(15); "bytes available on disk"
   PRINT
   PRINT TAB(10); "512 bytes in each allocation unit."
   PRINT TAB(8); "2,847 allocation units available on disk."
   PRINT : PRINT "Volume Serial Number is 3330-OBD4"
   PRINT
   INPUT "Format another (Y/N)"; c$
  
   DO
   SLEEP 1
   PRINT
   PRINT "Unable to read from drive "; d$
   INPUT "Abort, Retry, Fail"; c$
   LOOP

   GOTO Start
   END IF
PRINT "Bad command or filename": PRINT : GOTO Start

DIR:
PRINT
PRINT " Volume in drive "; dr$; " is "; lab$
PRINT " Volume Serial Number is 2AF8-7CD9"
PRINT " Directory of "; dr$; path$
PRINT
IF v = 1 THEN PRINT "  No files in current directory.": PRINT : GOTO Start

total = 0: f = 0
RESTORE
DO
READ a$, b$, c
IF c = -2 THEN EXIT DO
   d$ = "": IF c = -1 THEN d$ = "<DIR>"
   PRINT UCASE$(a$); TAB(10); UCASE$(b$); TAB(14); d$; TAB(23 - LEN(STR$(c)));
   IF c <> -1 THEN PRINT c;
   PRINT TAB(24); sran$(12); "-"; sran$(28); "-94";
   t1 = ran(12): t$ = sran$(59)
   PRINT TAB(36 - LEN(STR$(t1))); t1; : LOCATE CSRLIN, 36: PRINT ":"; t$
   IF c > -1 THEN total = total + c
   f = f + 1
LOOP
PRINT TAB(8 - LEN(STR$(f))); f; "files"; TAB(27 - LEN(STR$(total))); total; "bytes"
PRINT TAB(19); "17571840 bytes free"
PRINT
GOTO Start

'Directory Data. Format is:
'filename,extention,bytes or -1 for directory.

DATA dos,use,286,dos,itm,126,qbasic,ini,48,sports,bas,34,linkway2,,-1
DATA b_skills,,-1,fastype,,-1,pctools,,-1,orbitals,,-1,lanskool,,-1
DATA qv,,-1,adven,,-1,chess,,-1,drawer,,-1,pcglobe,,-1,othello,,-1
DATA mail,,-1,thedraw,,-1,watfile,,-1,chat,,-1,dos3,,-1,virus,,-1
DATA valance,,-1,astro,,-1,chemview,,-1,learndos,,-1,ftype,,-1
DATA hplc,,-1,tdist,,-1,periodic,,-1,compevo,,-1,tutor,,-1,temp,,-1
DATA pegasus,,-1,specials,,-1,tutorial,,-1,history,,-1,wpc60dos,,-1
DATA basic,,-1,glossary,,-1,wp60,,-1,qbasic,,-1,autoexec,bat,2048
DATA config,sys,1926,command,com,26348

DATA ,,-2

FUNCTION ran (top)
RANDOMIZE TIMER
ran = INT(RND(1) * (top - 1)) + 1
END FUNCTION

FUNCTION sran$ (top)
r$ = STR$(ran(top))
san$ = RIGHT$(r$, LEN(r$) - 1)
IF LEN(san$) < LEN(STR$(top)) - 1 THEN san$ = "0" + san$
sran$ = san$

END FUNCTION


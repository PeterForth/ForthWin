 (Broadcast source programs.
  OS-independent definitions.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  Conversion from 16-bit to 32-bit code - 1995-96gg
  Revision - September 1999
)

USER S0 \ address of the bottom of the data stack
USER R0 \ return address stack bottom
USER WARNING
USER STATE (- a-addr) \ 94
     \ a-addr - the address of the cell containing the flag "compilation state".
     \ STATE "true" in compile mode, otherwise "false."
     \ Modify STATE only the following standard words:
     \:; [] ABORT QUIT: NONAME
USER BLK

VECT OK
VECT <MAIN>
VECT? LITERAL
VECT? SLITERAL

: DEPTH (- + n) \ 94
\ + n - the number of single cells that are on the data stack before
\ how + n was placed there.
  SP @ S0 @ - NEGATE 1 CELLS /
\ value can be negative, therefore '> CELLS' instead of '/' is impossible  
;
:? STACK (->) \ give the error "stack exhaustion" if it is more than empty
  SP @ S0 @ SWAP U <IF S0 @ SP! -4 THROW THEN
;

:? COMP (->)
  STATE @ 0 = IF -312 THROW THEN (Only for compilation mode)
;

: WORD (char "<chars> ccc <char>" - c-addr) \ 94
\ Skip leading delimiters. Select characters restricted
\ char delimiter
\ Exception occurs if the length of the extracted string
\ greater than the maximum length of the line with the counter.
\ c-addr - the address of the variable of the region containing the extracted word
\ as a string with a counter.
\ If the parsed area is empty or contains only delimiters,
\ The resulting string has zero length.
\ At the end of the line is space, not included in the length of the line.
\ The program can change characters in the string.
  DUP SKIP PARSE 255 MIN
  DUP SYSTEM-PAD C! SYSTEM-PAD CHAR + SWAP CMOVE
  0 SYSTEM-PAD COUNT CHARS + C!
  SYSTEM-PAD
;

: '("<spaces> name" - xt) \ 94
\ Skip the leading spaces. Select the name, bounded by a space. Find name
\ and return the xt, the executable token for name. An ambiguous situation occurs
\ if name is not found.
\ During interpretation, name EXECUTE is equivalent to name.
  ALSO NON-OPT-WL CONTEXT!
  PARSE-NAME SFIND 0 = PREVIOUS
  IF -321 THROW THEN (-?)
;

: CHAR ("<spaces> name" - char) \ 94
\ Skip leading delimiters. Highlight a name terminated by spaces.
\ Put the code of its first character on the stack.
  PARSE-NAME DROP C @
;

: BYE (-) \ 94 TOOLS EXT
\ Return control to the operating system, if any.
  0 
  HALT
;

: EVAL-WORD (au -)
\ interpret (translate) a word named au
    SFIND? DUP IF
    STATE @ = IF 
    COMPILE, ELSE 
    EXECUTE THEN
                  ELSE
    -2003 THROW THEN
;

: NOTFOUND (au -)
\ reference to words in dictionaries in the form of vocname1 :: wordname
\ or vocname1 :: vocname2 :: wordname, etc.
\ or vocname1 :: wordname
\ Word wordname is translated in a modified context (!)

  2DUP 2> R [']? SLITERAL CATCH? DUP IF NIP NIP 2R>
  2DUP S "::" SEARCH 0 = IF 2DROP 2DROP THROW THEN \ In general, is there ::?
  2DROP ROT DROP
  GET-ORDER N> R
                         BEGIN (au)
    2DUP S "::" SEARCH WHILE (a1 u1 a3 u3)
    2 -2 D + (separator skip ::) 2> R
    R @ - 2 - SFIND IF
    SP @> R
    ALSO EXECUTE SP @ R> - 0 =
    IF CONTEXT! THEN
                                ELSE (a1 u ')
    RDROP RDROP
    NR> SET-ORDER
    -2011 THROW THEN
    2R> REPEAT
  NIP 0 = IF 2DROP PARSE-NAME THEN
  ['] EVAL-WORD CATCH
  NR> SET-ORDER THROW
 ELSE RDROP RDROP THEN
;

: INTERPRET_ (->) \ interpret the input stream
  BEGIN
    PARSE-NAME DUP
  WHILE
    SFIND? DUP
    IF
         STATE @ =
         IF COMPILE, ELSE EXECUTE THEN
    ELSE
         S "NOTFOUND" SFIND 
         IF EXECUTE
         ELSE 2DROP? SLITERAL THEN
    THEN
    ? STACK
  REPEAT 2DROP
;

VARIABLE & INTERPRET

'INTERPRET_' & INTERPRET TC-ADDR!

: INTERPRET & INTERPRET @ EXECUTE;


: # (SIGNED) (d1 - d2)
  [CHAR]) HOLD DUP> R DABS #S R> SIGN [CHAR] (HOLD
;

: .SN (n -)
\ Print n top stack items
   > R BEGIN
         R @
      WHILE
        SP @ R @ 1- CELLS + @ DUP 0 < 
        IF DUP U> D (D.) TYPE <# S> D # (SIGNED) #> TYPE SPACE
        ELSE. THEN
        R> 1-> R
      REPEAT RDROP
;

: OK1
  STATE @ 0 =
  IF
    DEPTH 6 U <IF
                 DEPTH IF. "Ok (" DEPTH .SN. ")" CR
                       ELSE. "Ok" CR
                       THEN
               ELSE. "Ok ([" DEPTH S> D (D.) TYPE. "] .."
                    5 .SN. ")" CR
               THEN
  THEN
;

: [\ 94 CORE
Interpretation: semantics is undefined.
\ Compile: Perform execution semantics given below.
\ Performance: ( -- )
\ Set interpretation status. [word of immediate execution.
  STATE 0!
; IMMEDIATE


:] (-) \ 94 CORE
\ Set compilation status.
  TRUE STATE!
;

: MAIN1 (-)
  BEGIN
    Refill
  WHILE
    INTERPRET OK
  REPEAT BYE
;

: QUIT (-) (R: i * x) \ CORE 94
\ Reset stack returns, write zero in SOURCE-ID.
\ Set standard input and interpretation status.
\ Do not display messages. Repeat the following:
\ - Accept line from input stream to input buffer, zero> IN
\ and interpret.
\ - Print the implementation-dependent prompt system if
\ the system is in a state of interpretation, all processes are completed,
\ and no ambiguous situations.
  BEGIN
    CONSOLE-HANDLES
    0 TO SOURCE-ID
    0 TO SOURCE-ID-XT
    [COMPILE] [
    ['] MAIN1 CATCH
    ['] ERROR CATCH DROP
 (R0 @ RP! \ The stack is not reset, because CATCH does it for us :)
    S0 @ SP! \ stack reset, tk OPTIONS can leave values: (
  AGAIN
;

: SAVE-SOURCE (- i * xi)
  SOURCE-ID-XT SOURCE-ID> IN @ SOURCE CURSTR @ 6
;

: RESTORE-SOURCE (i * xi -)
  6 <> IF ABORT THEN
  CURSTR! SOURCE! > IN! TO SOURCE-ID TO SOURCE-ID-XT
;

: EVALUATE-WITH ((i * x c-addr u xt - j * x)
\ Considering c-addr u as an input stream, compute it with the interpreter xt.
  SAVE-SOURCE N> R 
  > R SOURCE! -1 TO SOURCE-ID
  R> (['] INTERPRET) CATCH
  NR> RESTORE-SOURCE
  THROW
;

: EVALUATE (i * x c-addr u - j * x) \ 94
\ Saves current input specification.
\ Writes -1 to SOURCE-ID. Makes the string given by c-addr u,
\ input stream and input buffer, sets> IN to 0
\ and interprets. When the string is parsed to the end - restores
\ specification of the previous input stream.
\ Other changes to the stack are determined by EVALUATE words.
  ['] INTERPRET EVALUATE-WITH
;


VECT PROCESS-ERR (ior - ior) \ process a translation error (file).

: PROCESS-ERR1 (ior - ior) \ here a check for ior = 0 is also needed.
  DUP IF SEEN-ERR? IF DUP SAVE-ERR THEN THEN
;
'PROCESS-ERR1' PROCESS-ERR TC-VECT!

: RECEIVE-WITH-XT (i * x source source-xt xt - j * x ior)
\ save input specifications
\ set the input stream to source, the word to read the string in source-xt
\ execute xt
\ restore input specifications
  SAVE-SOURCE N> R
  C / L 2+ ALLOCATE THROW DUP> R 0 SOURCE! CURSTR 0!
  SWAP TO SOURCE-ID-XT
  SWAP TO SOURCE-ID
  CATCH DUP IF PROCESS-ERR (err - err) THEN
  R> FREE THROW
  NR> RESTORE-SOURCE
;

: RECEIVE-WITH (i * x source xt - j * x ior)
\ save input specifications
\ set input stream to source, execute xt
\ restore input specifications
  0 SWAP RECEIVE-WITH-XT
;

: HEAP-COPY (addr u - addr1)
\ copy line to heap and return its address in heap
  DUP 0 <IF 8 THROW THEN
  DUP CHAR + ALLOCATE THROW DUP> R
  SWAP DUP> R CHARS MOVE
  0 R> R @ + C! R>
;

VECT FIND-FULLNAME \ find the specified file and return it with the full path

: FIND-FULLNAME1 (a1 u1 - au)
  2DUP FILE-EXIST IF EXIT THEN
  2DUP + LibraryDirName 2DUP FILE-EXIST IF 2SWAP 2DROP EXIT THEN 2DROP
  2DUP + ModuleDirName 2DUP FILE-EXIST IF 2SWAP 2DROP EXIT THEN 2DROP
  2 (ERROR_FILE_NOT_FOUND) THROW
;
'FIND-FULLNAME1' FIND-FULLNAME TC-VECT!


: TranslateFlow (-)
  BEGIN REFILL WHILE INTERPRET REPEAT
;

: INCLUDE-FILE (i * x fileid - j * x) \ 94 FILE
\ Remove fileid from stack. Save current input specifications,
\ including the current value of SOURCE-ID. Write fileid to SOURCE-ID.
\ Make the file specified by fileid an input stream. Write 0 to BLK.
\ Other changes to the stack are determined by the words in the included file.
\ Repeat to the end of the file: read the line from the file, fill in the input
\ Buffer the contents of this line, set> IN to zero and interpret.
Interpretation of the text begins with the position from which it should occur
\ Further reading the file.
\ When the end of the file is reached, close the file and restore the specifications
\ input stream to their saved values.
\ Uncertain situation occurs if fileid is incorrect, if it occurs
\ input-output exceptions as it reads fileid, or
\ Exception occurs when closing a file. When has
\ place uncertain situation, status (open or closed) of any
\ interpreted files are implementation dependent.
  BLK 0!
  DUP> R  
  ['] TranslateFlow RECEIVE-WITH
  R> CLOSE-FILE THROW
  THROW
;

: INCLUDE-PROBE (addr u - ... 0 | ior)
  R / O OPEN-FILE-SHARED? DUP
  IF NIP EXIT THEN
  INCLUDE-FILE 0
;

VECT (INCLUDED)

: (INCLUDED1) (i * xau - j * x)
  R / O OPEN-FILE-SHARED THROW
  INCLUDE-FILE
;
'(INCLUDED1)' (INCLUDED) TC-VECT!

USER INCLUDE-DEPTH

: INCLUDED_STD (i * x c-addr u - j * x)
  CURFILE @> R
  2DUP HEAP-COPY CURFILE!
  
  INCLUDE-DEPTH 1+!
  INCLUDE-DEPTH @ 64> IF -27 THROW THEN
  ['] (INCLUDED) CATCH
  INCLUDE-DEPTH @ 1- 0 MAX 
  INCLUDE-DEPTH!
  
  CURFILE @ FREE THROW
  R> CURFILE!
  THROW
;

: INCLUDED (i * x c-addr u - j * x) \ 94 FILE
\ Remove c-addr u from the stack. Save current input specifications,
\ including the current value of SOURCE-ID. Open the file specified by c-addr u,
\ write the received fileid to the SOURCE-ID and make it an input stream.
Write 0 to BLK.
\ Other changes to the stack are determined by the words in the included file.
\ Repeat to the end of the file: read the line from the file, fill in the input
\ Buffer the contents of this line, set> IN to zero and interpret.
Interpretation of the text begins with the position from which it should occur
\ Further reading the file.
\ When the end of the file is reached, close the file and restore the specifications
\ input stream to their saved values.
\ Uncertain situation occurs if fileid is incorrect, if it occurs
\ input-output exceptions as it reads fileid, or
\ Exception occurs when closing a file. When has
\ place uncertain situation, status (open or closed) of any
\ interpreted files are implementation dependent.
  FIND-FULLNAME INCLUDED_STD
;
: REQUIRED (waddr wu laddr lu -)
  2SWAP SFIND
  IF DROP 2DROP
  ELSE 2DROP INCLUDED THEN
;
: REQUIRE ("word" "libpath" -)
  PARSE-NAME PARSE-NAME 2DUP + 0 SWAP C!
  REQUIRED
;
: INCLUDED-EXISTING (au -?) 2DUP FILE-EXISTS IF INCLUDED TRUE ELSE FALSE THEN;

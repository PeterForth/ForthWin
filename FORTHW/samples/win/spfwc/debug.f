( interactive debugger for )
( SPF/DSF/DF projects      )
( Dmitry Yakimov 2001      )
( ftech@tula.net           )

VARIABLE DBG-CTime \ compile time flag
VARIABLE DBG-RTime \ run-time flag
VARIABLE Nesting
VARIABLE C-LINE
VARIABLE NestOut

: +DEBUG TRUE  DBG-RTime ! 1000 NestOut ! ;
: -DEBUG FALSE DBG-RTime !    0 NestOut ! ;
: [DBG   TRUE  DBG-CTime !  ; IMMEDIATE
: DBG]   FALSE DBG-CTime !  ; IMMEDIATE


: OUT
\ ���������� ������� �������� �����������, 
\ ������������ �� ������ ����
   Nesting @ NestOut !
;

: SLIP
\ �� ���������� ��������� ����������� ������� ������,
\ ��� step over � c++
   Nesting @ 1+ NestOut !
;

CREATE debugTIB 80 CHARS ALLOT
: eval_debug_statements ( -- )
  \ A simple outer interpreter for interactive input at
  \ debugtime.
   BEGIN
     CR ." D>" debugTIB DUP 80 ACCEPT DUP
   WHILE
     ['] EVALUATE CATCH
     IF ." Oops!?" CR 2DROP
     THEN
   REPEAT
   2DROP ;

(  ��� �������� ���:
   CALL DBG-XT
   cell - ����� CLINE
   cell - ��������_������������_CLINE/�����_�����
   CALL XT - ���� �����
    
   ������ ������������ R@ @...
)

: DBG-XT ( addr u -- )
   DBG-RTime @  
   Nesting @ NestOut @ < AND
   IF 
      R@ @ 1+ ( skip �������)
      R@ CELL+ @ DUP 0xFFFF AND \ �����
      SWAP 16 RSHIFT \ ��������
      ROT + SWAP \ addr u
      R> 2 CELLS + >R
      SPACE R@ WordByAddr TYPE SPACE
      ." Nxt[" Nesting @ 0 <# #S #> 
      TYPE ." ]: "
      TYPE  1000 NestOut !
      eval_debug_statements
   ELSE R> 2 CELLS + >R
   THEN
;

VARIABLE DBG-IN
VARIABLE DBG-ADDR
VARIABLE DBG-ADDR?

: DBG-TOKEN
   STATE @ DBG-CTime @ AND DBG-ADDR? @ AND
   IF 
      C-LINE @ DBG-ADDR @ !
      DBG-IN @ 16 LSHIFT 
      >IN @ DBG-IN @ - OR DBG-ADDR @ CELL+ !
      DBG-ADDR? 0!
   THEN
;

: CLINE,
\ ����������� SOURCE � HERE ��� ������ �� ���������,
\ dbg-xt ����� ������ ����� ��� ������
   STATE @ DBG-CTime @ AND 
   IF 
      HERE BRANCH, >MARK
      HERE C-LINE !
      SOURCE DUP C,
      HERE OVER ALLOT
      SWAP CMOVE
      >RESOLVE1
   THEN
;

: DBG-ALLOT
\ ��������������� ����� ��� ���������� ���,
\ ��������������
     STATE @ DBG-CTime @ AND 
     IF  POSTPONE DBG-XT
         HERE DBG-ADDR !
         TRUE DBG-ADDR? !
         2 CELLS ALLOT HERE TO :-SET
     THEN
;

: DBG-INTERPRET
   CLINE,
   BEGIN
     STATE @ IF >IN @ DBG-IN ! THEN
     NextWord DUP 
   WHILE
     DBG-ALLOT
     2DUP S" ;" COMPARE 0= IF DBG-TOKEN THEN
     SFIND ?DUP
     IF
       STATE @ =
       IF COMPILE, ELSE EXECUTE THEN
     ELSE
       S" NOTFOUND" SFIND
       IF EXECUTE
       ELSE 2DROP  ?SLITERAL
       THEN
     THEN
     DBG-TOKEN
     ?STACK
   REPEAT 2DROP
;

' DBG-INTERPRET &INTERPRET !

: +Nest  1 Nesting +! ;
: -Nest -1 Nesting +!
  Nesting @ 0= IF -DEBUG THEN ;

FALSE WARNING !
: :
  : DBG-CTime @ IF POSTPONE +Nest CLINE,
  THEN
;

: ;
   DBG-CTime @ IF POSTPONE -Nest THEN
   POSTPONE ; 
; IMMEDIATE
TRUE WARNING !

REQUIRE /SYSTEMTIME lib/include/facil.f

CR   S"  ****** LOADING COMPATIBILITY W32F LAYER -EXTENSIONS-******  " TYPE CR



\  extensions   CASE-INS  

\ $Id: caseins.f,v 1.6 2008/03/23 18:56:10 ygreks Exp $
( Case insensitivity for SP-FORTH )
( CASE-INS - case sensitivity switcher )
( just include this lib :)

REQUIRE REPLACE-WORD lib/ext/patch.f
REQUIRE ON           lib/ext/onoff.f
REQUIRE [IF]         lib/include/tools.f \ должно быть подключено до caseins-варианта
REQUIRE [else]       lib/ext/caseins-tools.f

VARIABLE CASE-INS \ switcher
CASE-INS ON

: USEARCH-WORDLIST ( c-addr u wid -- 0 | xt 1 | xt -1 )
  CASE-INS @ 0= IF
    [ ' SEARCH-WORDLIST BEHAVIOR COMPILE, ] EXIT
  THEN
  @
  BEGIN
    DUP
  WHILE
    >R 2DUP
    R@ COUNT CEQUAL-U 
    IF 2DROP R@ NAME> R> ?IMMEDIATE IF 1 ELSE -1 THEN EXIT THEN
    R> CDR
  REPEAT DROP 2DROP 0
;

' USEARCH-WORDLIST TO SEARCH-WORDLIST

: UDIGIT ( C N1 -- N2 -1|0 ) 
\ N2 - значение литеры C как
\ цифры в системе счисления по основанию N1
\ hex-цифры могут быть строчными
  SWAP
  DUP [CHAR] 0 [CHAR] 9 1+ WITHIN
  IF \ within 0..9
     [CHAR] 0 -
  ELSE
     DUP [CHAR] A 1- >
     IF
       DUP [CHAR] a 1- > 
       IF 
         CASE-INS @ IF [CHAR] a ELSE 2DROP 0 EXIT THEN
       ELSE [CHAR] A THEN
       - 10 +
     ELSE 2DROP 0 EXIT THEN
  THEN
  TUCK > DUP 0= IF NIP THEN
;

' UDIGIT ' DIGIT REPLACE-WORD



















REQUIRE CHAR-UPPERCASE ~ac/lib/string/uppercase.f
\ REQUIRE /TEST ~profit/lib/testing.f

\ TRUE - strings are equal ignoring case
: CEQUAL-U ( a1 u1 a2 u2 -- flag )
  ROT TUCK <> IF DROP 2DROP FALSE EXIT THEN
  0 ?DO ( a1i a2i ) 2DUP
  C@ CHAR-UPPERCASE SWAP C@ CHAR-UPPERCASE <> IF 2DROP UNLOOP FALSE EXIT THEN
  SWAP CHAR+ SWAP CHAR+
  LOOP 2DROP TRUE
;

\ /TEST
\ 
\ REQUIRE TESTCASES ~ygrek/lib/testcase.f
\ 
\ TESTCASES CEQUAL-U
\ 
\ (( S" 2DROP" S" RDROP" CEQUAL-U -> FALSE ))
\ (( S" rDROP" S" RDROP" CEQUAL-U -> TRUE ))
\ (( S" " S" " CEQUAL-U -> TRUE ))
\ (( S" 2DROP" S" 2DRO" CEQUAL-U -> FALSE ))
\ (( S" SeArCh-woRDLiSt" S" SEARCH-WORDLIST" CEQUAL-U -> TRUE ))
\ 
\ END-TESTCASES

: [else]   \ 94 TOOLS EXT
    1
    BEGIN
      PARSE-NAME DUP
      IF  
         2DUP S" [if]"   CEQUAL-U  IF 2DROP 1+                 ELSE 
         2DUP S" [else]" CEQUAL-U  IF 2DROP 1- DUP  IF 1+ THEN ELSE 
              S" [then]" CEQUAL-U  IF       1-                 THEN
                                    THEN  THEN   
      ELSE 2DROP REFILL  AND \   SOURCE TYPE
      THEN DUP 0=
    UNTIL  DROP 
;  IMMEDIATE

: [if] \ 94 TOOLS EXT
  0= IF POSTPONE [else] THEN
; IMMEDIATE





\ EXT1 ****************  EXTENSION 1 SECONDS  ********************************

0 VALUE start-time

: get-local-time ( -- )         \ get the local computer date and time
  SYSTEMTIME GetLocalTime DROP
;

: ms@ ( -- ms )
  get-local-time
  SYSTEMTIME wHour         W@     60 *
  SYSTEMTIME wMinute       W@ +   60 *
  SYSTEMTIME wSecond       W@ + 1000 *
  SYSTEMTIME wMilliseconds W@ +
;

: time-reset ( -- )  ms@ TO start-time ;

: .elapsed ( -- )
  ms@ start-time -
  1000 /MOD
  60 /MOD
  60 /MOD
  ." Elapsed time: "
  2 .0 [CHAR] : EMIT
  2 .0 [CHAR] : EMIT
  2 .0 [CHAR] : EMIT
  3 .0
;

: elapse ( -<commandline>- )
  time-reset EVALUATE CR .elapsed
;


: DELAY-SECONDS ( n --) time-reset
                       BEGIN
     		       time-reset  get-local-time	 SYSTEMTIME wSecond   W@ + 1000 *
			     	DUP
				UNTIL
				      DROP ;
\ 1 SECOND = 1000 PAUSE
\ TEST4
\ : k begin   4 GetTickCount  SWAP - 30000000 SWAP / 1000 *   until ;
\ .( switching per second)
\  until ;


\ EXT2 ****************  EXTENSION A LESS FAST  WORD  ********************************
USER >OUT
USER W-CNT

: NLIST ( A -> )
  @
  >OUT 0! CR W-CNT 0!
  BEGIN
    DUP KEY? 0= AND
  WHILE  30 PAUSE
    W-CNT 1+!
    DUP C@ >OUT @ + 74 >
    IF CR >OUT 0! THEN
    DUP ID.
    DUP C@ >OUT +!
    15 >OUT @ 15 MOD - DUP >OUT +! SPACES
    CDR
  REPEAT DROP KEY? IF KEY DROP THEN
  CR CR ." Words: " BASE @ DECIMAL W-CNT @ U. BASE ! CR
;

: WORDS ( -- ) CR ." ---------------------------------------------------- " CR \
  CONTEXT @ NLIST
   CR ." ---------------------------------------------------- " CR
;

\ words should have also an instring feature to filter out the words that do not
\ have the followng   words  c  (show all words with a c inside)

\ EXT3 ****************  EXTENSION  FLOAD  *****************************

: FLOAD ( --)   BL WORD COUNT INCLUDED ;

\ USE  FLOAD  TEST.F   ....   INSPITE OF  S" TEST.F"  INCLUDED

\ *********************************************************************************

\ EXT4 ****************  EXTENSION CLS  ********************************

: CLS ( --)  30 0 DO CR LOOP ;

\ extensions,  floating point , extensions string stack

\  ALIAS     ALIAS CLS PAGE
\  S"  devel\~moleg\lib\util\alias.f" INCLUDED


 REQUIRE ?DEFINED devel\~moleg\lib\util\ifdef.f

\ nicaaou neiai, annioee?oaiia n eiaii a?oaiai neiaa.
: ALIAS ( | BaseName AliasName --> ) ' NextWord SHEADER LAST-CFA @ ! ;

\ ALIAS - yoi i?inoie caaieiaie neiaa, naycaiiue n ?o?ei eiaii.
\ a i?eioeia neaao?uea i?eia?u aiaeiae?iu:
\  : ;; ( --> ) [COMPILE] ; ; IMMEDIATE
\  ALIAS ; ;; IMMEDIATE 
\EOF
?DEFINED test{ \EOF -- oanoiaay naeoey ---------------------------------------

test{ : proba 0x123DFE76 ;
      ALIAS proba test        \ nicaaaony eiy test,
      test proba <> THROW     \ annioee?oaiia n eiaii neiaa proba
S" passed" TYPE
}test









\EOF
\ PARAMETERS.F  LOADS ALL THIS
\S
REQUIRE get-string ~nn/lib/getstr.f
REQUIRE EVAL-SUBST ~nn/lib/subst1.f
REQUIRE PLACE lib/ext/string.f
REQUIRE .R lib/ext/core-ext.f
REQUIRE @AZ ~nn/lib/az.f
REQUIRE OPEN/CREATE-FILE ~nn/lib/file.f
REQUIRE CLASS: ~nn/class/class.f
REQUIRE Control ~nn/lib/win/control.f
REQUIRE ComboBox ~nn/lib/win/controls/combobox.f
REQUIRE OFF ~nn/lib/onoff.f
REQUIRE ?FREE2 ~nn/lib/free.f
REQUIRE AppendNode ~nn/lib/list.f
REQUIRE S>NUM ~nn/lib/s2num.f
REQUIRE N>S  ~nn/lib/num2s.f
REQUIRE VOC-FOREACH ~nn/lib/utils/vocs.f



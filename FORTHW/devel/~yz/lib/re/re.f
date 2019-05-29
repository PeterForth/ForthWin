\ $Id: re.f,v 1.21 2008/12/11 06:33:00 ygreks Exp $
\ ���������� ��������� (regular expressions)
\ ������ ���������� NFA ��������� �� http://swtch.com/~rsc/regexp/regexp1.html

\ ������������ ���������� �� ����� �� ��������� � �������� dll
\ 1 - �� ����� dll :)
\ 2 - ����� ������� ����������� �������
\ 3 - ��������� ���� �������� ������ RE" �������� �� ����� ���������� � ��������� �
\     �������� ������� ��������� ������ ��������� ������� �� ���� � ��������!

\ TODO �� ����� ���������� ������� ����� DFA, ������������ ��������� �������������
\ TODO ����� 2
\ TODO �������� capturing
\ TODO fix bugs

\ ������� :
\ () ��������� ������������
\ ? �������� "0 ��� 1"
\ * �������� "0 ��� ������"
\ + �������� "1 ��� ������"
\ | �������� "���"
\ . ����� ������
\ ������������ ����������� ��������, � ������ . \ ( ) * | + ? { [
\ \w \s \d \W \S \D \t \n \r \x3F
\ [ ������������ ��������� �������� ������ ���������� ������, ��������� � ��������

\ ����: POSIX - http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html
\ �������:
\ - ����������� ������� ������ ������ special
\   ������ ���� "... special when matched with a preceding left-parenthesis, ..."
\   �� �������
\   "The current  -p1003.2 spec says that ')' is an ordinary character in the absence of an 
\    unmatched '('; this was an unintentional result of a wording error, and change is likely. 
\    Avoid relying on it." - http://linux.die.net/man/7/regex
\ - ��� backreferences - \1 \2 ���
\ - ������������ ������� (unicode to do?), ������ �������� - ������ ��� ���������
\ - . (�����) ����� ����� ������, � ����� 0 ����
\ - capturing �� ������������� POSIX (TODO!!!), ���������� leftmost longest � ��������� �������

\ NB ���� ��� Perl'������ ������� ���������� ��������� - http://perldoc.perl.org/perlre.html
\    �� ��� �������� ��� �������������� ����������, ��� ��������� ������� ������ � ���������,
\    ������� � ���� ������������� �� ����� � �� �����������

\ -----------------------------------------------------------------------

\ �������������� ������� �������� �������
\ RE" ( regexp" -- re )

\ ��������� ���������� ��������� re-a re-u � ������ a u, ��� ������������ ������������
\ stre_fast_match? ( a u re-a re-u -- ? )

\ ����������� ���������������� ������� � ������, ��� ������������ ������������
\ re_fast_match? ( a u re -- ? )

\ ��������� ���������� ��������� re-a re-u � ������ a u, ��������� ������������
\ stre_match? ( a u re-a re-u -- ? )

\ ����������� ���������������� ������� � ������, ��������� ������������
\ re_match? ( a u re -- ? )

\ ��������� ������������ �� ���������� re_match? ��� stre_match?
\ get-group ( n -- a u )
\ ���� n = 0 - ��� ������
\ ��� n > 0 - ���� ��� �� ����������� ������� �����-�������
\ ���� n ������ ��� ����� ��������� ������������ - ����������
\ ��� ������������ ������� �� ������������� ������������ 0 0
\ ��� ������������ ������� ������������� � ������ ������ ������������ addr 0

\ � ������ ������������� �������� ������������ ���������� � ����� ������

\ ��������� ������ �������� (10000 �������), ms
\ stre_match - 6891
\ re_match - 2938
\ stre_fast_match - 4328
\ re_fast_match - 672

\ -----------------------------------------------------------------------

REQUIRE [IFNDEF] ~nn/lib/ifdef.f
REQUIRE STR@ ~ac/lib/str5.f
REQUIRE >= ~profit/lib/logic.f
REQUIRE ��������� ~profit/lib/chartable.f
REQUIRE { lib/ext/locals.f
REQUIRE LAMBDA{ ~pinka/lib/lambda.f
REQUIRE /TEST ~profit/lib/testing.f
REQUIRE BOUNDS ~ygrek/lib/string.f
REQUIRE ENUM: ~ygrek/lib/enum.f
REQUIRE A_AHEAD ~mak/LIB/A_IF.F
REQUIRE NUMBER ~ygrek/lib/parse.f
REQUIRE new-set ~pinka/lib/charset.f
REQUIRE PRO ~profit/lib/bac4th.f
REQUIRE list-ext ~ygrek/lib/list/ext.f
REQUIRE list-make ~ygrek/lib/list/make.f

\ -----------------------------------------------------------------------

MODULE: regexp

list::() CONSTANT ()

state in-branch-0
state in-branch-1
state in-branch-2
state in-branch-3

state start-fragment
state no-brackets-fragment
state brackets-fin
state fragment-final current-state VALUE 'fragment-final
state fragment-error
state fragment-charset-1
state fragment-charset-2
state fragment-charset-all

state quoted-symbol

USER-VECT RESERVE

: RESERVE-DYNAMIC LAMBDA{ ALLOCATE THROW } TO RESERVE ;
: RESERVE-STATIC LAMBDA{ HERE SWAP ALLOT } TO RESERVE ;

USER-VALUE re_limit \ �������� ����� ���������
USER-VALUE re_start \ ��������� ����� ���������
USER-VALUE re_def_groups \ ������������ � ���������� ���������� �������������
USER-VALUE re_brackets \ ������ ��� nfa ��������������� ����� ������ �� ����� ���������� ������
USER-VALUE re_nfa_count \ ������� ����� nfa � ������� RE (�� ����� ���������� ������)
USER-VALUE re_gen  \ ����� ��������� (��� �������������), ��� � ������� .gen � nfa
USER-VALUE re_str_pos \ ������� � ������ (��� �������������), ��� ����� ��������� ������
USER-VALUE re_cur_set
USER-VALUE re_cur_set_state
USER-VALUE re_visited \ ���������� �� ����� ������ ���� (dottify, FREE-NFA-TREE)

: RE_INIT
  list::() TO re_brackets
  list::() TO re_visited
  new-set TO re_cur_set
  RESERVE-DYNAMIC ;

RE_INIT
..: AT-THREAD-STARTING RE_INIT ;..

..: AT-THREAD-FINISHING \ �� �����������
  re_cur_set FREE THROW
  {{ list
  re_brackets ['] free free-with
  re_visited list::free
  }}
;..

\ -----------------------------------------------------------------------

:NONAME DUP CONSTANT 1+ ; ENUM 1+const:
:NONAME DUP CONSTANT 2* DUP 0= ABORT" Overflow!" ; ENUM flags:

256 1+const:
 STATE_SPLIT STATE_FINAL
 STATE_MATCH_ANY
 STATE_WORD_CHAR STATE_WORD_CHAR_NOT
 STATE_SPACE_CHAR STATE_SPACE_CHAR_NOT
 STATE_DIGIT_CHAR STATE_DIGIT_CHAR_NOT
 STATE_MATCH_SET STATE_MATCH_SET_NOT
 STATE_ANCHOR_BOL STATE_ANCHOR_EOL
; CONSTANT N_STATE

\ ���������
0
CELL -- .c    \ ��� ���������
CELL -- .out1 \ ������ �����
CELL -- .out2 \ ������
CELL -- .flags
CELL -- .gen  \ ����� ��������� (�� ����� ��������, ������������ ������� ���������)
CELL -- .set  \ ��� .c=STATE_MATCH_SET ����� ��������� �� set
CONSTANT /NFA

1 flags: NFA_GROUP_START NFA_GROUP_END ; DROP

\ �������� (������������ ������ �� ����� ���������� ������)
0
CELL -- .i  \ ��������� ��������� ����� ��������� (����� nfa)
CELL -- .o  \ ������ ������� ����� ��������� (�������� - ������ ����� ���� ������������ ����� ����. nfa)
CELL -- .b  \ ���� ������ - ������ ���������� �� brackets-pair
CONSTANT /FRAG

\ ���������� ���������
0
CELL -- .nfa \ ������ ���������
CELL -- .sub \ ������������
CELL -- .nfa-n \ ����� ���-�� ��������� ��������� � ���� RE
CONSTANT /RE

\ -----------------------------------------------------------------------

: !>> ( addr u -- addr+CELL ) OVER ! CELL + ;

0
CELL -- .a.size
CELL -- .a.n
0 -- .a.data
CONSTANT /arr

: reserve-array { n | bytes -- arr }
    n CELLS /arr + -> bytes bytes RESERVE >R R@ bytes ERASE n R@ .a.n ! bytes R@ .a.size ! R> ;

: a:create { n | bytes -- arr }
    n CELLS /arr + -> bytes bytes ALLOCATE THROW >R R@ bytes ERASE 0 R@ .a.n ! bytes R@ .a.size ! R> ;
: a:full ( arr -- a u ) DUP .a.size @ ;
: a:copy ( arr -- arr' ) a:full DUP ALLOCATE THROW >R R@ SWAP MOVE R> ;

: a:free FREE THROW ;

: a:n .a.n @ ;

: a:elem ( n arr -- ) .a.data SWAP CELLS + ;
: a:append ( val arr -- )
   { arr }
   arr a:n arr a:elem !
   arr .a.n 1+! ;

: a:iter-> ( arr --> elem \ <-- )
   PRO
   DUP .a.data { z }
   a:n 0 ?DO
    z CONT
    z CELL+ -> z
   LOOP ;

: a:scan-> ( arr --> elem@ \ elem|0 <-- TRUE|FALSE )
   PRO
   DUP .a.data { z }
   a:n 0 ?DO
    z @ CONT IF z UNLOOP EXIT THEN
    z CELL+ -> z
   LOOP 0 ;

: arr-find-c ( arr c -- ? )
  { c }
  DUP .a.data SWAP a:n 0 ?DO 
    DUP @ .c @ c = IF DROP UNLOOP TRUE EXIT THEN
    CELL+
  LOOP DROP FALSE ;

0 [IF]
: a:scanback-> ( arr --> elem@ \ elem|0 <-- TRUE|FALSE )
   PRO { arr | z }
   arr a:n 1- arr a:elem -> z
   arr a:n 0 ?DO
    z @ CONT IF z UNLOOP EXIT THEN
    z CELL- -> z
   LOOP 0 ;
[THEN]

: save-subs ( -- subs )
   re_brackets list::length 2 * reserve-array DUP
   .a.data
   re_brackets LAMBDA{ >R R@ list::car !>> R> list::cdar !>> } list::iter
   DROP
   re_brackets list::length 2 * OVER .a.n ! ;

: adjust-sub-state { val nfa subs -- }
   subs .a.data
   subs a:n 0 ?DO
    DUP @ nfa = IF val OVER ! THEN
    CELL+
   LOOP
   DROP ;

{{ list
: new-brackets-pair ( -- pair ) %[ 0 % 0 % ]% DUP re_brackets cons TO re_brackets ;

: set-brackets-start ( start-nfa brackets-pair -- ) OVER .flags NFA_GROUP_START SWAP ! setcar ;
: set-brackets-end ( end-nfa brackets-pair -- ) OVER .flags NFA_GROUP_END SWAP ! cdr setcar ;
}}

: print-array { arr }
  CR arr a:n " [{n}] : " STYPE
  arr a:iter-> @ . ;

: normalize-subs { a u subs | z -- }
   0 subs a:elem -> z
   a u z 2!
   z 2 CELLS + -> z
   subs a:n 2/ 1 ?DO
    z @ u < z CELL+ @ u 1 + < AND IF z @ a + z CELL+ @ z @ - z 2! ELSE 0 0 z 2! THEN
    z 2 CELLS + -> z
   LOOP ;

: get-sub { n subs -- a u }
   n 2 * subs a:n < NOT ABORT" Man, you've crossed the bounds!"
   n 2 * subs a:elem 2@ ;

\ -----------------------------------------------------------------------

: new-chclass
  256 ;

\ -----------------------------------------------------------------------

VARIABLE process-continue

: run-parser ( a - )
   1 TO ������-�������
   ���������-������
   process-continue ON
   BEGIN
    process-continue @
   WHILE
    ������ re_limit >= IF ���������-��������� EXIT THEN
    ����-����� ���������-����-���
   REPEAT ;

\ ��������-�������-��������

\ -----------------------------------------------------------------------

: FREE-FRAG ( frag -- )
   DUP .b @ list::empty? NOT IF CR ." Fragment's bracket list not empty!" ABORT THEN
   DUP .o @ list::free
   FREE THROW ;

: FREE-NFA ( nfa -- )
   DUP .set @ ?DUP IF FREE THROW THEN
   FREE THROW ;

: frag ( nfa out-list -- frag )
   /FRAG ALLOCATE THROW >R
   R@ .o !
   R@ .i !
   list::() R@ .b !
   R> ;

: nfa { c link1 link2 | nfa -- nfa }
   /NFA RESERVE -> nfa
   c nfa .c !
   link1 nfa .out1 !
   link2 nfa .out2 !
   0 nfa .flags !
   0 nfa .gen !
   0 nfa .set !
   re_nfa_count 1+ TO re_nfa_count
   nfa ;

\ ������� �������� � ���������� ����� c
: liter ( c -- frag ) 0 0 nfa %[ DUP .out1 % ]% frag ;

: set-liter ( set c -- frag ) liter TUCK .i @ .set ! ;

\ ��������� ��� ������ ��������� frag1 � ��������� nfa
: link { frag1 nfa -- }
   \ ��� ���������� ������������ � frag1 ����������� � nfa
   nfa frag1 .b @ LAMBDA{ ( end-nfa pair ) OVER SWAP set-brackets-end } list::iter DROP
   frag1 .b @ list::free
   list::() frag1 .b !
   nfa frag1 .o @ LAMBDA{ ( nfa frag1.o.addr ) OVER SWAP ! } list::iter DROP ;

\ ������������ ��� ������ ��������� frag � ������� ������
: outs% ( frag -- ) .o @ ['] % list::iter ;

\ �������� ���������
: finalstate ( -- nfa ) STATE_FINAL 0 0 nfa ;

\ ������ ��������
: emptyfragment ( -- nfa ) STATE_SPLIT liter ;

\ �������� �������� ���������
: finalize ( frag -- frag ) DUP finalstate link ;

: move-brackets { src dst -- }
   src .b @ dst .b @ list::concat dst .b !
   list::() src .b ! ;

\ ���������������� ���������� ���� ����������
: concat { e1 e2 -- e }
  e1 e2 .i @ link
  e1 .i @ %[ e2 outs% ]% frag
  e2 OVER move-brackets
  e2 FREE-FRAG
  e1 FREE-FRAG ;

\ 0 or 1
: op-? { e1 -- e }
  STATE_SPLIT 0 e1 .i @ nfa
  %[ DUP .out1 % e1 outs% ]% frag
  e1 OVER move-brackets
  e1 FREE-FRAG ;

\ 0 or more
: op-* { e1 -- e }
  STATE_SPLIT 0 e1 .i @ nfa
  e1 OVER link
  ( nfa )
  %[ DUP .out1 % ]% frag
  e1 FREE-FRAG ;

\ 1 or more
: op-+ { e1 -- e }
  STATE_SPLIT 0 e1 .i @ nfa
  e1 OVER link
  ( nfa )
  e1 .i @ %[ SWAP .out1 % ]% frag
  e1 FREE-FRAG ;

\ alternation
: op-| { e1 e2 }
  STATE_SPLIT e1 .i @ e2 .i @ nfa
  %[ e1 outs% e2 outs% ]% frag
  e1 OVER move-brackets
  e2 OVER move-brackets
  \ DUP .b @ ." ???" write-list
  e1 FREE-FRAG
  e2 FREE-FRAG ;

\ -----------------------------------------------------------------------

: RANGE>S ( addr1 addr2 -- addr1 u ) OVER - 0 MAX ;

: op: S" +?*" all-asc: ;
: |: [CHAR] | asc: ;
: left: [CHAR] ( asc: ;
: right: [CHAR] ) asc: ;
: backslash: [CHAR] \ asc: ;
: [: [CHAR] [ asc: ;
: ]: [CHAR] ] asc: ;
: BOL: [CHAR] ^ asc: ;
: EOL: [CHAR] $ asc: ;

: unquote-next-liter ( -- nfa ) current-state >R quoted-symbol ����-����� execute-one R> current-state! ;


256 state-table perform-operation

all: CR ." Bad operation!" ABORT ;
symbol: * op-* ;
symbol: ? op-? ;
symbol: + op-+ ;


: HEXNUMBER ( a u -- n ) BASE @ >R HEX NUMBER 0= THROW R> BASE ! ;

\ ������������
quoted-symbol

all: CR ." ERROR: Quoting \" symbol EMIT ."  not allowed!" fragment-error ;
S" .\()*|+?{[$^" all-asc: symbol liter ;
symbol: t 0x09 liter ; \ Tab
symbol: n 0x0A liter ; \ LF
symbol: r 0x0D liter ; \ CR
symbol: x \ \x3F - ������ � ����� 0x3F
   ������ 2 + re_limit > IF ABORT THEN
   ������ 2 HEXNUMBER liter
   ������ 2 + ���������-������ ;
symbol: w STATE_WORD_CHAR liter ;
symbol: W STATE_WORD_CHAR_NOT liter ;
symbol: s STATE_SPACE_CHAR liter ;
symbol: S STATE_SPACE_CHAR_NOT liter ;
symbol: d STATE_DIGIT_CHAR liter ;
symbol: D STATE_DIGIT_CHAR_NOT liter ;

\ -----------------------------------------------------------------------

: extract { n -- a -1 | 0 }
   ������ n + re_limit >= IF FALSE EXIT THEN
   ������ TRUE ;

: proceed ( n -- ) ������ + ���������-������ ;

\ VARIABLE indent
\ : doi CR indent @ SPACES  ;

\ ����� - ���� �� ����������� |
\ ������ ����� RE ������� ������������ ����� ���� ��� ����� ����� ������ ������
: get-branches ( -- frag )
    \ doi ." get-branch " DEPTH .
    \ indent 1+!
    current-state >R
    in-branch-0
    ������ run-parser
    current-state 'fragment-final <>
    R> current-state!
    process-continue ON
    IF ABORT THEN
    \ indent @ 1- indent !
    \ IF doi ." get-branch fail" ABORT THEN
    \ doi ." get-branch done"
;

\ �������� - ����������� ������ ��� ������ � ����������
\ ������� ���� ��������. ����� ���� ��������
: get-fragment ( -- frag )
   \ doi ." get-fragment " DEPTH .
   \ indent 1+!
   current-state >R
   start-fragment
   ������ run-parser
   current-state 'fragment-final <>
   R> current-state!
   process-continue ON
   IF ABORT THEN
   \ indent @ 1- indent !
   \ IF doi ." get-fragment fail" ABORT THEN
   \ doi ." get-fragment done"
;

\ get-fragment � get-branches ���������� �������� ����-�����
\ ������ �������� �� ������� ��������� - ������ �� ���������

\ ���������� ������ - ��� ����� (�������� ������������) - ������������������ ����������

\ �� ����� ���������� �����
in-branch-0

all: rollback1 get-fragment in-branch-1 ;
|: emptyfragment in-branch-2 ;
right: fragment-error ;
end-input: fragment-error ;


\ �� ����� ���� ��������
in-branch-1

all: rollback1 get-fragment concat ;
|: in-branch-2 ;
right: rollback1 fragment-final ;
end-input: fragment-final ;


\ �� ����� ���� �����
in-branch-2

all: rollback1 get-fragment in-branch-3 ;
|: emptyfragment op-| ;
right: emptyfragment op-| rollback1 fragment-final ;
end-input: emptyfragment op-| fragment-final ;


\ �� ����� ���� ����� � ���� ��������
in-branch-3

all: rollback1 get-fragment concat ;
|: op-| in-branch-2 ;
right: op-| rollback1 fragment-final ;
end-input: op-| fragment-final ;


: is-(?:) ( -- ? )
   2 extract 0= IF FALSE EXIT THEN 
   ( a ) 2 S" ?:" COMPARE IF FALSE EXIT THEN
   2 proceed
   TRUE ;

\ ������ RE ���������
start-fragment

all: symbol liter no-brackets-fragment ;
symbol: . STATE_MATCH_ANY liter no-brackets-fragment ;
BOL: STATE_ANCHOR_BOL liter no-brackets-fragment ;
EOL: STATE_ANCHOR_EOL liter no-brackets-fragment ;
op: fragment-error ;
left:
 { | pair frag }
 is-(?:) NOT IF \ i.e. we should remeber this bracket!
   new-brackets-pair -> pair
   get-branches -> frag
   frag .i @ pair set-brackets-start
   pair frag .b @ list::cons frag .b !
 ELSE 
   get-branches -> frag
 THEN
 brackets-fin frag ;
right: fragment-error ;
backslash: unquote-next-liter no-brackets-fragment ;
end-input: fragment-final ;
|: fragment-error ;
[: fragment-charset-1 ;
]: fragment-error ;

: erase-set ( set -- ) /set ERASE ;
: copy-set ( src dst -- ) /set MOVE ;

: update-set
   symbol re_cur_set set+ \ eat one symbol always
   2 extract NOT IF EXIT THEN
   { addr }
   addr C@ [CHAR] - <> IF EXIT THEN \ if it is not a range - do nothing
   addr 1 + C@ [CHAR] ] = IF 1 proceed EXIT THEN \ if '-' is a last symbol in a charset - leave it and quit
   addr 1 + C@ DUP re_cur_set set+ \ add the range border
   ( c1 ) symbol ( c1 c2 )
   2DUP < IF CR ." Bad order" fragment-error EXIT THEN
   ?DO I re_cur_set set+ LOOP \ add all characters in a range
   2 proceed ; \ eat all what we have processed

\ ������ ���������� ������, �������� ������ ������ ���������
fragment-charset-1

on-enter: re_cur_set erase-set STATE_MATCH_SET TO re_cur_set_state ;
all: update-set fragment-charset-all ; \ ] �������������� ��� � ���
CHAR ^ asc: STATE_MATCH_SET_NOT TO re_cur_set_state fragment-charset-2 ;
end-input: fragment-error ;

\ ������ ���������� ������, �������� ������ ������ ����������� ���������� ������ �������������� ���� ����
fragment-charset-2

all: update-set fragment-charset-all ; \ ] �������������� ��� � ���
end-input: fragment-error ;


\ ������ ���������� ������
fragment-charset-all

all: update-set ;
]: /set RESERVE re_cur_set OVER copy-set re_cur_set_state set-liter no-brackets-fragment ;
end-input: fragment-error ;


\ ��������� �� ���������, �.�. ���� ������ ��� ���� (� �������� ������ ����� ��������)
no-brackets-fragment

all: rollback1 fragment-final ;
op: symbol perform-operation fragment-final ;
end-input: fragment-final ;


\ ����� ���������� ��������� - ������ ���� ����������� ������
brackets-fin

all: fragment-error ;
right: no-brackets-fragment ;
end-input: fragment-error ;


\ �������� �������
fragment-final

on-enter: process-continue OFF ;
all: CR ." ALREADY IN FINAL STATE!" ;


\ ������
fragment-error

on-enter:
 ALSO regexp
 " REGEX SYNTAX ERROR : position {������ re_start -} in {re_start ������ RANGE>S}<!>{������ re_limit RANGE>S}"
 PREVIOUS
 CR STYPE CR
 process-continue OFF ;
all: CR ." ALREADY IN ERROR STATE!" ;

\ -----------------------------------------------------------------------

: clean-visited ( -- ) re_visited list::free () TO re_visited ;

: member? LAMBDA{ OVER = } list::exist? NIP ;

\ ����������� ������������ NFA
: (FREE-NFA-TREE) ( nfa -- )
   DUP re_visited member? IF DROP EXIT THEN
   DUP re_visited list::cons TO re_visited
   DUP .out1 @ ?DUP IF RECURSE THEN
   DUP .out2 @ ?DUP IF RECURSE THEN
   FREE-NFA ;

: FREE-NFA-TREE clean-visited (FREE-NFA-TREE) clean-visited ;

\ ��������� RE �������� ������� a u
\ � ������ ������ ���������� - ������������ ����������
: (parse-full) { a u | pair re -- re }
   a TO re_start a u + TO re_limit
   re_start ���������-������

   \ re_brackets empty? NOT IF CR ." WARNING: regexp(1): possible memory leak" THEN
   \ re_brackets FREE-LIST

   () TO re_brackets
   new-brackets-pair -> pair \ whole string
   0 TO re_nfa_count

   get-branches
   finalize ( frag )

   /RE RESERVE -> re

   re_brackets list::reverse TO re_brackets
   save-subs re .sub !
   \ re_brackets write-list
   re_brackets list::['] free list::free-with
   () TO re_brackets

   ( frag ) DUP .i @ re .nfa !
   re_nfa_count re .nfa-n !
   FREE-FRAG
   re ;

EXPORT

\ ���������� ��� ��������� ������ �������������� ���������� ���������
\ ������ ��� re ��������� �� RESERVE-DYNAMIC
: FREE-REGEX ( re -- )
   DUP .nfa @ FREE-NFA-TREE
   DUP .sub @ a:free
   FREE THROW ;

: BUILD-REGEX ( a u -- re ) RESERVE-DYNAMIC (parse-full) ;

: BUILD-REGEX-HERE
   STATE @ IF
    POSTPONE A_AHEAD
    RESERVE-STATIC (parse-full)
    POSTPONE A_THEN
    POSTPONE LITERAL
   ELSE
    RESERVE-STATIC (parse-full)
   THEN ; IMMEDIATE

\ -----------------------------------------------------------------------

256 state-table is_alpha_char

all: FALSE ;
CHAR a CHAR z range: TRUE ;
CHAR A CHAR Z range: TRUE ;

256 state-table is_digit_char

all: FALSE ;
CHAR 0 CHAR 9 range: TRUE ;

: is_alphanum_char ( c -- ? ) DUP is_alpha_char SWAP is_digit_char OR ;
: is_word_char ( c -- ? ) DUP is_alphanum_char SWAP [CHAR] _ = OR ;
: is_space_char ( c -- ? ) BL 1+ < ;

DEFINITIONS

N_STATE state-table char-nfa-match ( c nfa -- ? )

all: CR signal " Attempt to match inappropriate state {n}. Fatal error." STYPE ABORT ;
0 256 range: DROP signal = ;
STATE_FINAL          asc: 2DROP FALSE ;
STATE_MATCH_ANY      asc: 2DROP TRUE ;
STATE_WORD_CHAR      asc: DROP is_word_char ;
STATE_WORD_CHAR_NOT  asc: DROP is_word_char NOT ;
STATE_SPACE_CHAR     asc: DROP is_space_char ;
STATE_SPACE_CHAR_NOT asc: DROP is_space_char NOT ;
STATE_DIGIT_CHAR     asc: DROP is_digit_char ;
STATE_DIGIT_CHAR_NOT asc: DROP is_digit_char NOT ;
STATE_MATCH_SET      asc: .set @ belong ;
STATE_MATCH_SET_NOT  asc: .set @ belong NOT ;

\ -----------------------------------------------------------------------

\ �������� ��������� � ������ �������
\ nondeterm ������� ������ ����
: addstate { nfa s -- }
   nfa 0 = IF EXIT THEN
   nfa .gen @ ( DUP .) re_gen = IF EXIT THEN
   re_gen nfa .gen !
   nfa .c @ STATE_SPLIT = IF
    nfa .out1 @ s RECURSE
    nfa .out2 @ s RECURSE
    EXIT
   THEN

   nfa .c @ STATE_ANCHOR_BOL =
   IF
    re_str_pos 0 <> IF EXIT THEN
    nfa .out1 @ s RECURSE
    EXIT
   THEN

   nfa s a:append ;

\ s1 - ������ ��������� ����������� ����
\ c - �������������� ������ �� ������
\ s2 - OUT - ������ ��������� ��� ���������� ����
: step { c s1 s2 | a -- }
   0 s2 .a.n !
   re_gen 1+ TO re_gen
   \ CR ." STEP " re_gen .
   0 s1 a:elem -> a
   s1 a:n 0 ?DO
    c a @ DUP .c @ char-nfa-match IF a @ .out1 @ s2 addstate THEN
    a CELL+ -> a
   LOOP ;

\ �������� ��������� � ������ �������
\ nondeterm ������� ������ ����
\ z - ������ ������������
\ subs - ��������� ������������ ��� nfa
: subs_addstate { nfa s z subs -- }
   nfa 0 = IF EXIT THEN \ !!
   nfa .gen @ re_gen = IF EXIT THEN \ !!
   re_gen nfa .gen ! \ !!

   subs a:copy TO subs

   nfa .flags @ [ NFA_GROUP_START NFA_GROUP_END OR ] LITERAL AND IF
    re_str_pos nfa subs adjust-sub-state \ ?DUP IF CR ." SUB " nfa . re_str_pos subs print-sub SWAP ! subs print-sub THEN
   THEN

   nfa .c @ STATE_SPLIT = IF \ !!
    \ CR ." SPLIT"
    nfa .out2 @ s z subs RECURSE
    \ nfa .out1 @ s RECURSE \ !!
    \ CR ." CONTINUE"
    nfa .out1 @ s z subs RECURSE
    \ nfa .out2 @ s RECURSE \ !!
    subs a:free
    \ CR ." <---"
    EXIT \ !!
   THEN \ !!

   nfa .c @ STATE_ANCHOR_BOL = \ !!
   IF                          \ !!
    re_str_pos 0 <> IF EXIT THEN \ !!
    nfa .out1 @ s z subs RECURSE
    \ nfa .out1 @ s RECURSE \ !!
    subs a:free 
    EXIT \ !!
   THEN \ !!

   nfa s a:append \ !!
   subs z a:append ;

\ s1 - ������ ��������� ����������� ����
\ z1 - ������ ���������� ������������ ��� ������� ��������� ����������� ����
\ c - �������������� ������ �� ������
\ ������� ����� ������ ��������� � ������������
: subs_step { c s1 s2 z1 z2 | s z -- }

   z2 START{ a:iter-> @ a:free }EMERGE

   0 z2 .a.n !
   0 s2 .a.n ! \ !!
   re_gen 1+ TO re_gen \ !!

   0 s1 a:elem -> s \ !!
   0 z1 a:elem -> z
   s1 a:n 0 ?DO \ !!
    c s @ DUP .c @ char-nfa-match IF s @ .out1 @ s2 z2 z @ subs_addstate THEN
    \ c a @ DUP .c @ char-nfa-match IF a @ .out1 @ s2 addstate THEN \ !!
    s CELL + -> s \ !!
    z CELL + -> z
   LOOP ;

EXPORT

: set-default-groups
   re_def_groups ?DUP IF a:free THEN
   TO re_def_groups ;

: take-default-groups re_def_groups 0 TO re_def_groups ;

: get-group ( n -- a u )
   re_def_groups 0= THROW
   re_def_groups get-sub ;

: get-idx-group ( n -- u1 u2 )
   0 get-group DROP SWAP get-group { a1 a u }
   u 0= IF 0 0 EXIT THEN
   a a1 - DUP u + ;

: groups-total ( -- n )
   re_def_groups 0= IF 0 EXIT THEN
   re_def_groups a:n 2 / ;

\ ������������� RE � ������, ��� ������������ ������������
: re_fast_match? { a u re | s1 s2 -- ? }
   re .nfa @ .gen @ 1+ TO re_gen

   re .nfa-n @ a:create -> s1
   re .nfa-n @ a:create -> s2

   re .nfa @ s1 addstate
   a u BOUNDS ?DO
    I C@ s1 s2 step
    s1 s2 -> s1 -> s2
   LOOP
   \ LAMBDA{ car .c @ STATE_FINAL = } l1 scan-list NIP
   \ l1 FREE-LIST
   s1 START{ a:scan-> .c @ DUP STATE_FINAL = SWAP STATE_ANCHOR_EOL = OR }EMERGE 0 <>
   re_gen re .nfa @ .gen ! \ ��� ����������� �������������
   s1 a:free
   s2 a:free
   ;

\ ��������� �� re � ���������� (�� ������) ������ a u
\ ���� ���� ����� ��������� - ������� � �����
\ ����� 0
: re_sub { a u re | s1 s2 last -- u1 }
   re .nfa @ .gen @ 1+ TO re_gen

   re .nfa-n @ a:create -> s1
   re .nfa-n @ a:create -> s2

   re .nfa @ s1 addstate
   0 TO last
   a u BOUNDS ?DO
    I C@ s1 s2 step
    s1 s2 -> s1 -> s2
\    s1 a:n 1 = IF
    \ s1 START{ a:scan-> .c @ STATE_FINAL = }EMERGE 
    s1 STATE_FINAL arr-find-c 
    IF ( 0 ) I a - 1+ TO last THEN
    s1 a:n 0= IF LEAVE THEN
   LOOP
   re_gen re .nfa @ .gen ! \ ��� ����������� �������������
   s1 a:free
   s2 a:free
   last ;

\ ������������� RE � ������
: re_match?
   { a u re | s1 z1 s2 z2 subs1 ? -- ? }
   \ StartTrace
   0 TO re_str_pos
   re .nfa-n @ a:create -> z1
   re .nfa-n @ a:create -> z2

   re .nfa-n @ a:create -> s1 \ !!
   re .nfa-n @ a:create -> s2 \ !!

   re .nfa @ .gen @ 1+ TO re_gen \ !!
   re .nfa @ s1 z1 re .sub @ subs_addstate 
   \ re .nfa @ s1 addstate \ !!

   a u BOUNDS ?DO \ !!
    I a - 1 + TO re_str_pos
    \ CR re_str_pos . I C@ EMIT
    I C@ s1 s2 z1 z2 subs_step
    \ I C@ s1 s2 step \ !!
    s1 s2 -> s1 -> s2 \ !!
    z1 z2 -> z1 -> z2
    \ CR I a - . s1 print-array
   LOOP \ !!
   \ CR s1 START{ a:iter-> @ .c @ . }EMERGE
   \ CR z1 START{ a:iter-> @ print-array }EMERGE
   s1 START{ a:scan-> .c @ DUP STATE_FINAL = SWAP STATE_ANCHOR_EOL = OR }EMERGE \ !!
   DUP 0<> -> ? \ !!
   ?DUP IF
    s1 .a.data - ( offset ) z1 .a.data + @
   ELSE
    re .sub @
   THEN
   a:copy -> subs1
   a u subs1 normalize-subs
   s1 a:free \ !!
   s2 a:free \ !!
   z1 START{ a:iter-> @ a:free }EMERGE
   z2 START{ a:iter-> @ a:free }EMERGE
   z1 a:free
   z2 a:free
   subs1 set-default-groups
   re_gen re .nfa @ .gen ! \ !! \ ��� ����������� �������������
   ? ; \ !!

: STREGEX=> ( re-a re-u --> re \ <-- ) PRO BUILD-REGEX { re } re CONT re FREE-REGEX ;

\ ��������� ���������� ��������� re-a re-u � ������ a u, ��� ������������ ������������
: stre_fast_match? ( a u re-a re-u -- ? ) STREGEX=> re_fast_match? ;

\ ��������� ���������� ��������� re-a re-u � ������ a u
: stre_match? ( a u re-a re-u -- ? ) STREGEX=> re_match? ;

\ �������� ������ ������������ ��������.
\ ������� ������ ������ �������� ��������� \" - ����� �������� �� ����� _����������_ �� ���� �������.
\ ���������� ������ �������������� � �������.
\ �� ����� ���������� �������� ���������������� ������� �� ����.
: RE" \ Compile-time: ( regex" -- )
\ runtime: ( -- re )
   "" >R
   BEGIN
    [CHAR] " PARSE
    2DUP + 1- C@ [CHAR] \ =
   WHILE
    1- R@ STR+
    '' R@ STR+
   REPEAT
   R@ STR+
   R@ STR@ POSTPONE BUILD-REGEX-HERE
   R> STRFREE ; IMMEDIATE

\ �������� ������ �� ������� ����� ������, �������������� � RE
\ �� ����� ���������� ������� RE �� ����
: EOLRE: ( -- re ) -1 PARSE POSTPONE BUILD-REGEX-HERE ; IMMEDIATE

\ �������� �������� ��� �������� - \0 \1 ... \10
:NONAME 10 0 DO I DUP " : \{n} {n} get-group ;" DUP STR@ EVALUATE STRFREE LOOP ; EXECUTE

;MODULE

\ -----------------------------------------------------------------------

/TEST

\ ������ ��������� backtracking ���������� ���������� ���������
\ ������������� ������ aa..(N ���)..a
\ � �������� a?a?..(N ���)..a?aa..(N ���)..a

: s^n { n s -- ss..(N ���)..s } "" n 0 DO DUP s STR@ ROT STR+ LOOP ;

: r1 { n s | q -- s }
   n s s^n
   " {$s}?" -> q
   n q s^n TUCK S+
   q STRFREE ;

: test { n | q s r -- ? }
   " a" -> q
   n q s^n -> s
   n q r1 -> r
   q STRFREE
   CR
   CR
   s STR@ r STR@ stre_match? IF ." It matches" ELSE ." Failed" THEN
   CR
   CR
   " Equivalent perl code (try it!): {CRLF}print {''}It matches\n{''} if {''}{$s}{''} =~ /{$r}/;" STYPE
   CR
( "
string :
{$s}
regex :
{$g}" STYPE)

   s STRFREE
   r STRFREE ;

40 test

\EOF

\ $Id: caseins.f,v 1.6 2008/03/23 18:56:10 ygreks Exp $
( Case insensitivity for SP-FORTH )
( CASE-INS - case sensitivity switcher )
( just include this lib :)

REQUIRE REPLACE-WORD lib/ext/patch.f
REQUIRE ON           lib/ext/onoff.f
REQUIRE [IF]         lib/include/tools.f \ ������ ���� ���������� �� caseins-��������
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
\ N2 - �������� ������ C ���
\ ����� � ������� ��������� �� ��������� N1
\ hex-����� ����� ���� ���������
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

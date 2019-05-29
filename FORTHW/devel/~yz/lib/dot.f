\ $Id: dot.f,v 1.3 2007/06/19 13:40:02 ygreks Exp $
\
\ ���������� dot ��������
\
\ ��� �������������� ���������� dot ��������� � ��������
\ ����������� GraphViz http://www.graphviz.org/

REQUIRE state-table ~profit/lib/chartable.f
REQUIRE BOUNDS ~ygrek/lib/string.f

MODULE: DOT-MODULE

0 VALUE H

: H-DOTOUT H DUP 0= ABORT" DOT- words can be used only inside dot{ }dot" ;

EXPORT

: DOT-TYPE ( a u -- ) H-DOTOUT WRITE-FILE THROW ;
: DOT-CR ( -- )  S" " H-DOTOUT WRITE-LINE THROW ;
: DOT-EMIT ( c -- ) SP@ 1 DOT-TYPE DROP ;

DEFINITIONS

127 state-table quoted-symbol?

all: FALSE ;
0
CHAR / range: TRUE ;
CHAR :
CHAR @ range: TRUE ;
  CHAR [ asc: TRUE ;
  CHAR \ asc: TRUE ;
  CHAR ] asc: TRUE ;
  CHAR ^ asc: TRUE ;
  CHAR ` asc: TRUE ;
  CHAR { asc: TRUE ;
  CHAR | asc: TRUE ;
  CHAR } asc: TRUE ;
  CHAR ~ asc: TRUE ;

127 state-table escape-output

all: signal DOT-EMIT ;
0 BL 1- range: [CHAR] ? DOT-EMIT ;
CHAR \ asc: [CHAR] \ DOT-EMIT signal DOT-EMIT ;
CHAR " asc: [CHAR] \ DOT-EMIT signal DOT-EMIT ;

: need-quotes? ( a u -- ? )
  BOUNDS
  ?DO
   I C@ quoted-symbol? IF UNLOOP TRUE EXIT THEN
  LOOP
  FALSE ;

EXPORT

\ output with quoting and escaping if needed
: SAFE-DOT-TYPE ( a u -- )
   2DUP need-quotes?
   IF
    [CHAR] " DOT-EMIT
    BOUNDS ?DO I C@ escape-output LOOP
    [CHAR] " DOT-EMIT
   ELSE
    DOT-TYPE
   THEN ;

\ ������� a u ����� ����� �������� a2 u2 ������������� � a1 u1
: DOT-ATTRIBUTE ( node-a node-u val-a val-u attr-a attr-u -- )
   2>R
   DOT-CR
   2SWAP ( node-a node-u ) SAFE-DOT-TYPE
   S"  [" DOT-TYPE
   2R> ( attr-a attr-u ) DOT-TYPE
   S" =" DOT-TYPE
   ( val-a val-u ) SAFE-DOT-TYPE
   S" ];" DOT-TYPE ;

\ a u - ���� ������� ���� ����������� ������
: DOT-FILLCOLOR ( a u -- )
   S" node" 2SWAP S" fillcolor" DOT-ATTRIBUTE ;

\ ������� a u ����� ����� ������� a2 u2
: DOT-LABEL ( a u a2 u2 -- )
   S" label" DOT-ATTRIBUTE ;

\ ������� a u ����� ����� ����� a2 u2
: DOT-SHAPE ( a u a2 u2 -- )
   S" shape" DOT-ATTRIBUTE ;

\ ����� �� ������� a u � ������� a2 u2
: DOT-LINK ( a u a2 u2 -- )
   DOT-CR
   2SWAP
   SAFE-DOT-TYPE
   S"  -> " DOT-TYPE
   SAFE-DOT-TYPE
   S" ;" DOT-TYPE ;

\ ������ dot ���������. ��������� � ���� a u
: dot{ ( a u -- )
   R/W CREATE-FILE THROW TO H
   S" digraph {" DOT-TYPE ;

\ ��������� dot ���������
: }dot
    DOT-CR
    S" }" DOT-TYPE
    H-DOTOUT CLOSE-FILE THROW
    0 TO H ;

;MODULE

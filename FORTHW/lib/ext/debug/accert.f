\ $Id: accert.f,v 1.3 2007/11/03 09:26:29 ygreks Exp $
\ ~day 11.02.2001
\ � ACCERT ������ �������� ���, ��������
\ ACCERT( 2DUP LOG )
\ ��������� �������� �� ������������, �������� ����������
\ ����������
\ ������ ACCERT-LEVEL:
\ 0 - �� ������������� ACCERT'�
\ 1 - ������������� ��� ACCERT'�
\ 2 - ������������� ACCERT ������ ���� 1
\ 3 - ������������� ACCERT ������ ���� 2

VARIABLE ACCERT-LEVEL
0 ACCERT-LEVEL ! \ �� ��������� ��������

: _LINE_
\ ����������� ��������� ������� - u - ����� ������� ������
  CURSTR @ 0 <# #S #> [COMPILE] SLITERAL
; IMMEDIATE

: _FILE_
\ ����������� ��������� ������� - ��� �������� ����� ����������
  CURFILE @ ASCIIZ> [COMPILE] SLITERAL
; IMMEDIATE

: ACCERT-EV ( addr u n -- )
   ACCERT-LEVEL @ 1- U> IF EVALUATE ELSE 2DROP THEN
;

: _ACCERT( ( n -- )
\ ����������� ����� �� ) ���� n > ACCERT-LEVEL-1
\ ����� ���������� ���
  >R
  BEGIN
    [CHAR] ) PARSE 2DUP + C@ [CHAR] ) = 0=
  WHILE
    R@ ACCERT-EV
    REFILL 0= IF RDROP EXIT THEN
  REPEAT R> ACCERT-EV
; IMMEDIATE

: ACCERT1( 1 [COMPILE] _ACCERT( ; IMMEDIATE
: ACCERT2( 2 [COMPILE] _ACCERT( ; IMMEDIATE
: ACCERT3( 3 [COMPILE] _ACCERT( ; IMMEDIATE
: ACCERT( [COMPILE] ACCERT1( ; IMMEDIATE

\EOF
\ ������

2 ACCERT-LEVEL !
: test
  ACCERT3( _FILE_ TYPE [CHAR] : EMIT _LINE_ TYPE 
  SPACE S" hi, this is accertion!" TYPE )
;
 test

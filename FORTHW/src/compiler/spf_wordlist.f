\ $Id: spf_wordlist.f,v 1.13 2008/11/08 15:53:33 ruv Exp $

( �������� �������� ������ � �������� WORDLIST.
  ��-����������� �����������.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999, ���� 2000
)

_VOC-LIST @ \ ������� �������

VARIABLE _VOC-LIST \ ������ ��������

' _VOC-LIST TC-ADDR! \ ������ ��� ��������� �������
' _VOC-LIST TO VOC-LIST  \ ����������� � tc

VECT VOC-LIST \ ����� ��� �����������
' _VOC-LIST ' VOC-LIST TC-VECT!  \ ��������� ��������


USER LAST     \ ��������� �� ���� ����� ���������
              \ ���������������� ��������� ������
              \ � ������� �� LATEST, ������� ����
              \ ����� ���������� ����� � CURRENT

1 CONSTANT &IMMEDIATE \ ��������� ��� ��������� ������ IMMEDIATE
2 CONSTANT &VOC

WORDLIST VALUE FORTH-WORDLIST  ( -- wid ) \ 94 SEARCH
\ ���������� wid - ������������� ������ ����, ����������� ��� ����������� 
\ �����, �������������� �����������. ���� ������ ���� ���������� ������ 
\ ���������� � ����� ���������� ������� ������.

: >BODY ( xt -- a-addr ) \ 94
\ a-addr - ����� ���� ������, ��������������� xt.
\ �������������� �������� ���������, ���� xt �� �� �����,
\ ������������� ����� CREATE.
(  1+ @ ���� � ������ 2.5 )
  5 +
;

: +SWORD ( addr u wid -> ) \ ���������� ��������� ������ � ������,
         \ �������� ������� addr u, � ������, ��������� wid.
         \ ��������� ������ ���� ����� � ����� �
         \ ���������� ������ �� ALLOT.
  HERE LAST !
  HERE 2SWAP S", SWAP DUP @ , !
;

: +WORD ( A1, A2 -> ) \ ���������� ��������� ������ � ������,
         \ �������� ������� �� ��������� A1, � ������, ���������
         \ ���������� A2. ��������� ������ ���� ����� � ����� �
         \ ���������� ������ �� ALLOT. � �������� ����� ��
         \ ������ A2 ���������� ����� ���� ����� ������, �
         \ ������� ���������� ����� � ���� ������.
         \ ������: C" SP-FORTH" CONTEXT @ +WORD
  SWAP COUNT ROT +SWORD
;

100000 VALUE WL_SIZE  \ ������ ������, ���������� ��� ���������� �������

LOAD-BUILD-NUMBER 1+ 
DUP SPF-KERNEL-VERSION 1000 * + VALUE VERSION  \ ������ � ����� ����� SPF � ���� 4XXYYY
    SAVE-BUILD-NUMBER

CREATE BUILD-DATE
NOWADAYS ,"

: AT-WORDLIST-CREATING ( wid -- wid ) ... ;

: WORDLIST ( -- wid ) \ 94 SEARCH
\ ������� ����� ������ ������ ����, ��������� ��� ������������� wid.
\ ����� ������ ���� ����� ���� ��������� �� �������������� �������������� 
\ ������� ���� ��� ����� ����������� �������������� � ������������ ������.
\ ������� ������ ��������� �������� ��� ������� 8 ����� ������� ���� � 
\ ���������� � ��������� � �������.

  HERE VOC-LIST @ , VOC-LIST !
  HERE 0 , \ ����� ����� ��������� �� ��� ���������� ����� ������
       0 , \ ����� ����� ��������� �� ��� ������ ��� ����������
       0 , \ wid �������-������
       0 , \ ����� ������� = wid �������, ������������� �������� �������
       0 , \ reserved, ��� ����������

  AT-WORDLIST-CREATING ( wid -- wid )
;
\ ��� ��������� �������� �������������� ����������:
\       0 , \ ����� �������� ���������� ������� (����� ��������)
\       0 , \ ������ ����, ������� ������������� ��������� �������
\       0 , \ DP ���������� ������� (������� ������)


: TEMP-WORDLIST ( -- wid )
\ ������� ��������� ������� (� ����������� ������)

  WL_SIZE ALLOCATE THROW DUP >R WL_SIZE ERASE
  -1      R@ ! \ �� ������������ � VOC-LIST, ������ ������� ����������� �������
  R@      R@ 6 CELLS + !
  VERSION R@ 7 CELLS + !
  R@ 9 CELLS + DUP CELL- !
  R> CELL+
;
: FREE-WORDLIST ( wid -- )
  CELL- FREE THROW
;

: VOC-NAME! ( c-addr wid --   )  CELL+ ! ;
: VOC-NAME@ ( wid -- c-addr|0 )  CELL+ @ ;  \ c-addr is an address of a counted string
: CLASS! ( cls wid -- ) CELL+ CELL+ CELL+ ! ;
: CLASS@ ( wid -- cls ) CELL+ CELL+ CELL+ @ ;
: PAR!   ( Pwid wid -- ) CELL+ CELL+ ! ;
: PAR@   ( wid -- Pwid ) CELL+ CELL+ @ ;
: WID-EXTRA ( wid -- addr )  4 CELLS + ; \ ��������� ��� ���������� ������ 
\ ������ ���������� �������������� ��� �����, ����� �������� ������ ���� ��������.


\ -5 -- cfa
\ -1 -- flags
\  0 -- NFA
\  1 -- name
\  n -- LFA

CODE NAME> ( NFA -> CFA )
     MOV EAX, -5 [EAX]
     RET
END-CODE

CODE NAME>C ( NFA -> 'CFA )
     LEA EAX, -5 [EAX]
     RET
END-CODE

CODE NAME>F ( NFA -> FFA )
     LEA EAX, -1 [EAX]
     RET
END-CODE

CODE NAME>L ( NFA -> LFA )
     MOVZX EBX, BYTE [EAX]
     LEA EAX, [EBX] [EAX]
     LEA EAX, 1 [EAX]
     RET
END-CODE

CODE CDR ( NFA1 -> NFA2 )
     OR EAX, EAX
     JZ SHORT @@1
      MOVZX EBX, BYTE [EAX]
      MOV EAX, 1 [EBX] [EAX]
@@1: RET
END-CODE

: ID. ( NFA[E] -> )
  COUNT TYPE
;

: IS-IMMEDIATE ( NFA -> F )
  NAME>F C@ &IMMEDIATE AND
;
: IS-VOC ( NFA -> F )
  NAME>F C@ &VOC AND
;

\ ��� �������� �������������:
: ?IMMEDIATE ( NFA -> F ) IS-IMMEDIATE ;
: ?VOC ( NFA -> F ) IS-VOC ;


: IMMEDIATE ( -- ) \ 94
\ ������� ��������� ����������� ������ ������������ ����������.
\ �������������� �������� ���������, ���� ��������� �����������
\ �� ����� �����.
  LAST @ NAME>F DUP C@ &IMMEDIATE OR SWAP C!
;

: VOC ( -- )
\ �������� ��������� ������������ ����� ��������� "�������".
  LAST @ NAME>F DUP C@ &VOC OR SWAP C!
;

\ ==============================================
\ �������������� - ������� �������� � ����

: IS-CLASS-FORTH ( wid -- flag )
  CLASS@ DUP 0= SWAP FORTH-WORDLIST = OR
;
: ENUM-VOCS ( xt -- )
\ xt ( wid -- )
  >R VOC-LIST @ BEGIN DUP WHILE
    DUP CELL+ ( a wid ) R@ ROT >R
    EXECUTE R> @
  REPEAT DROP RDROP
;
: (ENUM-VOCS-FORTH) ( xt wid -- xt )
  DUP IS-CLASS-FORTH IF SWAP DUP >R EXECUTE R> EXIT THEN DROP
;
: ENUM-VOCS-FORTH ( xt -- )
\ ������� ������ ������� ����-�������� (� ������� CLASS ����� 0 ��� FORTH-WORDLIST )
\ xt ( wid -- )
  ['] (ENUM-VOCS-FORTH) ENUM-VOCS  DROP
;
: FOR-WORDLIST  ( wid xt -- ) \ xt ( nfa -- )
  SWAP @ BEGIN  DUP WHILE ( xt NFA ) 2DUP 2>R SWAP EXECUTE 2R> CDR REPEAT  2DROP
;

\ ==============================================
\ ������� - ����� ����� �� ������ � ��� ����

: (NEAREST1) ( addr 0|nfa1 nfa2 -- addr 0|nfa1|nfa2 )
  DUP 0= IF DROP EXIT THEN >R
  \ ���������� xt (������ ������ ����, cfa @)
  2DUP  DUP IF  NAME> THEN 1-
  SWAP  R@ NAME> 1- ( a1 addr a2 ) WITHIN IF DROP R> EXIT THEN RDROP
  \ 1- �.�. WITHIN ������� �����
;
: (NEAREST2) ( addr nfa1 wid -- addr nfa2 )
  ['] (NEAREST1) FOR-WORDLIST
;
: (NEAREST3) ( addr nfa1 -- addr nfa2 )
  ['] (NEAREST2) ENUM-VOCS-FORTH
;

VECT (NEAREST-NFA) ( addr nfa1 -- addr nfa2 )

' (NEAREST3) ' (NEAREST-NFA) TC-VECT!

: (WordByAddr) ( addr -- c-addr u )
  0 (NEAREST-NFA)
  DUP 0= IF 2DROP S" <?not in the image>" EXIT THEN
  TUCK - ABS 4096 U< IF COUNT EXIT THEN
  DROP S" <?not found>"
;
: WordByAddr ( addr -- c-addr u )
  ['] (WordByAddr) CATCH  ?DUP IF ."  EXC:" . DROP S" <?WordByAddr exception>" EXIT THEN
  255 UMIN
;
: (WordByAddrSilent) ( addr -- c-addr u )
  0 (NEAREST-NFA) DUP 0= IF NIP DUP ( 0 0 ) EXIT THEN
  TUCK - ABS 4096 U< IF COUNT EXIT THEN
  0 ( caddr 0 )
;
: WordByAddrSilent ( addr -- c-addr u )
  ['] (WordByAddrSilent) CATCH  ?DUP IF ."  EXC:" . 0 ( x 0 ) THEN
  255 UMIN
;

\ ��� �������� �������������:
: NEAR_NFA ( addr -- nfa|0 addr )
  0 (NEAREST-NFA) SWAP
;

( ����� ���� � �������� � ���������� �������� ������.
  ��-����������� �����������.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999
)

VECT FIND

VECT SEARCH-WORDLIST ( c-addr u wid -- 0 | xt 1 | xt -1 ) \ 94 SEARCH
\ ����� �����������, �������� ������� c-addr u � ������ ����, ���������������� 
\ wid. ���� ����������� �� �������, ������� ����.
\ ���� ����������� �������, ������� ���������� ����� xt � ������� (1), ���� 
\ ����������� ������������ ����������, ����� ����� ������� (-1).


1 [IF] \ optimized and refactored version

S" src/compiler/spf_find_cdr.f" INCLUDED

[ELSE]

\ �������������� by day (29.10.2000)
\ �������������� by mak July 26th, 2001 - 15:45

CODE CDR-BY-NAME ( c-addr u nfa1|0 -- c-addr u nfa1|nfa2|0 )
\ ����, ��� � CDR (��. � spf_wordlist.f), �� ����� ����� ������ �������� �������� � �������� ���.
\ ��� ���������� �� SEARCH-WORDLIST, by ~ygrek Nov.2006
      PUSH EDI

      MOV EDX, [EBP]                \ ����� (�������)
      MOV ESI, EAX                  \ ���� � ������
      MOV EDI, 4 [EBP]              \ ������� �����

      CMP EDX, # 4    \ ��� ������ 0,1,2,3 ����� �� ������� ������ --
      JNB SHORT @@4    \ ��������� ����� (�� ����� ������� �������)
      MOV EBX, # 0xFF
      JMP @@1

@@4:  \ ���������� ���� ��� ���������
      A;  0xBB C, -1 W, 0 W, \    MOV EBX, # FFFF
      CMP EDX, # 3
      JB  SHORT @@8
      A;  0xBB C,  -1 DUP W, W, \   MOV  EBX, # FFFFFFFF
@@8:   MOV EAX, [EDI] \ -- � ���� ����� ����� ��������� AV, ���� c-addr � ���� ���������� ������
      SHL EAX, # 8
      OR  EDX, EAX
      AND EDX, EBX
      MOV AL, # 0
      A;  0x25 C, 0xFF C, 0 C, 0 W, \       AND EAX, # FF
      JMP @@1
@@3:
      A;  0x25 C, 0xFF C, 0 C, 0 W, \       AND EAX, # FF
      MOV ESI, 1 [ESI] [EAX]
@@1:   OR ESI, ESI
      JZ SHORT @@2                   \ ����� ������
      MOV EAX, [ESI]
      AND EAX, EBX
      CMP EAX, EDX
      JNZ SHORT @@3              \ ����� �� ����� - ���� ������
\ ��������� ��� ������
      INC ESI
      CLD
      XOR ECX, ECX
      MOV CL, DL
      PUSH ESI
      PUSH EDI                  \ ��������� ����� ������ �������� �����
      REPZ CMPS BYTE
      POP EDI
      JZ SHORT @@5
      POP EAX                   \ �������� esi �� ���������� � ������ �������
      MOV ESI, [ESI] [ECX]
      JMP SHORT @@1
@@2:
      XOR EAX, EAX
      JMP SHORT @@7                  \ ����� � "�� �������"

@@5:  
      POP ESI
      DEC ESI               \   ������������� �� ������ ������ � NFA
      MOV EAX, ESI
@@7:
      POP EDI
      RET
END-CODE

[THEN]

\ ����-����������:
\ : CDR-BY-NAME ( a u nfa1|0 -- a u nfa2|0 )
\  BEGIN  ( a u NFA | a u 0 )
\    DUP
\  WHILE  ( a u NFA )
\    >R 2DUP R@ COUNT COMPARE R> SWAP
\  WHILE
\    CDR  ( a u NFA2 )
\  REPEAT THEN 
\ ;

: SEARCH-WORDLIST-NFA ( c-addr u wid -- 0 | nfa -1 )
  @ CDR-BY-NAME NIP NIP ?DUP 0<>
;

: SEARCH-WORDLIST1
   SEARCH-WORDLIST-NFA 0= IF 0 EXIT THEN
   DUP NAME>
   SWAP ?IMMEDIATE IF 1 EXIT THEN -1
;

' SEARCH-WORDLIST1 ' SEARCH-WORDLIST TC-VECT!


USER-CREATE S-O 16 CELLS TC-USER-ALLOT \ ������� ������
USER-VALUE CONTEXT    \ CONTEXT @ ���� wid1

: SFIND ( addr u -- addr u 0 | xt 1 | xt -1 ) \ 94 SEARCH
\ ��������� ��������� CORE FIND ���������:
\ ������ ����������� � ������, �������� ������� addr u.
\ ���� ����������� �� ������� ����� ��������� ���� ������� � ������� ������,
\ ���������� addr u � ����. ���� ����������� �������, ���������� xt.
\ ���� ����������� ������������ ����������, ������� ����� ������� (1);
\ ����� ����� ������� ����� ������� (-1). ��� ������ ������, ��������,
\ ������������ FIND �� ����� ����������, ����� ���������� �� ��������,
\ ������������ �� � ������ ����������.
  S-O 1- CONTEXT
  DO
    2DUP I @ SEARCH-WORDLIST
    DUP IF 2SWAP 2DROP UNLOOP EXIT THEN DROP
   I S-O = IF LEAVE THEN
   1 CELLS NEGATE
  +LOOP
  0
;

: FIND1 ( c-addr -- c-addr 0 | xt 1 | xt -1 ) \ 94 SEARCH
\ ��������� ��������� CORE FIND ���������:
\ ������ ����������� � ������, �������� ������� �� ��������� c-addr.
\ ���� ����������� �� ������� ����� ��������� ���� ������� � ������� ������,
\ ���������� c-addr � ����. ���� ����������� �������, ���������� xt.
\ ���� ����������� ������������ ����������, ������� ����� ������� (1);
\ ����� ����� ������� ����� ������� (-1). ��� ������ ������, ��������,
\ ������������ FIND �� ����� ����������, ����� ���������� �� ��������,
\ ������������ �� � ������ ����������.
  DUP >R COUNT SFIND
  DUP 0= IF NIP NIP R> SWAP ELSE RDROP THEN
;

: DEFINITIONS ( -- ) \ 94 SEARCH
\ ������� ������� ���������� ��� �� ������ ����, ��� � ������ ������ � ������� 
\ ������. ����� ����������� ����������� ����� ���������� � ������ ����������.
\ ����������� ��������� ������� ������ �� ������ �� ������ ����������.
  CONTEXT @ SET-CURRENT
;

: GET-ORDER ( -- widn ... wid1 n ) \ 94 SEARCH
\ ���������� ���������� ������� ���� � ������� ������ - n � �������������� 
\ widn ... wid1, ���������������� ��� ������ ����. wid1 - �������������� ������ 
\ ����, ������� ��������������� ������, � widn - ������ ����, ��������������� 
\ ���������. ������� ������ �� ����������.
  CONTEXT 1+ S-O DO I @ 1 CELLS +LOOP
  CONTEXT S-O - 1 CELLS / 1+
;

: FORTH ( -- ) \ 94 SEARCH EXT
\ ������������� ������� ������, ��������� �� widn, ...wid2, wid1 (��� wid1 
\ ��������������� ������) � widn,... wid2, widFORTH-WORDLIST.
  FORTH-WORDLIST CONTEXT !
;

: ONLY ( -- ) \ 94 SEARCH EXT
\ ���������� ������ ������ �� ��������� �� ���������� ����������� ������ ������.
\ ����������� ������ ������ ������ �������� ����� FORTH-WORDLIST � SET-ORDER.
  S-O TO CONTEXT
  FORTH
;

: SET-ORDER ( widn ... wid1 n -- ) \ 94 SEARCH
\ ���������� ������� ������ �� ������, ���������������� widn ... wid1.
\ ����� ������ ���� wid1 ����� ��������������� ������, � ������ ���� widn
\ - ���������. ���� n ���� - �������� ������� ������. ���� ����� �������,
\ ���������� ������� ������ �� ��������� �� ���������� ����������� ������
\ ������.
\ ����������� ������ ������ ������ �������� ����� FORTH-WORDLIST � SET-ORDER.
\ ������� ������ ��������� �������� n ��� ������� 8.
   DUP IF DUP -1 = IF DROP ONLY EXIT THEN
          DUP 1- CELLS S-O + TO CONTEXT
          0 DO CONTEXT I CELLS - ! LOOP
       ELSE DROP S-O TO CONTEXT  CONTEXT 0! THEN
;


: ALSO ( -- ) \ 94 SEARCH EXT
\ ������������� ������� ������, ��������� �� widn, ...wid2, wid1 (��� wid1 
\ ��������������� ������) � widn,... wid2, wid1, wid1. �������������� �������� 
\ ���������, ���� � ������� ������ ������� ����� �������.
  GET-ORDER 1+ OVER SWAP SET-ORDER
;
: PREVIOUS ( -- ) \ 94 SEARCH EXT
\ ������������� ������� ������, ��������� �� widn, ...wid2, wid1 (��� wid1 
\ ��������������� ������) � widn,... wid2. �������������� �������� ���������,
\ ���� ������� ������ ��� ���� ����� ����������� PREVIOUS.
  CONTEXT 1 CELLS - S-O MAX TO CONTEXT
;

: VOC-NAME. ( wid -- ) \ ���������� ��� ������ ����, ���� �� ��������
  DUP FORTH-WORDLIST = IF DROP ." FORTH" EXIT THEN
  DUP CELL+ @ DUP IF ID. DROP ELSE DROP ." <NONAME>:" U. THEN
;

: ORDER ( -- ) \ 94 SEARCH EXT
\ �������� ������ � ������� ������, �� ������� ���������������� ������ �� 
\ ����������. ����� �������� ������ ����, ���� ���������� ����� �����������.
\ ������ ����������� ������� �� ����������.
\ ORDER ����� ���� ���������� � �������������� ���� ���������� ��������������
\ �����. ������������� �� ����� ��������� ������������ �������, 
\ ���������������� #>.
  GET-ORDER ." Context: "
  0 ?DO ( DUP .) VOC-NAME. SPACE LOOP CR
  ." Current: " GET-CURRENT VOC-NAME. CR
;

: LATEST ( -> NFA )
  CURRENT @ @
;

\ Andrey Filatkin, af@forth.org.ru

REQUIRE REPLACE-WORD lib/ext/patch.f

: SLITERAL2 \ ������ ������������ SLITERAL -
\ � ������ ������������� ������ ����������� � 
\ ������������ ������. ��� �������������, �� ���������� ������ ������.
  STATE @ IF
    ['] _SLITERAL-CODE COMPILE,
    DUP C,
    HERE SWAP DUP ALLOT MOVE 0 C,
  ELSE
    TUCK HEAP-COPY SWAP
  THEN
; IMMEDIATE
' SLITERAL2 ' SLITERAL REPLACE-WORD

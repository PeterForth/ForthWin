\ Andrey Filatkin, af@forth.org.ru
\ Work in spf3, spf4
\ ������� �������� ������. FREE �� ����������� ������, � ������� � ������.
\ ALLOCATE ������� ���� ���������� ����� ������ � ������, � ������
\ ���� �� ������� - ������� �����. FREEALL - ����������� ��� ������ � ������.
\ ��� ���� - ���� ���������� ������� ���� allocate9x �� ~nemmick.

WINAPI: HeapSize kernel32.dll

USER 9xLIST

FALSE WARNING !

: ALLOCATE ( n -- a ior )
  2 CELLS MAX 3 + 0xFFFFFFFC AND >R
  9xLIST 
  BEGIN DUP @ ?DUP WHILE
    CELL+ @ R@ =
    IF ( a )
      DUP @ DUP @ ROT !
      0 RDROP EXIT 
    THEN
    @
  REPEAT
  DROP R> ALLOCATE
;

: FREE ( a -- ior)
  DUP CELL- 0 THREAD-HEAP @ HeapSize  ( a size -- )
  CELL - ( ." >" DUP .) OVER CELL+ !
  9xLIST @ OVER !
  9xLIST !
  0
;

TRUE WARNING !

: FREEALL
  9xLIST @
  BEGIN ?DUP WHILE
    DUP @ SWAP FREE DROP
  REPEAT
  9xLIST 0!
;

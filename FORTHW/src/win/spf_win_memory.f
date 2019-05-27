\ $Id: spf_win_memory.f,v 1.8 2006/12/04 21:16:00 ygreks Exp $

( ���������� �������.
  Windows-��������� �����.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  ������� - �������� 1999
)
\ 94 MEMORY

USER THREAD-HEAP   \ ����� ���� �������� ������

VARIABLE USER-OFFS \ �������� � ������� ������ ������, 
                   \ ��� ��������� ����� ����������

: ERR
  IF 0 ELSE GetLastError THEN
;
: USER-ALLOT ( n -- )
  USER-OFFS +!

\ ��������� � USER-CREATE ~day 
\  USER-OFFS @ +   \ � ������ ����������
\  CELL 1- +  [ CELL NEGATE ] LITERAL AND \ ����� �����������
\  USER-OFFS !
;
: USER-HERE ( -- n )
  USER-OFFS @
;

HEX

VARIABLE EXTRA-MEM
4000 ' EXTRA-MEM EXECUTE !

: SET-HEAP ( heap-id -- )
  >R
  USER-OFFS @ EXTRA-MEM @ CELL+ + 8 R@ 
  HeapAlloc DUP
  IF
     CELL+ TlsIndex!
     R> THREAD-HEAP !
     R> R@ TlsIndex@ CELL- ! >R
  ELSE
     -300 THROW
  THEN
;

: CREATE-HEAP ( -- )
\ ������� ��� �������� ������.
  0 8000 1 HeapCreate SET-HEAP
;

: CREATE-PROCESS-HEAP ( -- )
\ ������� ��� ��������
 \ MSDN recommends using serialization for process heap
  \ Heap returned by GetProcessHeap caused problems with forth GUI and we want 
   \ to completely control our process heap
  0 8000 0 HeapCreate SET-HEAP
;

DECIMAL

: DESTROY-HEAP ( -- )
\ ���������� ��� �������� ������ ��� ��������
  THREAD-HEAP @ HeapDestroy DROP
;

: ALLOCATE ( u -- a-addr ior ) \ 94 MEMORY
\ ������������ u ���� ������������ ������������ ������. ��������� ������������ 
\ ������ �� ���������� ���� ���������. �������������� ���������� ����������� 
\ ������� ������ ������������.
\ ���� ������������� �������, a-addr - ����������� ����� ������ �������������� 
\ ������� � ior ����.
\ ���� �������� �� ������, a-addr �� ������������ ���������� ����� � ior - 
\ ��������� �� ���������� ��� �����-������.

\ SPF: ALLOCATE �������� ���� ������ ������ ����� �������� ������
\ ��� "��������� �����" (��������, �������� ������ ���������� �������)
\ �� ��������� ����������� ������� ���� ���������, ��������� ALLOCATE

  CELL+ 8 ( HEAP_ZERO_MEMORY) THREAD-HEAP @ HeapAlloc
  DUP IF R@ OVER ! CELL+ 0 ELSE -300 THEN
;

: FREE ( a-addr -- ior ) \ 94 MEMORY
\ ������� ����������� ������� ������������ ������, ������������ a-addr, ������� 
\ ��� ����������� �������������. a-addr ������ ������������ ������� 
\ ������������ ������, ������� ����� ���� �������� �� ALLOCATE ��� RESIZE.
\ ��������� ������������ ������ �� ���������� ������ ���������.
\ ���� �������� �������, ior ����. ���� �������� �� ������, ior - ��������� �� 
\ ���������� ��� �����-������.
  CELL- 0 THREAD-HEAP @ HeapFree ERR
;
: RESIZE ( a-addr1 u -- a-addr2 ior ) \ 94 MEMORY
\ �������� ������������� ������������ ������������ ������, ������������� � 
\ ������ a-addr1, ����� ��������������� �� ALLOCATE ��� RESIZE, �� u ����.
\ u ����� ���� ������ ��� ������, ��� ������� ������ �������.
\ ��������� ������������ ������ �� ���������� ������ ���������.
\ ���� �������� �������, a-addr2 - ����������� ����� ������ u ���� 
\ �������������� ������ � ior ����. a-addr2 �����, �� �� ������, ���� ��� �� 
\ �����, ��� � a-addr1. ���� ��� �����������, ��������, ������������ � ������� 
\ a-addr1, ���������� � a-addr2 � ���������� ������������ �� �������� ���� 
\ ���� ��������. ���� ��� ���������, ��������, ������������ � �������, 
\ ����������� �� ������������ �� u ��� ��������������� �������. ���� a-addr2 �� 
\ ��� ��, ��� � a-addr1, ������� ������ �� a-addr1 ������������ ������� 
\ �������� �������� FREE.
\ ���� �������� �� ������, a-addr2 ����� a-addr1, ������� ������ a-addr1 �� 
\ ����������, � ior - ��������� �� ���������� ��� �����-������.
  CELL+ SWAP CELL- 8 ( HEAP_ZERO_MEMORY) THREAD-HEAP @ HeapReAlloc
  DUP IF CELL+ 0 ELSE -300 THEN
;
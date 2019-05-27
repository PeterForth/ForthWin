\ $Id: spf_win_defwords.f,v 1.10 2006/12/04 21:16:00 ygreks Exp $

( ���������� � Windows - ����������� � ������� ������������� 
  ������� Windows � �������������� ������� [callback, wndproc � �.�.]
  Windows-��������� �����������.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999
)

VARIABLE WINAPLINK
0  VALUE NEW-WINAPI?

: __WIN:  ( params "������������" "�������������" -- )
  HERE >R
  0 , \ address of winproc
  0 , \ address of library name
  0 , \ address of function name
  , \ # of parameters
  IS-TEMP-WL 0=
  IF
    HERE WINAPLINK @ , WINAPLINK ! ( ����� )
  THEN
  HERE DUP R@ CELL+ CELL+ !
  PARSE-NAME CHARS HERE SWAP DUP ALLOT MOVE 0 C, \ ��� �������
  HERE DUP R> CELL+ !
  PARSE-NAME CHARS HERE SWAP DUP ALLOT MOVE 0 C, \ ��� ����������
  LoadLibraryA DUP 0= IF -2009 THROW THEN \ ABORT" Library not found"
  GetProcAddress 0= IF -2010 THROW THEN \ ABORT" Procedure not found"
;

: WINAPI: ( "������������" "�������������" -- )
  ( ������������ ��� ������� WIN32-��������.
    ���������� ����������� ����� ����� ��� "������������".
    ���� address of winproc ����� ��������� � ������ �������
    ���������� ���������� ��������� ������.
    ��� ������ ���������� "���������" ��������� ���������
    ���������� �� ���� ������ � �������, �������� ����������
    � ��-������ ���� ���������. ��������� ���������� �������
    ����� ������� �� ����.
  )
  NEW-WINAPI?
  IF HEADER
  ELSE
    -1
    >IN @  HEADER  >IN !
  THEN
  ['] _WINAPI-CODE COMPILE,
  __WIN:
;

: EXTERN ( xt1 n -- xt2 )
  HERE
  SWAP LIT,
  ['] FORTH-INSTANCE> COMPILE,
  SWAP COMPILE,
  ['] <FORTH-INSTANCE COMPILE,
  RET,
;

: CALLBACK: ( xt n "name" -- )
\ ����� n � ������!
  EXTERN
  HEADER
  ['] _WNDPROC-CODE COMPILE,
  ,
;

: WNDPROC: ( xt "name" -- )
  4 CELLS CALLBACK:
;

: TASK ( xt1 -- xt2 )
  CELL EXTERN
  HERE SWAP
  ['] _WNDPROC-CODE COMPILE,
  ,
;
: TASK: ( xt "name" -- )
  TASK CONSTANT
;

: ERASE-IMPORTS
  \ ��������� ������� ������������� ��������
  WINAPLINK
  BEGIN
    @ DUP
  WHILE
    DUP 4 CELLS - 0!
  REPEAT DROP
;

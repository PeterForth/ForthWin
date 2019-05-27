\ $Id: defwords.f,v 1.5 2008/12/23 19:46:55 spf Exp $
\ 
\ ���������� � Linux - ����������� � ������� �������������� ������� 
\ [callback, wndproc � �.�.]
(  
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999
)

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

: TASK ( xt1 -- xt2 )
  CELL EXTERN
  HERE SWAP
  ['] _WNDPROC-CODE COMPILE,
  ,
;
: TASK: ( xt "name" -- )
  TASK CONSTANT
;

VARIABLE WINAPLINK 	 
\ ���� WINAPLINK � �������� ���, �� ERASE-IMPORTS ������������ �����,
\ ������������ ��� dll-xt.f/so-xt.f-����� ����������� dll/so.
	  	 
: ERASE-IMPORTS 	 
\ ��������� ������� ������������� �������� 	 
  WINAPLINK 	 
  BEGIN 	 
    @ DUP 	 
  WHILE 	 
    DUP 4 CELLS - 0! 	 
  REPEAT DROP 	 
;

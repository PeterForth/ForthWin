\ $Id: spf_pe_save.f,v 1.15 2009/01/04 23:30:22 spf Exp $

( ���������� ������� � ������� Windows Portable Executable.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  ������� - �������� 1999
)

( HERE �� ������ ������ ����������)
DECIMAL
DUP        VALUE ORG-ADDR      \ ����� ���������� ����
DUP        VALUE IMAGE-BEGIN   \ ����� �������� ����
512 1024 * VALUE IMAGE-SIZE    \ ������� ����� ������������� ��� 
                               \ �������� ������ ����
DUP 8 1024 * - CONSTANT IMAGE-BASE \ ����� �������� ������ ������

VARIABLE RESOURCES-RVA
VARIABLE RESOURCES-SIZE

VARIABLE EXPORTS-RVA
VARIABLE EXPORTS-SIZE

HEX

: SAVE ( c-addr u -- ) \ �������� S" My Forth Program.exe" SAVE
  ( ���������� ������������ ����-������� � EXE-����� ������� PE - Win32 )
  R/W CREATE-FILE THROW >R
  ModuleName R/O OPEN-FILE-SHARED THROW >R
  HERE 400 R@ READ-FILE THROW 400 < THROW
  R> CLOSE-FILE THROW
  ?GUI IF 2 ELSE 3 THEN HERE 0DC + C!
  2000    HERE A8 +  ! ( EntryPointRVA )
  IMAGE-BEGIN 2000 -  HERE B4 +  ! ( ImageBase )
  IMAGE-SIZE 2000 +
          HERE D0 +  ! ( ImageSize )
  IMAGE-SIZE
          HERE 1A8 + ! ( VirtualSize )
  HERE IMAGE-BEGIN -  1FF + 200 / 200 *
          HERE 1B0 + ! ( PhisicalSize )

  2 HERE 086 + W!
  RESOURCES-RVA @ HERE 108 + !
  RESOURCES-SIZE @ HERE 10C + !
  EXPORTS-RVA @ HERE F8 + !
  EXPORTS-SIZE @ HERE FC + !
  HERE 1C8 + 38 ERASE

  HERE 400 R@ WRITE-FILE THROW ( ��������� � ������� ������� )
  HERE 200 ERASE
  IMAGE-BEGIN HERE OVER - 1FF + 200 / 200 * R@ WRITE-FILE THROW
  R> CLOSE-FILE THROW
;

DECIMAL

: SUBSTRING-OPTIONS ( c-addr1 u1 -- c-addr u )
\ �������� �� ������ �����, ��������� ��� ���������
  DUP 0= IF EXIT THEN
  OVER C@ [CHAR] " = IF SWAP CHAR+ SWAP 1- [CHAR] " ELSE BL THEN
  0 >R RP@ TUCK C! 1 SEARCH RDROP  0= IF 2DROP 0. EXIT THEN
  SWAP CHAR+ SWAP 1-
  -TRAILING
;
: COMMANDLINE-OPTIONS ( -- c-addr u )
\ ���� ����� ��������� ������ �������
  GetCommandLineA ASCIIZ> SUBSTRING-OPTIONS
;

\ Andrey Filatkin, af@forth.org.ru
\ Work in spf3, spf4
\ ������ � ini-�������

REQUIRE [DEFINED]  lib/include/tools.f

WINAPI: GetPrivateProfileStringA    kernel32.dll
WINAPI: WritePrivateProfileStringA  kernel32.dll

USER-CREATE BufIni 4096 USER-ALLOT
: N>S ( u -- addr0)
  S>D DUP >R DABS <#
  [ VERSION 400000 < [IF] ] 0 HOLD  [ [THEN] ] \ ����� ���� 0-������
  #S R> SIGN #> DROP
;

\ �������� ��������� �������� �����
: GetIniString ( addr0_ini addr0_sec addr0_key addr0_def -- addr0)
\ ��� addr0_ini - ����-��������������� ������ - ��� ini �����
\ addr0_sec - ��� ������
\ addr0_key - ��� �����
\ addr0_def - �������� �� ���������
\ addr0 - ��������� ������
  ASCIIZ> 1+ PAD SWAP MOVE
  >R >R
  4096 BufIni PAD
  2R>
  GetPrivateProfileStringA DROP
  BufIni
;

\ �������� ��������� �������� �����
: SetIniString ( addr0_ini addr0_sec addr0_key addr0 -- )
\ ���� - addr0 - ������������ ������
  SWAP ROT
  WritePrivateProfileStringA DROP
;

\ �������� �������� �������� �����
: GetIniInt ( addr0_ini addr0_sec addr0_key u1 -- u2)
\ u1 - ��������� �����
  DUP >R
  N>S GetIniString
  ASCIIZ> ['] ?SLITERAL1 CATCH 0= IF
    RDROP
  ELSE
    2DROP R>
  THEN
;

\ �������� �������� �������� �����
: SetIniInt ( addr0_ini addr0_sec addr0_key u1 -- )
  N>S SetIniString
;

\ �������� ������ ������ � ������
: EnumSectionKeys ( addr0_ini addr0_sec addr u -- flag)
\ ��� addr0_ini - ����-��������������� ������ - ��� ini �����
\ addr0_sec - ��� ������
\ addr - ����� ��� ������
\ u - ��� ������
\ ������ ������ ������:
\ ������ ���� - 0-������, � ����� ������ ��� ����
  ROT >R
  SWAP
  0 PAD C!  PAD
  0 R>
  GetPrivateProfileStringA 0<>
;

\ ������� ����
: DeleteIniKey ( addr0_ini addr0_sec addr0_key -- )
  0 SetIniString
;

\ ������� ������
: DeleteIniSection ( addr0_ini addr0_sec -- )
  0 0 SetIniString
;

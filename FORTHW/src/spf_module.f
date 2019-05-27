\ $Id: spf_module.f,v 1.2 2008/03/28 14:38:37 ygreks Exp $
\ 

TARGET-POSIX [IF]
S" src/posix/module.f" INCLUDED
[ELSE]
S" src/win/spf_win_module.f" INCLUDED
[THEN]

USER CURFILE

: CUT-PATH ( a u -- a u1 )
\ �� ������ a u �������� ����� �� ������ �� ���������� 
\ ������� ����������� ��������� (������������)
\ "some/path/name" -> "some/path/"
\ "some/path/" -> "some/path/"
\ "name" -> ""
\ �������� ������ �������� ���������� (r/o).
  CHARS OVER +
  BEGIN 2DUP <> WHILE CHAR- DUP C@ is_path_delimiter UNTIL CHAR+ THEN
  OVER - >CHARS
;

: ModuleDirName ( -- addr u )
  ModuleName CUT-PATH
;

: +ModuleDirName ( addr u -- addr2 u2 )
\ �������� addr u � "������_����_����������/"
  2>R
  ModuleDirName 2DUP +
  2R> DUP >R ROT SWAP CHAR+ CHARS MOVE 
  R> +
;

: +LibraryDirName ( addr u -- addr2 u2 )
\ �������� addr u � "������_����_����������/devel/"
  2>R
  ModuleDirName 2DUP +
  S" devel/" ROT SWAP CHARS MOVE
  6 + 2DUP +
  2R> DUP >R ROT SWAP CHAR+ CHARS MOVE 
  R> +
;

: SOURCE-NAME ( -- a u )
  CURFILE @ DUP IF ASCIIZ> ELSE 0 THEN
;


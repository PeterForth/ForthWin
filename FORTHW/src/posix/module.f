\ $Id: module.f,v 1.5 2008/05/12 12:09:22 ygreks Exp $
\

0 VALUE ARGC \ ���������� ���������� ��������� ������
0 VALUE ARGV \ ������ ����������

: is_path_delimiter ( c -- flag )
  [CHAR] / =
;

: ModuleName ( -- addr u )
  (( S" /proc/self/exe" DROP SYSTEM-PAD 1024 )) readlink
  DUP -1 = IF DROP 0 THEN
  SYSTEM-PAD SWAP 
  \ NB: not asciiz!
  \ 2DUP + 0 SWAP C!
;

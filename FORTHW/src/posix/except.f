\ $Id: except.f,v 1.3 2008/03/23 08:56:48 ygreks Exp $
( ��������� ���������� ���������� [������� �� ����, ���������
  �� ������������ �������, � �.�.] - ����� �������� � THROW.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  C������� 1999
)

   H-STDIN  VALUE  H-STDIN    \ ����� ����� - ������������ �����
   H-STDOUT VALUE  H-STDOUT   \ ����� ����� - ������������ ������
   H-STDERR VALUE  H-STDERR   \ ����� ����� - ������������ ������ ������
          0 VALUE  H-STDLOG

: AT-THREAD-FINISHING ( -- ) ... ;
: AT-PROCESS-FINISHING ( -- ) ... FREE-THREAD-MEMORY ;

: HALT ( ERRNUM -> ) \ ����� � ����� ������
  AT-THREAD-FINISHING
  AT-PROCESS-FINISHING
  1 <( )) exit
;

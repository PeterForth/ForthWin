\ $Id: spf_win_const.f,v 1.3 2006/12/04 21:16:00 ygreks Exp $

( Windows-���������, ����������� ��� �/�.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  ������� - �������� 1999
)

HEX

  40 CONSTANT PAGE_EXECUTE_READWRITE
1000 CONSTANT MEM_COMMIT
2000 CONSTANT MEM_RESERVE
  -1 CONSTANT INVALID_HANDLE_VALUE
  20 CONSTANT FILE_ATTRIBUTE_ARCHIVE
   2 CONSTANT CREATE_ALWAYS
   3 CONSTANT OPEN_EXISTING
   0 CONSTANT FILE_BEGIN
   1 CONSTANT FILE_CURRENT

80000000 CONSTANT R/O ( -- fam ) \ 94 FILE
\ fam - ������������ ����������� �������� ��� ������ ������ �������
\ � ����� "������ ��� ������"

40000000 CONSTANT W/O ( -- fam ) \ 94 FILE
\ fam - ������������ ����������� �������� ��� ������ ������ �������
\ � ����� "������ ��� ������"

C0000000 CONSTANT R/W ( -- fam ) \ 94 FILE
\ fam - ������������ ����������� �������� ��� ������ ������ �������
\ � ����� "������/������"

DECIMAL
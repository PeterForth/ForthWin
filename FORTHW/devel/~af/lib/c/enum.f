\ �������� ����� ��������
\ enum{ , zero , one DROP 5 , five }

VOCABULARY EnumSupport
GET-CURRENT ALSO EnumSupport DEFINITIONS

\ ������� ��������� ���������
: , ( n -- n+1 )  DUP CONSTANT 1+ ;

\ ����������� �������� ��������
: } ( n -- )  DROP PREVIOUS ;

SET-CURRENT

\ �������� �������� ����� ��������, ������ 0 �� ����
: enum{
  0
  ALSO EnumSupport
;
PREVIOUS

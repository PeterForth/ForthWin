\ ����� ������� �������, ���������������� �� c-��������
\ ��������������� ������

: CAPI-CALL ( ... n extern-addr -- x )
\ ����� ������� �������, ���������������� �� c-��������
  SWAP >R
  API-CALL
  R> 0 DO NIP LOOP
;

: CAPI: ( "������������" "�������������" n -- )
  ( ������������ ��� ������� c-�������.
    ���������� ����������� ����� ����� ��� "������������".
    ���� address of winproc ����� ��������� � ������ �������
    ���������� ���������� ��������� ������.
    ��� ������ ���������� "���������" ��������� ���������
    ���������� �� ���� ������ � �������, �������� ����������
    � ��-������ ���� ���������. ��������� ���������� �������
    ����� ������� �� ����.
    2 CAPI: strstr msvcrt.dll

    Z" s" Z" asdf" strstr
  )
  >IN @  CREATE  >IN !
  __WIN:
  DOES>
  DUP >R @ DUP 0= IF
    DROP R@ AO_INI ?DUP IF DUP R@ ! ELSE RDROP EXIT THEN
  THEN
  R> 3 CELLS + @ SWAP
  CAPI-CALL
;

: CVAPI: ( "������������" "�������������" -- )
\ ��� ������� � ���������� ������ ����������
\ ��� ������ ����� ���������� ���� ������� �� �����
\ CVAPI: sprintf msvcrt.dll

\ 50 ALLOCATE THROW VALUE buf
\ 10 Z" %d" buf 3 sprintf
  >IN @  CREATE  >IN !
  0 __WIN:
  DOES>
  DUP >R @ DUP 0= IF
    DROP R@ AO_INI ?DUP IF DUP R@ ! ELSE RDROP EXIT THEN
  THEN
  RDROP
  CAPI-CALL
;

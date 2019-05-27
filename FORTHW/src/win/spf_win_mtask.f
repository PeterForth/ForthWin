\ $Id: spf_win_mtask.f,v 1.4 2008/05/13 08:15:19 ygreks Exp $

( ���������������.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999
)
: START ( x task -- th )
  \ ��������� ����� task (��������� � ������� TASK:) � ���������� x
  \ ���������� th - ����� ������, ��� 0 � ������ �������
  0 >R RP@
  0 2SWAP 0 0 CreateThread
  RDROP
;
: SUSPEND ( th -- )
  \ ������� �����
  SuspendThread DROP
;
: RESUME ( th -- )
  \ ��������� �����
  ResumeThread DROP
;
: STOP ( th -- )
  \ ���������� ����� (�������)
  -1 SWAP TerminateThread DROP
;
: PAUSE ( ms -- )
  \ ������������� ������� ����� �� ms ����������� (1000=1���)
  Sleep DROP
;
: TERMINATE ( -- )
  \ ���������� ������� ����� (�������)
  DESTROY-HEAP
  -1 ExitThread
;
: THREAD-ID ( -- n ) 
  \ ������������� ������
  36 FS@ 
;

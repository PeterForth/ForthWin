\ $Id: mtask.f,v 1.6 2008/12/21 14:58:46 ygreks Exp $
\ 
\ ������ � ��������
\ �. �������, 20.05.2007

CR .( FIXME: man pthread_detach)
: START ( x task -- tid )
  \ ��������� ����� task (��������� � ������� TASK:) � ���������� x
  \ ���������� tid - ����� ������, ��� 0 � ������ �������
  0 >R RP@ 0 2SWAP SWAP 4 <( )) pthread_create
  IF RDROP 0 ELSE R> THEN
;
: SUSPEND ( tid -- )
  \ ������� �����
  \ ���������� ������ ��� ������!
  1 <( 19 ( SIGSTOP) )) pthread_kill DROP
;
: RESUME ( tid -- )
  \ ��������� �����
  \ ���������� ������ ��� ������!
  1 <( 18 ( SIGCONT) )) pthread_kill DROP
;
: STOP ( tid -- )
  \ ���������� ����� (�������)
  1 <( )) pthread_cancel DROP
;

\ Do not expect -1 PAUSE to sleep forever -- this is implementation-specific
\ For now provide special handling for such usage
: PAUSE ( ms -- )
  \ ������������� ����� �� ms ����������� (1000=1���)
  \ ���������� ����� �������
  BEGIN
  DUP 
  U>D 1000 UM/MOD SWAP 1000000 * >R >R
  (( RP@ 0 )) nanosleep DROP RDROP RDROP
  DUP -1 <> UNTIL 
  DROP
;
: TERMINATE ( -- )
  \ ���������� ������� ����� (�������)
  (( -1 )) pthread_exit DROP
;
: THREAD-ID ( -- tid )
  \ ������������� ������
  (()) pthread_self 
;

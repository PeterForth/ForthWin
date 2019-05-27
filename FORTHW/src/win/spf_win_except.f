\ $Id: spf_win_except.f,v 1.18 2008/11/17 20:55:52 ruv Exp $

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
: AT-PROCESS-FINISHING ( -- ) ... DESTROY-HEAP ;

: HALT ( ERRNUM -> ) \ ����� � ����� ������
  AT-THREAD-FINISHING
  AT-PROCESS-FINISHING
  ExitProcess
;

USER EXC-HANDLER  \ ���������� ���������� (������������� � �����������)

( DispatcherContext ContextRecord EstablisherFrame ExceptionRecord ExceptionRecord --
  DispatcherContext ContextRecord EstablisherFrame ExceptionRecord )
VECT <EXC-DUMP> \ �������� �� ��������� ����������

: DROP-EXC-HANDLER
  R> 0 FS! RDROP RDROP
;

: (EXC) ( DispatcherContext ContextRecord EstablisherFrame ExceptionRecord -- flag )
  (ENTER) \ ����� ��� ����� ������
  0 FS@ \ ����� ���������� ������������ ������ ��������� ����������
  \ ���� � ������� ������� ��� ����� (�� "�����", -- ��������, ���� ������ ������?)
  BEGIN DUP WHILE DUP -1 <> WHILE DUP CELL- @ ['] DROP-EXC-HANDLER <> WHILE ( ." alien " ) @ REPEAT THEN THEN
  \ ������� ����������� ������� �� -1
  DUP 0= OVER -1 = OR IF 0 TlsIndex! S" ERROR: EXC-frame not found " TYPE -1 ExitThread THEN
  \ ����, ��� ���� ����� �� ������, ������ ��� -- �.�. ���������� � (EXC) �������� ������ ����� ���� �����.
  DUP 0 FS! \ ��������������� ��� �����, ����� ���������� ������ exceptions � �������
  CELL+ CELL+ @ TlsIndex! \ ����� ����������� ��������� �� USER-������ �������� ������

\  2DROP 2DROP
\  0 (LEAVE)               \ ��� ���� ����� �������� ��������� ����

  DUP @ 0xC000013A = IF \ CONTROL_C_EXIT - Ctrl+C on wine
    0xC000013A HALT
  THEN
  DUP <EXC-DUMP>

  HANDLER @ 0=
  IF \ ���������� � ������, ��� CATCH, ������ ����� � ��������� (~day)
     DESTROY-HEAP
     -1 ExitThread
  THEN

  FINIT \ ���� float ����������, ���������������

  @ THROW  \ ���������� ���������� � ������ �������� :)
  R> DROP   \ ���� ��� �� ���������, �� �������� ������� �� callback
;

: SET-EXC-HANDLER
  R> R>
  TlsIndex@ >R
  ['] (EXC) >R
  0 FS@ >R
  RP@ 0 FS!
  RP@ EXC-HANDLER !
  ['] DROP-EXC-HANDLER >R \ ������������� ����� ����� ��������.����������
  >R >R
;
' SET-EXC-HANDLER ' <SET-EXC-HANDLER> TC-VECT!

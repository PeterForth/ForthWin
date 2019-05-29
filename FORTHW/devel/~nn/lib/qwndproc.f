\ 14.May.2002 Tue 12:57 Ruv
( ����� CALLBACK-������� �������� � ��������� ������ ����-�������,
  ����� �������� ������� � ����� ������� ��������� WNDPROC,
  ������� ����� ���������.
  ���� �������, ������������ ����� QWNDPROC: , ����� �������� � ����
  ������,  �� ��� �� ���� �� �������� �� ������ ���������� � user-����������.
)
\ �������� Nicholas Nemtsev

\ REQUIRE [NONAME ~nn/lib/noname.f

WINAPI: TlsAlloc    KERNEL32.DLL
WINAPI: TlsFree     KERNEL32.DLL
WINAPI: TlsSetValue KERNEL32.DLL
WINAPI: TlsGetValue KERNEL32.DLL

0 VALUE TlsIndexStore

: SaveTlsIndex ( -- )
\ ��������� TlsIndex �������� ������ � ����������� ��������� ������ ������
    TlsIndex@ TlsIndexStore TlsSetValue DROP \ ������ ���������� ������.
;
: RestoreTlsIndex ( -- )
\ ������������ TlsIndex �������� ������ �� ����������� ��������� ������ ������
    TlsIndexStore TlsGetValue TlsIndex!
;



\ ..: AT-PROCESS-FINISHING ( -- ) 
\ TlsIndexStore TlsFree DROP
\ ;..

..: AT-PROCESS-STARTING ( -- )
   TlsAlloc DUP -1 = IF GetLastError THROW THEN
    TO TlsIndexStore
;..

..: AT-THREAD-STARTING ( -- ) 
 TlsIndexStore TlsGetValue 0= IF SaveTlsIndex THEN ;..

: QWNDPROC: ( xt | "name" -- )
  HERE
  \ at enter
\  [NONAME  RestoreTlsIndex NONAME] COMPILE,
  ['] RestoreTlsIndex COMPILE,
  SWAP COMPILE, 
  \ at exit
  RET,
\  RET,
  HEADER
  ['] _WNDPROC-CODE COMPILE,
  ,
;

REQUIRE \EOF ~nn/lib/eof.f
\EOF
REQUIRE { lib\ext\locals.f
USER uvar1 
:NONAME
    2 uvar1 +! uvar1 @ .
    TlsIndex@ .
;
QWNDPROC: MyCallBackProc

: test1 \ --
  AT-PROCESS-STARTING
  AT-PROCESS-STARTING
  AT-THREAD-STARTING
  TlsIndexStore .
  TlsIndex@ . CR
  10 uvar1 !
  BEGIN 
      MyCallBackProc API-CALL
  AGAIN
;

:NONAME { time id msg hwnd -- }
    time .
; QWNDPROC: TimerProc

REQUIRE MessageLoop ~nn/lib/win/messageloop.f

: test2
  AT-PROCESS-STARTING
  AT-THREAD-STARTING
  TlsIndexStore .
  TlsIndex@ . CR
  ['] TimerProc 500 0 0 SetTimer . CR
  MessageLoop
;

test2
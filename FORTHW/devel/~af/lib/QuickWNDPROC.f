\ 14.May.2002 Tue 12:57 Ruv
\ 16 May,2002 af  af@forth.org.ru
( ����� CALLBACK-������� �������� � ��������� ������ ����-�������,
  ����� �������� ������� � ����� ������� ��������� WNDPROC,
  ������� ����� ���������.
  ���� �������, ������������ ����� QUICK_WNDPROC , ����� �������� � ����
  ������,  �� ��� �� ���� �� �������� �� ������ ���������� � user-����������.
)

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

..: AT-PROCESS-FINISHING ( -- )
    TlsIndexStore TlsFree DROP
;..
..: AT-PROCESS-STARTING ( -- )
    TlsAlloc DUP 0xFFFFFFFF = IF GetLastError THROW THEN
    TO TlsIndexStore
;..
..: AT-THREAD-STARTING ( -- )  SaveTlsIndex ;..

TlsAlloc TO TlsIndexStore
SaveTlsIndex

: EXTERN2 ( xt1 -- xt2 )
  HERE
  ['] RestoreTlsIndex COMPILE,
  SWAP COMPILE,
  RET,
;

: QUICK_WNDPROC ( xt "name" -- )
  EXTERN2
  CREATE  ( -- enter_point )
  ['] _WNDPROC-CODE COMPILE,
  ,
;

( \ example
: MyCallBackWord \ -- 
  ." test passed!" CR
;
' MyCallBackWord QUICK_WNDPROC MyCallBackProc
: test \ --
  MyCallBackProc API-CALL
;
  )
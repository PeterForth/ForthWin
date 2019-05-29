( 
!!! ������ ��� ���������� ��������� ������ ��� ������� ����������� 
!!! ���� TASK:, � ��������� ������. ���� � ��� ���� ����������� 
!!! CALLBACK:, �� �������� ���������� ��� ����������� ����
!!! ���� � ����������. [��. ������� - 2006.02.18]

    ������ ����� ��������� �� ����������� ������ ������ � SPF4
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ��������� � SPF4 ���� ������ � ���� ��������� ������������� �
  ����� �������� ������������ ����� ��������� ������������ �������,
  �� ���������� ��������� ����� ����� ��������� ������������ ������ 
  ������. 
    ���������: 
    1. �������� ���� ��������� �� ����� ������ ���������� ������ 
  ����������� ������ ��. 
    2. ������������� ���������� ����� ������ � ������ ������������ 
  �����, ���������� �������� � ContextRecord �� ����� ���������� 
  "0xC0000005L ACCESS_VIOLATION".
    3. ��� ������ �� callback ��������� ������� ����� � �������
  ���������������� ���������.

     � SPF4 ���� ����� ����� ���:
  xxxxFFFF-StackReserved   --- ����. ������� ����� ����������������� ��.
                           } ���� ��������� [���-�� ����� RP@]
  xxxxFFFF-r-ST_RES        --- R0 @ = [*]
                           } ���� ������ [���-�� ����� SP@]
  xxxxFFFF-r               --- S0 @ =
                           } ���� �������� ��� callback
  xxxxFFFF                 --- ��� �����

    ����� ��������� stack-guard.f ���� ����� ����� ���:
  xxxxFFFF-StackReserved   --- ����. ������� ����� ����������������� ��.
                           } ���� ��������� [���-�� ����� RP@]
  xxxxFFFF-r-ST_RES        --- R0 @ =
                           } �������������� ������� ��-�� 
                             ������������ �� PAGE-SIZE [**]
  xxxxFFFF-r-ST_RES+unused --- ���������� "SP-PROTECTED" �������� ����� �������,
                               ������� �������� �� 4��.  
                           } ������� ���������� � ������� VirtualProtect/PAGE_READONLY
                             [READONLY � ����� ����������, ���� ��������
                             ���� �������� �� ����������� � ������������ 
                             �������� ������ -3.]
  xxxxFFFF-r-ST_RES+unused+PAGE_SIZE --- ����. ������� ����� ������.
                           } ���� ������ [���-�� ����� SP@]
  xxxxFFFF-r               --- S0 @ =
                           } ���� �������� ��� callback
  xxxxFFFF                 --- ��� �����

    [*] � ���������� ������ ������ '-' ������ �� '_', 
        ���� �� ���������� � ����������.
    [**] ������ unused ������� �� r, ������� ������� �� ��.
         [� ���� � WinXP unused=132�, � � Win98SE - 160�.] 

  ��������� ������ [mailto:ss@forth.org.ru]
  2006.02.15

    TODO � ������ ����
    ~~~~~~~~~~~~~~~~~~
    0. ������������ ����� ��������� � _WNDPROC-CODE. 
    1. ������-�� ����������� THROW � AT-THREAD-STARTING/AT-THREAD-FINISHING.
    2. DECOMMIT �������������� ������� ����� ������ � ���������
       PAGE_GUARD/PAGE_NOACCESS �� ������ �������������� ��������,
       ����� �� ���� ������� COMMIT � ������ �����. [��������� ��?]
    3. src/spf_win_api.f::_WNDPROC-CODE: � ����� ST-RES ������ 
       StackCommitSize ����� �� PE-���������. [IMAGE-BASE 0xE4 + @ R-RESERVE -]
    z. ������, � ������ �� ���������� ����� �����? 

    �������
    ~~~~~~~
  2006.02.16 
  ~~~~~~~~~~
    ���������� ��� Win98. ������-�� �� ���������� PAGE_NOACCESS.
  ����� ���������������� PAGE_READONLY.
    �������������: ��������� ��� Win98 ���� PAGE_GUARD, �� ���� �� 
  ������������� PAGE_NOACCESS � ��� ��������� � "SP-PROTECTED"
  ������ ��� ����� ����������� ����. [1,2]
    �������������, ����� ��������� � ���������� �������� � ������ 
  ����� ���������� PAGE_READWRITE. ;( ���� ���������� ���������� 
  PAGE_GUARD+PAGE_READWRITE �� �����, �� WindowsXP ���� ���� ����������.
  [� PAGE_GUARD+PAGE_READONLY ����� "0xC0000005L ACCESS_VIOLATION"]
    ���� ��������� ��� ��������� �� C � ����������� ��� Win98 � WinXP:
  http://forth.org.ru/~ss/stack-guard-test.zip
    
  2006.02.17
  ~~~~~~~~~~
    - ������������� � AT-PROCESS-STARTING ������.
    * ����� ������� ��������� ������ "-3 ������������ �����" 
      [������� "<=", ������ "="]
    + �������� ��������� ����� ��� ������ �� callback.
      ��� ������ �� callback �� ����� ������ ��������� ����, � 
      ������ ������. ��������� �������� �������� ��� ������ �����.
    * ����� SAVE ������� H-STDOUT, � ����� �������� ��� � PROTECT-RETURN-STACK.
      �����, "spf_guarded.exe" ��� ��������������� � ���� ����� ��������.
      [����� CONSOLE-HANDLES ��������� � PROCESS-INIT ???]

  2006.02.18
  ~~~~~~~~~~
    ����������� AT-THREAD-STARTING ���������� ��� _����_ CALLBACK:, �
  �� ������ TASK: [�� ��� �� ��� ��������!] � ����� ������ ���� ������� 
  ��������� � _WNDPROC-CODE, ����� ������������ ������ ���������� 
  ������� ����� [���� � ��� �� ����� ;(]
    ����� ����, ��������� ������ ���������� ���-�� CALLBACK: ��� �������� 
  ������� ������-�����������, ������� ���������� �� ����������� ����� C 
  [�.�. CDECL ��� SYSV/X86 ABI.]  �� ������, ���� ����� �������� 
  CDECL-CALLBACK:, ������ ��������� ��� ��������� �� ����� � ��������� 
  C-���������� ���� ������� �� [���� �� ������� ���������, [� �� ����!]]
  ���������: �� ~af/lib/QuickWNDPROC.f ��� ���������� ������.
  ~~~~~~~~~  � � ���������� �� ~ac/lib/lin/curl/curl.f ����.
    IMHO, ������ ��� ���������� �������� �������� ������ � TASK:,
  QUICK_WNDPROC � �������� �������.


    ����������
    ~~~~~~~~~~
  [1] MSDN "Thread Stack Size" 
      http://msdn.microsoft.com/library/en-us/dllproc/base/thread_stack_size.asp
  [2] ��� ������ ��� PAGE_GUARD, EXCEPTION_GUARD_PAGE  � ����.
      ��������, "��� ��������� ����?" http://www.rsdn.ru/article/baseserv/stack.xml#EBPA
)

WINAPI: VirtualProtect  KERNEL32.DLL 
\ BOOL VirtualProtect(
\   LPVOID lpAddress,       // region of committed pages
\   SIZE_T dwSize,          // size of the region
\   DWORD flNewProtect,     // desired access protection
\   PDWORD lpflOldProtect   // old protection
\ );

0x1000 CONSTANT PAGE-SIZE
1 CONSTANT PAGE_NOACCESS
2 CONSTANT PAGE_READONLY
0x100 CONSTANT PAGE_GUARD

: VIRTUAL-PROTECT-PAGE ( addr new-prot -- old-prot ior )
  2>R 
  0 SP@ R> PAGE-SIZE R> VirtualProtect ERR
;

USER-VALUE SP-PROTECTED    \ ����� ���������� �������� ����� � ������� ������
USER-VALUE SP-OLD-PROTECT  \ �������� ���� ������

: xTHROW ?DUP IF >R 0 @ THEN ; \ ������� THROW �� �����������
                               \ ���������� ������ PAGE_READONLY - PAGE_GUARD
                               \ � ����� ������� THROW 
                               \ � �������, ������� THROW � AT-THREAD-FINISHING

: PROTECT-RETURN-STACK
  R0 @ PAGE-SIZE + PAGE-SIZE / PAGE-SIZE * DUP TO SP-PROTECTED
  H-STDOUT IF
    ." Protecting at "  DUP HEX . DECIMAL CR
    ." Unused stack space: " SP-PROTECTED R0 @ - . ." bytes" CR
    ." Maximal stack depth: " S0 @ SP-PROTECTED PAGE-SIZE + - CELL / . ." cells" CR
  THEN
  PAGE_READONLY VIRTUAL-PROTECT-PAGE xTHROW TO SP-OLD-PROTECT \ . . OK
  H-STDOUT IF
    ." Old protection: " SP-OLD-PROTECT . CR
  THEN
;

: EXC-DUMP-20060215 ( exc-info -- ) 
  \ ����� ���������� ��������� � ������ ������������ �����.
  IN-EXCEPTION @ IF DROP EXIT THEN
  TRUE IN-EXCEPTION !
  \ ������ ContextRecord->Ebp == SP@
  \ 3 PICK 180 + @ ." EBP=" HEX U.  CR
  3 PICK 180 + @ SP-PROTECTED PAGE-SIZE + > 0= IF -3 OVER ! ( ������������ �����) THEN
  3 PICK 180 + @ S0 @ > IF -4 OVER ! ( ���������� �����) THEN
  \ ������ ContextRecord->Esp == RP@
  3 PICK 196 + @ R0 @ > IF -6 OVER ! ( ���������� ����� ���������!) THEN
  \ 3 PICK 196 + @ S0 @ 0x100000 < IF -5 OVER ! ( ������������ ����� ���������) THEN
  FALSE IN-EXCEPTION !
  [ ' <EXC-DUMP> BEHAVIOR BRANCH, ]
;
' EXC-DUMP-20060215 TO <EXC-DUMP>
 
\ ��� ������ ..: AT-PROCESS-STARTING PROTECT-RETURN-STACK ;..
..: AT-THREAD-STARTING PROTECT-RETURN-STACK ;..
..: AT-THREAD-FINISHING
  \ ��� ������ �� callback �� ����� ������ ��������� ����, � ������ ������
  \ ��������� �������� �������� ��� ������ �����.
  \ ���������� spf4.exe :NONAME 30 0 DO DROP LOOP ; TASK: x  0 x START DROP
  \ [WinXP: �������� �����]
  \ SP@ S0 @ CELL- < IF ." -3 Stack overflow" CR -3 xTHROW THEN
  SP@ S0 @ CELL- > IF ." -4 Stack underflow" CR -4 xTHROW THEN
  \ �� ������ ������, ������ ������, ��� ��, �����, �� �� ����������.
  SP-PROTECTED SP-OLD-PROTECT VIRTUAL-PROTECT-PAGE xTHROW DROP 
;.. 
\ ST-RES �������� ����� �� ����� ����-����� � ������
\ ��� ���� CALLBACK:
0x7000 ST-RES ! \ ������ (PAGE-SIZE*3) ������.
                \ � ������ ��� (PAGE-SIZE*2) ������.
                \ ������ 0x8000 ���� ������� ������ (��. StackCommitSize � spf-stub.f, [1])
                \ E��� ����� ������ ����� ������ ��� 0x8000 ����, ��
                \ �������������� PE-��������� � "spf[_guarded].exe".
                \ (�������� � ������� http://hte.sourceforge.net, F6, "pe/header",
                \  "optional header: NT fields", "stack commit".)

\ � ���� ������������� �� �������� ����� ����� ������� SAVE
\ 0 TO H-STDOUT S" spf4_guarded.exe" SAVE BYE
\ � ����������:
\  spf4_guarded.exe : tt 10000 0 DO I LOOP ; tt
\                                            ^  -3 ������������ �����

\EOF

: sp-pre-overflow
  HEX 
  0x5F00 CELL /  0 DO I LOOP 
  SP@ .
  0x5F00 CELL /  0 DO DROP LOOP 
;
: sp-overflow
  HEX 
  0x7000 CELL /  0 DO I LOOP 
  SP@ .
  0x7000 CELL /  0 DO DROP LOOP 
;

( ~af\lib\QuickWNDPROC.f 
:NONAME
  ." In ~af's callback" CR
; QUICK_WNDPROC quick_callback
quick_callback API-CALL BYE)

:NONAME
  ." In Forth callback" CR
  \ sp-pre-overflow
  ." Out callback" CR
; CELL CALLBACK: callback

:NONAME
  10 PAUSE
  100000 MIN
  DUP RALLOT DROP RFREE 
  S0 @ R0 @ - . CR
  ." S0-R0=" S0 @ R0 @ - . CR
  HEX
  ." SP@=" SP@ . CR
  ." S0=" S0 @ . CR
  ." RP@="  RP@ . CR
  ." R0="  R0 @ . CR
  DECIMAL
  sp-pre-overflow
  \ 0 callback API-CALL DROP
  \ sp-overflow 
  \ SP-PROTECTED DUP HEX .  @ .
  1 PAUSE
  ." task done." OK
  0 
; 
TASK: test
.( S0-R0=) S0 @ R0 @ - . .( ST-RES=) ST-RES @ . CR
123 test START 150 PAUSE CloseHandle DROP CR OK \ QUIT
123 test START 1 PAUSE CloseHandle DROP CR OK 
123 test START 0 PAUSE CloseHandle DROP CR OK 
123 test START 1 PAUSE CloseHandle DROP CR OK 
123 test START 0 PAUSE CloseHandle DROP CR OK 
123 test START 1 PAUSE CloseHandle DROP CR OK 
123 test START 0 PAUSE CloseHandle DROP CR OK 
123 test START 1 PAUSE CloseHandle DROP CR OK 
123 test START 0 PAUSE CloseHandle DROP CR OK 
123 test START 1 PAUSE CloseHandle DROP CR OK 
123 test START 0 PAUSE CloseHandle DROP CR OK 
123 test START 1 PAUSE CloseHandle DROP CR OK 
123 test START 0 PAUSE CloseHandle DROP CR OK 
123 test START 1 PAUSE CloseHandle DROP CR OK 
123 test START 0 PAUSE CloseHandle DROP CR OK 
123 test START 1 PAUSE CloseHandle DROP CR OK 
123 test START 0 PAUSE CloseHandle DROP CR OK 
123 test START 1 PAUSE CloseHandle DROP CR OK 
: stress-test
  60 2 * 150 *  0 DO  
    I 10 + test START CloseHandle DROP
    I 150 UMOD 0= IF 
      500 PAUSE 
      ." ===========================================" CR
    THEN
  LOOP
;
stress-test
1000 PAUSE
OK
BYE
\EOF

: sp-overflow1
  ST-RES @ CELL / 10 + 0 DO I LOOP 
;
: test
  sp-overflow1
  ." good!"
;
test
DEPTH .

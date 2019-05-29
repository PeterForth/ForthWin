( ������� ���������� ��������� ���������������/��������������� )
 .( Round Robin priority multitasking v0.49 for sp-forth4 ) CR
 .(     HELP will be added soon ) CR
 .(	no portability to other systems :-( ) CR
\ SET-OPT 
\  To Do: 
\       + - ���������� �����
\       + - ���������� �������� ���������������/�������������/������_������
\       ? - ����� ����� ��������, ������ � �������� ������
\	- - ����� ��������� �������
\       ?? - ����������� �� �������������� ��� �����������
\       - - ����������� ���������� (��������, �� ������������)
\       - - ����������� ��������� ������������� �� ���������� ������ ����,
\       ����������� ����������

\ ����������� ����������
\       - ����������� ���������� �� spf-�������� ���

 (
_______�������� �� ������
\ [...] - ������, ���-�� ������ {� ���������� ���������� ����� ����
  �������� �� ���� �� �������� �����}

"""""��������/��������� ������"""""""

[������/��������� ������]
        0  ������� { ����� ��������, ����� ������ �������}
        -1  ���� { ������ ����� �������� ��������� �� ��� ������ ���� ���� 
        ����������}
        �[>0]  { ������� �������� ������ �������� �� ��������� ���������
        ������ ����� �� ���������� ������������; ���������������� ������
        ������}

[���������]
        0  ������ {���� �� ����, ����������� ��� ������ ������� ������}
        x[>0]  ��������� {���� �� ����, ����������� ������ �-�� ����}
        x[<-1]  ������������

"""""""����� �� ������""""""""""""

[��������� ������]
        ����� ��������� ������{}

[���������� ������] 
        ����� ���������� ������{}

"""""""���������� �����"""""""""""

[���� ������]
[���� ���������]
[����� ��������]
[�������������]
[user �������]

==========================================================================
        * ���������1: ���� �������������� ������ ����� ���� ���������
        ����� �������, ������ �������, �������{???}

        * ���������2: ���� ����������� � ����� ������ �����, �������� �
        ����� ����� ������ �� ������ � ����� �������, �������� � ����������
        ���� ����� ������ ��  ��������
        
        * ���������3: ������ ���������� 512 ����� ������ ��� ����� {�� 256}
        + 108 ���� ��� �������� ������������, �������� �� � �� ���������
        + ����������� ������� ������ {� ����} � ������� ��� �����������;
        ����� ������� �� ������� �������, ����� ��� ����� ���-�� ����

)

HERE
\ ���. �����, �� �������
\ : -- CREATE OVER , + DOES> @ + ;
\ ���������-��������� ������
0
CELL -- Status
CELL -- Priority
CELL -- NextTask
CELL -- PrevTask
CELL -- Task_sp0
CELL -- Task_sp
CELL -- Task_rp
CELL -- Task_rp0
CELL -- Task_fp
CELL -- TaskID
CELL -- UserArea
CONSTANT ring_str_size

\ ���� ���������� �����
CREATE Ring     64 ring_str_size *  ALLOT \ ���� ������������ �� 64 ������

\ ������ ������� ����� ��������� ����������, ������ �� ������� ���������
\ �� ��������� ����� � ������ (��������� �� ��������� ������)
\ ��� ������� ������, ��� �������� ������� ����� ����������, �������
\ ���������� �� ���������������, �� ���������� ������ ������ ����
\ ��������� �������

CREATE ID_stack 64 ALLOT \ ���� ���������� (������ � ������� ��������� �������))

: init_ID 64 0 ?DO I ID_stack I + C! LOOP ;
init_ID
VARIABLE ID_pointer     ID_stack ID_pointer !

: ID_get ( -- free_id ) ID_pointer @ C@ ID_pointer 1+! ;
: ID_free ( id -- ) -1 ID_pointer +! ID_pointer @ C! ;

: Ring[] ( i -- addr[i] ) ring_str_size * Ring + ; \ ���������� ����� �������� �-��� ������/����

  0 VALUE RT \ ������� ������ - ������������� ��������, ������ �� ���������
             \ ������������� � ������ �����

  VARIABLE TaskNum \ ����� � ������
  
  0 VALUE NEW_T
  0 VALUE _prev 0 VALUE _next
  
: other@ ; : other! ;


( ________________________________________________________________
�������1, ���������� ������
---------------------------------
<< - ��������� �� ����. ������
>> - ��������� �� ����. ������
>�>, <x< - ����������

<< 1 >> << 2 >> << 3 >> - ������ �����

<3< [1] >2> <1< [2] >3> <2< [3] >1> - ����, �� ���������
����� ������� - 2, ������� ������ 4 -- << 4 >>

[2]>> -> [4]>>  \ ����� � 2 ��������� �� ����. ������, ����������, 
                \ ���������� � 4 ��� ����. ������ 3
 {4} -> [2]>>   \ ���������� 4 � ��������� ����. � 2
  {2} -> [4]<<  \ ���������� 2 � ��������� ����. � 4
   {4} -> [3]<< \ ���������� 4 ��� ����. ��� 3
����� �/� �������� ������������

���������� ������ ����� ����� � ������ ��� ���, ��� ������ ������
� ���� ������ ����� �����:
<< 1 >> -- <1< [1] >1>, ������� ���-��; ��� ������� � ��� ������ 4 << 4 >>

[1]>> -> [4]>>
 {4} -> [1]>>
  {1} -> [4]<<
   {4} -> [1]<<
___________________________________________________________________)


0 Ring[] TO RT

: define_task ( memory status priority xt -- )
ID_get DUP Ring[] TO NEW_T \ � NEW_T - ��������� ����� ���������� ���������
NEW_T TaskID !          >R
NEW_T Priority !        NEW_T Status !
DUP 512 CELLS + 128 + ALLOCATE THROW NEW_T UserArea !
NEW_T UserArea @ + 256 CELLS + DUP 256 CELLS + DUP 8 +
\ ��������� ����� ����� ������
NEW_T Task_fp !
NEW_T Task_rp ! R> NEW_T Task_rp @ !
NEW_T Task_sp !
\ ��������� ��������� ��������� ������ - ���������� ����� �����
NEW_T Task_sp @ NEW_T Task_sp0 !
NEW_T Task_rp @ NEW_T Task_rp0 !
;

: add_to_ring ( -- )
TaskNum @ IF
  RT PrevTask @ DUP >R 
  NEW_T NextTask !
  NEW_T RT NextTask !
  RT NEW_T PrevTask !
  NEW_T R> PrevTask !
  NEW_T TO RT -1 TO NEW_T \ ������� ����� ������ �������
ELSE \ � ������-�� ������!!!
  NEW_T TO RT
  RT DUP NextTask !
  RT DUP PrevTask !
THEN
TaskNum 1+!
;


( ________________________________________________________________
�������2, �������� ������ �� ������
----------------------------------
�������� ������.

������: << 1 >> << 2 >> << 3 >>, ������ ������ 2, ��� �������, ���� ��� �������
���������:
<3< [1] >2>  <1< [2] >3>  <2< [3] >1>

[2]>> -> [1]>>
 [2]<< -> [3]<<
  ������������� ������ ������� �� ���� ��������� ����������
__________________________________________________________________ )



: terminate ( delete_task) ( id -- )
RT >R
Ring[] TO RT
RT NextTask @ TO _next ( 3)
RT PrevTask @ TO _prev ( 1)
RT NextTask @ _prev NextTask !
RT PrevTask @ _next PrevTask !
RT TaskID @ ID_free RT UserArea @ FREE THROW
R> TO RT
-1 TaskNum +!
;

: find_active_task
   RT Status @
    0 > IF -1 RT Status +! THEN \ -1 ������ ����, ������� ��������� �� 
                                \ ��� �� ������, ���� �� ������
   BEGIN RT NextTask @ TO RT RT Status @ \ DUP . ." -status "
   DUP 0 > IF -1 RT Status +! THEN 0=
   UNTIL
;

: check_priority
\ ������ ����������
RT Priority @ RT Status ! ;

: save_task
RP@ SP@ \ CELL+
RT Task_sp 2!
other! ;

: analiz
RT NextTask @ TO RT
\ ������ ��������� ������
RT Status @ \ ( 0|-1|U )
   0<> IF
   find_active_task
THEN
;
  
: switch ( PAUSE ) \ "�������������" �����
\ �������� ��������� ������
\ ." [" S0 @ . SP@ . ." ]" 
  save_task
\ ." [" S0 @ . SP@ . ." ]" 
\ ��������� �� ������ ��������� ������
  analiz
\ ." [" S0 @ . SP@ . ." ]"   
  check_priority
\ ." [" S0 @ . SP@ . ." ]"   
\ ������������ ������
RT Task_sp0 2@ ( sp s0 ) S0 ! SP!
RT Task_rp 2@ ( r0 rp ) RP! R0 !
;

VARIABLE _S VARIABLE _R
VARIABLE _S0 VARIABLE _R0

: activate ( -- ) \ ������ "��������������" �����
RP@ SP@ \ ����� ������� ��������� �������, �������� ��� ���-������
CELL+ _S ! _R ! S0 @ _S0 ! R0 @ _R0 !
\ ����� ������� ����� �� ������ - ������ ��� ����� ��������� ������������ �����
  other@
  RT Task_sp0 @ S0 !
  RT Task_sp @ SP!
  RT Task_rp0 @ R0 !
  RT Task_rp @ RP!
  \ ��������� �� ���������� ��������� �����
." activatied  "
;

: stop \  � ��� ��� ���� �� � ���������, ��������� ���������
RT NextTask @ TO _next ( 3)
RT PrevTask @ TO _prev ( 1)
RT NextTask @ _prev NextTask !
RT PrevTask @ _next PrevTask !
\  ������� ���� ������ �� ������ - �������� ����� 
  -1 TaskNum +! \ ��������� ���-�� ����������� ����� �� ����
TaskNum @ IF \ �������, � �� ��������� �� �� ������ ��������???
        \ ��� �� ���������, �������������
 RT TaskID @ ID_free \ ( id task_area -- ) ���������� ���������
\ RT UserArea @ FREE THROW \ ( addr_to _free -- ) ���������� ���������������� ������� ������
   switch
ELSE
\ ��������������� �������� �����, �� ��������� �����
   _S0 @ S0 ! _S @ SP!
   _R @ RP! _R0 @ R0 !
    RT TaskID @ ID_free \ ( id task_area -- ) ���������� ���������
    RT UserArea @ FREE THROW \ ( addr_to _free -- ) ���������� ���������������� ������� ������
THEN ;

: sleep ( -- ) -1 RT Status ! switch ;
: suspend ( id -- ) Ring[] Status -1 SWAP ! ;
: resume ( id -- ) Ring[] Status 0 SWAP ! ;

.( Size of Ring.f is ) HERE SWAP - . CR

\EOF ===================================================================

CR .( SAMPLE, TESTING) CR  0 VALUE RR

\ EOF 

.( 2 - speed testing2) CR
WINAPI: GetTickCount KERNEL32.DLL

0 VALUE TT1 0 VALUE TT2 0 VALUE TT3
.( defining tasks )
: TEST1 1000000 0 ?DO switch LOOP stop ; .( .)
: TEST2 1000000 0 ?DO switch LOOP stop ; .( .)
: TEST3 1000000 0 ?DO switch LOOP stop ; .( .)
.( adding to ring )
10 0 0 ' TEST1 define_task add_to_ring .( .)
10 0 1 ' TEST2 define_task add_to_ring .( .)
10 0 2 ' TEST3 define_task add_to_ring .( .)
.( done) CR
: TEST4 activate ;

.( starting    PRESS ANY KEY TO BEGIN ) CR KEY DROP 

.( Ring speed test...)
GetTickCount
TEST4
GetTickCount
SWAP - 30000000 SWAP / 1000 * . .( switching per second)
.(  done ) CR

\EOF
.( 1 - task switching testing ) CR
0 VALUE RR
: ?? ( id ) \ ����������� ��������� �-��� ������
DUP Ring[] TO RR
. ." -task control block" CR
." ____________________" CR
RR Status @ . CR
RR Priority @ . CR
RR NextTask @ . CR
RR PrevTask @ . CR
RR Task_sp @ . CR
RR Task_rp @ . CR
RR Task_fp @ . CR
\ RR RetAddr @ . CR
RR TaskID @ . CR
RR UserArea @ . CR
;

.( defining tasks )
: TEST1 10 0 ?DO ." TEST1 " I . ." {" S0 @ . SP@ . ." }" 1 .S DROP ." <<1 " CR switch LOOP stop ; .( .)
: TEST2 10 0 ?DO ." TEST2 " I . ." {" S0 @ . SP@ . ." }" 55 77 .S 2DROP ." <<2 " CR switch LOOP stop ; .( .)
: TEST3 10 0 ?DO ." TEST3 " I . ." <<3 " CR switch LOOP stop ; .( .)
.( adding to ring )
10 0 0 ' TEST1 define_task add_to_ring .( .)
10 0 2 ' TEST2 define_task add_to_ring .( .)
10 0 2 ' TEST3 define_task add_to_ring .( .)
.( done) CR
: TEST4 activate ." ring was done" ;
.( starting    PRESS ANY KEY TO BEGIN ) CR STARTLOG KEY DROP TEST4
\EOF



\ $Id: spf_win_con_io.f,v 1.13 2008/03/29 12:33:28 ygreks Exp $

( ���������� ����-�����.
  Windows-��������� �����.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999
  ��������� - Ruvim Pinka ������ 1999
)

: EKEY? ( -- flag ) \ 93 FACILITY EXT
\ ���� ������������ ������� ��������, ������� "������". ����� "����".
\ ������� ������ ���� ���������� ��������� ����������� EKEY.
\ ����� ���� ��� EKEY? ���������� �������� "������", ��������� ����������
\ EKEY? �� ���������� KEY, KEY? ��� EKEY ����� ���������� "������",
\ ����������� � ���� �� �������.
  0 >R RP@ H-STDIN GetNumberOfConsoleInputEvents DROP R>
;

CREATE INPUT_RECORD ( /INPUT_RECORD) 20 2 * CHARS ALLOT

: ControlKeysMask ( -- u )
\ ������� ����� ����������� ������ ��� ���������� ������������� �������.
    [ INPUT_RECORD ( Event dwControlKeyState ) 16 + ] LITERAL @
;

1 CONSTANT KEY_EVENT

: EKEY ( -- u ) \ 93 FACILITY EXT
\ ������� ���� ������������ ������� u. ����������� ������������ �������
\ ������� �� ����������.
\ � ������ ���������� 
\ byte  value
\    0  AsciiChar
\    2  ScanCod
\    3  KeyDownFlag
  0 >R RP@ 2 INPUT_RECORD H-STDIN \ 1 ������� �� 2 (30.12.2001 ~boa)
  ReadConsoleInputA DROP RDROP
  INPUT_RECORD ( EventType ) W@  KEY_EVENT <> IF 0 EXIT THEN
  [ INPUT_RECORD ( Event AsciiChar       ) 14 + ] LITERAL W@
  [ INPUT_RECORD ( Event wVirtualScanCode) 12 + ] LITERAL W@  16 LSHIFT OR
  [ INPUT_RECORD ( Event bKeyDown        ) 04 + ] LITERAL C@  24 LSHIFT OR
;

HEX
: EKEY>CHAR ( u -- u false | char true ) \ 93 FACILITY EXT
\ ���� ������������ ������� u ������������� ������� - ������� ������ �
\ "������". ����� u � "����".
  DUP    FF000000 AND  0=   IF FALSE    EXIT THEN
  DUP    000000FF AND  DUP IF NIP TRUE EXIT THEN DROP
  FALSE
;

: EKEY>SCAN ( u -- scan flag )
\ ������� ����-��� �������, ��������������� ������������� ������� u
\ flag=true - ������� ������. flag=false - ��������.
  DUP  10 RSHIFT  000000FF AND
  SWAP FF000000 AND 0<>
;
DECIMAL

VARIABLE PENDING-CHAR \ ���������� ���� -> ���������� ����������, �� USER

: KEY? ( -- flag ) \ 94 FACILITY
\ ���� ������ ��������, ������� "������". ����� "����". ���� ������������
\ ������������ ������� ��������, ��� ������������� � ������ ����������.
\ ������ ����� ��������� ��������� ����������� KEY.
\ ����� ���� ��� KEY? ���������� �������� "������", ��������� ����������
\ KEY? �� ���������� KEY ��� EKEY ����� ���������� "������" ��� ������������
\ ������������ �������.
  PENDING-CHAR @ 0 > IF TRUE EXIT THEN
  BEGIN
    EKEY?
  WHILE
    EKEY  EKEY>CHAR
    IF PENDING-CHAR !
       TRUE EXIT
    THEN
    DROP
  REPEAT FALSE
;

VECT KEY

: KEY1 ( -- char ) \ 94
\ ������� ���� ������ char. ������������ �������, �� ���������������
\ ��������, ������������� � ����� �� ��������.
\ ����� ���� ������� ��� ����������� �������. �������, ����������� �� KEY,
\ �� ������������ �� �������.
\ ���������, ��������� ����������� ��������� ����������� ��������,
\ ������� �� ���������.
  PENDING-CHAR @ 0 >
  IF PENDING-CHAR @ -1 PENDING-CHAR ! EXIT THEN
  BEGIN
    EKEY  EKEY>CHAR 0=
  WHILE
    DROP
  REPEAT
;
' KEY1 ' KEY TC-VECT!

DIS-OPT \ ��� ������ ���� 4.00 build 10
REQUIRE WINDOWS... ~yz/lib/winlib.f
SET-OPT

0 VALUE win
0 VALUE win2
0 VALUE times

\ -----------------------------------

PROC: quit
  winmain W: wm_close ?send DROP
PROC;

PROC: hello
  " ������!" msg
PROC;

WINAPI: SelectObject GDI32.DLL
WINAPI: TextOutA GDI32.DLL

PROC: paint
  times windc SelectObject DROP
  " ������ ����!" ASCIIZ> SWAP 0 0 windc TextOutA DROP  
PROC;

\ ��������� ����
MENU: inner
  hello MENUITEM ���� ������ 
MENU;

\ �������� ����
MENU: filemenu
  hello MENUITEM &������\tF5
  inner SUBMENU &��������� ����
  ' NOOP DISABLED MENUITEM ���������
  LINE
  quit MENUITEM &Quit\tAlt-X
MENU;

MENU: mainmenu
  filemenu SUBMENU ����
MENU;

\ -------------------------------------
MESSAGES: my

M: wm_contextmenu
  filemenu lparam LOWORD lparam HIWORD show-menu
  TRUE
M;

WINAPI: WinHelpA USER32.DLL

M: wm_help
  0 W: help_helponhelp 0 winmain -hwnd@ WinHelpA DROP
M;

MESSAGES;
\ -------------------------------------

\ ������� ������� ������
KEYTABLE
  hello ONKEY vk_f5
  quit  ONKEY alt+X
KEYTABLE;

WINAPI: CreateHatchBrush GDI32.DLL

: run
  WINDOWS...
  \ 0 - ��� ������������� ����
  0 create-window TO win
  \ ������� ������� ����, ��� �������� �������� ��������� ����������
  win TO winmain
  win dialog-window TO win2
  \ ��������� ����
  " ������ ���� �������� ������" win -text!
  " �������� ����" win2 -text!
  \ ������ � ��������� ��������� ����
  100 100 win2 winresize
  100 100 win2 winmove
  \ ���� ��������� ����
  win2 -bgbrush@ DeleteObject DROP
  blue >bgr W: hs_bdiagonal CreateHatchBrush win2 -bgbrush!
  \ �������� ���� � ��������� ����
  mainmenu win attach-menubar
  \ ������� �����
  " Times New Roman Cyr" 36 bold italic create-font TO times
  \ ���������� ��������� ��������� ����
  paint win -painter!
  \ ���������� ��� ���������� ���������
  my win -wndproc!
  \ �������� ����, ��������� �� ��������� ��� ���������
  win wincenter
  win winshow
  win2 wincenter
  win2 winshow
  ...WINDOWS
  ." ��������� �����������"
  times delete-font
  BYE ;

\ 0 TO SPF-INIT?
 ' ANSI>OEM TO ANSI><OEM
\ TRUE TO ?GUI
\ ' run MAINX !
\ S" winlib-example.exe" SAVE  
run
BYE

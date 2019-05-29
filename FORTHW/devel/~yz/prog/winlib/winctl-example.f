DIS-OPT \ ��� ��-����� ���� 4.10
REQUIRE button ~yz/lib/winctl.f
SET-OPT

0 VALUE ls
0 VALUE g
0 VALUE c
0 VALUE e

PROC: adds { \ [ 255 ] str }
  str e -text@  str c addstring
PROC;

: newcolor 
  thisctl -pos@ 255 SWAP - DUP 255 rgb thisctl -bgcolor! 
  thisctl -pos@ S>D <# 0 HOLD #S #> DROP ls -text! ;

: newpos ( incr -- )
  thisctl -pos@ + thisctl -min@ MAX thisctl -max@ MIN thisctl -pos!
  newcolor ;

MESSAGES: scroll
M: sb_lineleft  -1  newpos M;
M: sb_lineright  1  newpos M;
M: sb_pageleft  -10 newpos M;
M: sb_pageright  10 newpos M;
M: sb_thumbposition
  wparam HIWORD thisctl -pos! newcolor
M;
MESSAGES;

GROUP mm

PROC: dia
  " ������ �������" MODAL...
    GRID 
      mm start-group
      1 " ����" radio | 2 " �����" radio | 3 " �����" radio |
      ===
      multiedit (/ -name e -size 120 25 /) -middle | 
      " >>" button (/ -command adds /) | 
      combo (/ -name c -size 150 300 /) -middle |
      ===
      " 0" label (/ -name ls  -bgcolor blue  -color white /) 10 -width | 
      hscroll (/ -max 255  -notify scroll  -bgcolor white /) -xspan |
    GRID; TO g
    GRID
      1 IMAGE-BASE LoadIconA icon 10 -width | hline -xspan -middle |
      ===
      listbox  " *.*" 0 this lb-dir  (/ -size 120 150 /) -yspan | g |
      ===
      hline -xspan |
      ===
      filler 40 -width |
      "    Ok   " ['] dialog-ok ok-button -right | 
      " ������" cancel-button |
    GRID; SHOW
    dialog-termination W: idcancel = IF 
      ." ������ ��� �������" CR
    ELSE
      mm @ -1 = IF ." ������ �� �������" ELSE ." �����: " mm @ . THEN CR
      ." � ��������������� ������ " 
      c -selected@ -1 = IF  ." ������ �� �������"
      ELSE ." �������: " HERE c -selected@ c fromcombo HERE .ASCIIZ THEN
      CR
    THEN
  ...MODAL
PROC;

KEYTABLE
  dia ONKEY vk_space
KEYTABLE;

: run
  WINDOWS...
  0 dialog-window TO winmain
  " �������� ����������" winmain -text!
  winmain create-status
  " ������� ������, ����� ������� ���������� ����..." 0 winmain set-status
  winmain winshow
  ...WINDOWS
  BYE
;

\ 0 TO SPF-INIT?
 ' ANSI>OEM TO ANSI><OEM
\ TRUE TO ?GUI
\ ' run MAINX !
\ S" winctl-example.exe" SAVE  
run
BYE

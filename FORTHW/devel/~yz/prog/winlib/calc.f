DIS-OPT
REQUIRE button ~yz/lib/winctl.f
REQUIRE >FLOAT lib/include/float2.f
SET-OPT

0 VALUE edit_kg
0 VALUE edit_lbs

0 VALUE edit_from    
0 VALUE edit_to
0 VALUE conv_alg

: status 0 winmain set-status ;

: msg_ok " ������... " status ;

: msg_init " ���� �� ��������... " status ;

: msg_bad_num " ��������� ������������ �����... " status ;

: convert { \ [ 100 ] str len -- } 
  edit_from -text#  TO len  \ �������� ���-�� �������� � ����� �� ����-����������
  " " edit_to -text!        \ �������� ����-�������� 

  str edit_from -text@      \ �������� �� ��������� ������ ��������� �����
  69 str len + C!           \ ������, ��� ����������� ������� � ������� �� ���� � ����� ������� ����� "E"
  str len 1 + >FLOAT        \ ��������� ������� �� float
    IF                      \ ���� ������� ������� ��������� ��������� ���� 
      conv_alg              \ ������� ���������� conv_alg,
      IF                    \ ���� ������ 
        0.456E F/           \ ��������������� kg � lbs
      ELSE                  \ ���� ���� 
        0.456E F*           \ ��������������� lbs � kg
      THEN
      >FNUM                 \ ��������� float � ������� � �������� ��
      str CZMOVE            \ �� ��������� Z ������ 
      str edit_to -text!    \ ���������� ������� � ����-��������� 
      msg_ok
    ELSE      
      msg_bad_num
    THEN      
;

MESSAGES: edit_kg_msg
  M: en_killfocus
    TRUE TO conv_alg        \ �������� � ���������� conv_alg �������� TRUE
                            \ ��� ��������, ��� �������������� ���� �� kg � lbs
    edit_kg TO edit_from    \ ���������� ����-��������
    edit_lbs TO edit_to     \ ���������� ����-����������
    convert
  M;
MESSAGES;

MESSAGES: edit_lbs_msg
  M: en_killfocus
    FALSE TO conv_alg
    edit_lbs TO edit_from
    edit_kg TO edit_to
    convert
  M;
MESSAGES;

: about
  " � ���������"        \ ��������� ����
                        \ ���������� ����
  " ��������� ��� �������� KG � LBS � ������� \n����������� ����� � ������� ����� - ����� \n\n\t������ 1.5 \n\n������� �� ����-������� SPF v4.16 \n\n\t\t����� - �������� �.�. \n\t\t�-��������, 2005 �." 
  0x0000 message-box    \ ������� ��������� � ���� ��� ������
  DROP                  \ ������ �� ����� ��� ������� �������
;

MESSAGES: main_messages
M: wm_help
  about
  TRUE
M;
MESSAGES;

: run
  WINDOWS...
  0 dialog-window TO winmain
  " �����������" winmain -text!

  \ ��������� ����� � ���� �����
  GRID
  ===
    GRID
    ===
      filler |
    ===  
      " KG" label (/ -align right /) -left | edit (/ -name edit_kg -size 120 20 -notify edit_kg_msg /) -right |
    ===
      " LBS" label (/ -align right /) -left | edit (/ -name edit_lbs -size 120 20 -notify edit_lbs_msg /) -right |
    ===
    " KG-LBS" groupbox cur-grid @ :gbox !
    GRID; |
  ===
  filler |
  ===
  GRID;  winmain -grid!

  32 edit_kg limit-edit     \ ������������ ����� �������� �������� � ����� �����
  32 edit_lbs limit-edit    \    

  \ ������� ��������� ������ � ������� �� ��� ��� ������� Enter
  " �������� " button
  -defbutton

  edit_kg winfocus

  winmain create-status
  msg_init

  \ ���������� ��� ���������� ���������
  main_messages winmain -wndproc!

  winmain wincenter
  winmain winshow

  ...WINDOWS
  BYE
;

\ 0 TO SPF-INIT?
 ' ANSI>OEM TO ANSI><OEM

\ TRUE TO ?GUI
\ ' run MAINX !
\ S" calc.exe" SAVE  

run

\EOF

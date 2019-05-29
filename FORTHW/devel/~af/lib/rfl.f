\ Andrey Filatkin, af@forth.org.ru
\ Work in spf3, spf4
\ Recent File List

REQUIRE {                 ~af/lib/locals.f
REQUIRE GetIniString      ~af/lib/ini.f
REQUIRE USES              ~af/lib/api-func.f
REQUIRE FileExist         ~af/lib/fileexist.f
REQUIRE STR@              ~ac/lib/str2.f
REQUIRE PAllocSupport     ~af/lib/pallocate.f

USES user32.dll

[UNDEFINED] WITHIN [IF]
  : WITHIN ( n1|u1 n2|u2 n3|u3 -- flag ) \ 93 CORE EXT
    OVER - >R - R> U<
  ;
[THEN]

   0 CONSTANT MF_BYCOMMAND
1024 CONSTANT MF_BYPOSITION
   0 CONSTANT MF_STRING

0 VALUE IdFirstRFL \ ������ �������������
0 VALUE MaxCountRFL \ ������������ ����� ������ � ������
0 VALUE RFLBefore \ ��� ��������� ������ � ����
0 VALUE hRFLMenu \ ���� ���������� ������

VOCABULARY RFLSupport
GET-CURRENT ALSO RFLSupport DEFINITIONS

0 VALUE RFList \ ����, �������� ����� ������. 9 �����, 0-��� ������ - ������
0 VALUE inifile  \ ��� �����, � ������� �������� ������
0 VALUE rflsection \ ��� ������ rfl

ALSO PAllocSupport

\ � spf3 ����� �� �������� � ALLOCATE, �������� ������������
\ ���������� ALLOCATE
: HEAP-PROCESS-COPY ( addr u -- addr1 )
\ ����������� ������ � ��� ��������� � ������� � ����� � ����
  0 MAX
  DUP 1+ ALLOCATE THROW DUP >R
  SWAP DUP >R MOVE
  0 R> R@ + C! R>
;

: AddRFLNode ( addr u -- )
  \ �������� ���������� � ������
  RFList 8 CELLS + @ ?DUP IF FREE DROP THEN
  \ ����� ������
  RFList DUP CELL+ 8 CELLS MOVE
  \ ���������� ������ ����� � ������
  HEAP-PROCESS-COPY RFList !
;

PREVIOUS

: LoadRFList ( -- )
  1024 ALLOCATE THROW >R
  inifile rflsection R@ 1024 EnumSectionKeys
  IF
    R@
    BEGIN
      DUP C@
    WHILE
      DUP inifile rflsection ROT S" " DROP GetIniString
      ASCIIZ> 2DUP FileExist IF AddRFLNode ELSE 2DROP THEN
      ASCIIZ> + 1+
    REPEAT
    DROP
  THEN
  R> FREE THROW
;
: SaveRFList ( -- ) { \ cnum tmp -- }
  inifile rflsection DeleteIniSection
  0 TO cnum
  0 MaxCountRFL 1- DO
    RFList I CELLS + @ IF
      cnum 1+ TO cnum
      inifile
      rflsection
      cnum " File{n}" DUP TO tmp STR@ DROP
      RFList I CELLS + @
      SetIniString
      tmp STRFREE
    THEN
  -1 +LOOP
;
: MoveToTopRFL ( node -- )
  ?DUP IF
    CELLS RFList + DUP @ >R
    RFList SWAP DO
      I CELL- @ I !
    -1 CELLS +LOOP
    R> RFList !
  THEN
;
: SeekInRFL ( addr u -- node true\ false )
  2DUP SWAP CharLowerBuff DROP
  9 0 DO
    RFList I CELLS + @ ?DUP IF
      ASCIIZ> 2OVER COMPARE
      0= IF 2DROP I TRUE UNLOOP EXIT THEN
    THEN
  LOOP
  2DROP FALSE
;
: ShowRFLMenu ( -- ) { \ tmp -- }
  RFList
  MaxCountRFL 0 DO
    DUP @ 0= IF DUP DUP CELL+ SWAP 8 I - CELLS MOVE THEN
    CELL+
  LOOP DROP
   0 MaxCountRFL 1- DO
    RFList I CELLS + @ ?DUP IF
      ASCIIZ> I 1+ " &{n}  {s}" DUP TO tmp STR@ DROP
      IdFirstRFL I +
      MF_STRING MF_BYPOSITION OR  RFLBefore hRFLMenu
      InsertMenu DROP
      tmp STRFREE
    THEN
  -1 +LOOP
;
: ClearRFLMenu ( -- )
  9 0 DO
    MF_BYCOMMAND  IdFirstRFL I +  hRFLMenu  DeleteMenu DROP
  LOOP
;

SET-CURRENT

ALSO PAllocSupport

: CreateRFL ( addr_ini addr_section -- )
  TO rflsection TO inifile
  9 CELLS ALLOCATE THROW TO RFList
  LoadRFList
;
: FreeRFL ( -- )
  ClearRFLMenu
  9 0 DO
    RFList I CELLS + @ FREE DROP
  LOOP
  RFList FREE DROP
;

PREVIOUS

: RefreshMenu ( -- )
  ClearRFLMenu ShowRFLMenu
;
: AddToRFL ( addr u -- )
  2DUP SeekInRFL IF MoveToTopRFL 2DROP
  ELSE AddRFLNode THEN
  SaveRFList
  RefreshMenu
;
: RFLClick? ( id -- addr u true \ false )
  DUP IdFirstRFL DUP MaxCountRFL + WITHIN IF
    IdFirstRFL -
    DUP CELLS RFList + @ ?DUP IF
      ASCIIZ> 2DUP FileExist IF
        ROT MoveToTopRFL TRUE
      ELSE
        2DROP
        CELLS RFList + 0!
        FALSE
      THEN
    THEN
    SaveRFList
    RefreshMenu
  ELSE DROP FALSE THEN
;

PREVIOUS

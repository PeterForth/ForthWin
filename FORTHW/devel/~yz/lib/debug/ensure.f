
\ REQUIRE _LINE_ lib/ext/debug/accert.f

: _LINE_
\ ����������� ��������� ������� - u - ����� ������� ������
  CURSTR @ 0 <# #S #> [COMPILE] SLITERAL
; IMMEDIATE

: _FILE_
\ ����������� ��������� ������� - ��� �������� ����� ����������
  CURFILE @ ASCIIZ> [COMPILE] SLITERAL
; IMMEDIATE

: (ENSURE) CR TYPE ." : line " TYPE ."  - ENSURE FAILED !" CR ;

: ENSURE ( ? -- ) S" 0= IF _LINE_ _FILE_ (ENSURE) 12345 THROW THEN" EVALUATE ; IMMEDIATE

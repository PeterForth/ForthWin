REQUIRE /STRING lib/include/string.f
REQUIRE PLACE ~mak/place.f
REQUIRE { ~ac/lib/locals.f


CREATE str1 256 ALLOT   S" First line" str1 PLACE
CREATE str2 256 ALLOT
CREATE str3 256 ALLOT
CREATE str4 256 ALLOT
: ?? COUNT TYPE CR ; 
  \ TYPE ���⠥� ��ப� (�뢮��� � �⠭����� �뢮�, � �஭� - � nncron.out)
  \ CR - ��ॢ���� ��ப�
  \ ᫮�� str1 �� �ᯮ�짮����� ������ ���� ��ப� �� �⥪
  \ �⮡� �८�ࠧ����� ��� ���� � ���� � ������ ��. ᫮�� COUNT
str1 ( a -- ) COUNT ( a+1 len --) TYPE CR
  \ ��ப� �࠭���� � ����: N byte1 byte2 ... byteN
  \ �� ⠪ ���뢠��� ��ப� � ���稪��.
  \ ����� PLACE (�� �� �⠭���⭮�� �����, �� � �஭� ����) ����頥� ��ப� � 㪠������ ����
S" �� ��ࢠ� ��ப�" str1 PLACE
  \ +PLACE �������� ��ப� � 㪠������ ��ப�
S" , � �� � ��� �������" str1 +PLACE
  \ /STRING 㪮�稢��� �������� ��ப� (⮫쪮 � �⥪�)
str1 COUNT 4 /STRING TYPE CR
  \ १���� �㤥� ⠪��: ��ࢠ� ��ப�, � �� � ��� �������
  
  \ StringLeft str2 str1 10
str1 COUNT 10 MIN str2 PLACE                        str2 ??
  \ � ��� � ᫮��
: StringLeft ( a1 u len -- a1 len) MIN ;
  \ �ਬ������ ⠪:
str1 COUNT 10 StringLeft str2 PLACE                 str2 ??

  \ StringRight str2 str1 10
str1 COUNT DUP 10 - 0 MAX /STRING str2 PLACE        str2 ??
  \ ��।������
: StringRight ( a1 u len -- a2 len) OVER SWAP - 0 MAX /STRING ;
str1 COUNT 10 StringRight str2 PLACE                str2 ??

  \ StringMid str2 str1 5 12
str1 COUNT 5 /STRING 12 MIN str2 PLACE              str2 ??

: StringMid ( a1 u pos len -- ) >R /STRING R> MIN ;
str1 COUNT 5 12 StringMid str2 PLACE                str2 ??

  \ StringTrimLeft str2 str1 5
str1 COUNT 5 /STRING str2 PLACE                     str2 ??
  \ StringTrimRight str2 str1 5
str1 COUNT 5 - 0 MAX str2 PLACE                     str2 ??

  \ StringReplace str2 str1 str3 str4
  \ �� ᫮��窮 ��᫮����, �� �� ���⮫쪮 �⮡� ��� �� ��ॢ�ਫ.
  \ �ࠢ�� ����ᠭ� ��� �㤥� � ��⮬ 㪠������ ��ப, �.�. ��
  \ 㭨���ᠫ쭮.
: StringReplace ( -- )
  str2 0!
  str1 COUNT
  BEGIN OVER SWAP str3 COUNT SEARCH WHILE
    >R SWAP 2DUP - str2 +PLACE
    str4 COUNT str2 +PLACE
    R> str3 C@ /STRING
  REPEAT
  str2 +PLACE
  DROP
;

S" �� ��ப� - �� �祭� ��訩 ��ࠧ��. � �� ⮫쪮 ��ࠧ��." str1 PLACE
S" �� " str3 PLACE
S" " str4 PLACE \ �� ������ ��ப� ��������
StringReplace str2 ??

 \ �᫨ ��ᯮ�짮������ ������묨 ��६���묨, � ����� ᤥ���� �� ᫮�� � 
 \ 㭨���ᠫ��
: StringReplace2 { a2 a1 u1 a3 u3 a4 u4 \ rest -- a2 u2 }
  a2 0!
  a1 u1
  BEGIN OVER SWAP a3 u3 SEARCH WHILE
    TO rest SWAP 2DUP - a2 +PLACE
    a4 u4 a2 +PLACE
    rest u3 /STRING
  REPEAT
  a2 +PLACE
  DROP
  a2 COUNT
;

str2 S" �� ��ப� - �� ��. ��. ���. � �� ⮫쪮 ���." S" �� " S" " StringReplace2 TYPE CR

\ StringGetPos pos str1 str2
: StringGetPos { a1 u1 a2 u2 -- pos }
    a1 u1 a2 u2 SEARCH IF DROP a1 - 1+ ELSE 2DROP 0 THEN ;
\ ����樨 ��稭����� � 1. �᫨ �����ப� �� ������� - 0

S" 123456789" S" 567" StringGetPos . CR

\ �. �������, http://www.forth.org.ru/~yz
\ ������ ������������� ���������� Automate: ���� FOREACH ... NEXT

REQUIRE [[ ~yz/lib/automate.f
REQUIRE { lib/ext/locals.f

0 VALUE shell
0 VALUE folder
0 VALUE item

: gd S" getDetailsOf" ;

: Z.R ( z n -- )
  OVER .ansiz SWAP ZLEN - 0 MAX SPACES ;

: show-desktop

 ComInit DROP

 " Shell.Application" CreateObject
 IF " �� ���� ��������� ������ Shell.Explorer" .ansiz BYE THEN
 TO shell

 shell [[ NameSpace ( 17 ) ]] TO folder

  " ������" 20 Z.R  " ���" 25 Z.R  " ������" 12 Z.R " ��������" 12 Z.R CR
 69 0 DO c: - EMIT LOOP CR

 folder [[ Items ]] FOREACH
   OBJ-I TO item
   folder [[ GetDetailsOf ( item OBJECT , 1 ) ]] DUP 20 Z.R FREEMEM 
   folder [[ GetDetailsOf ( item OBJECT , 0 ) ]] DUP 25 Z.R FREEMEM 
   folder [[ GetDetailsOf ( item OBJECT , 2 ) ]] DUP 12 Z.R FREEMEM 
   folder [[ GetDetailsOf ( item OBJECT , 3 ) ]] DUP 12 Z.R FREEMEM 
   CR
   item release
 NEXT

folder release
shell release

ComDestroy ;

show-desktop

BYE

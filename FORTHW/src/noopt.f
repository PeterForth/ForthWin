\ $Id: noopt.f,v 1.4 2008/03/23 08:38:42 ygreks Exp $
\ ��������/���������� ������������
\ http://www.fforum.winglion.ru/viewtopic.php?p=1929#1929

: SHORT? ( n -- -129 < n < 128 ) 
  0x80 + 0x100 U< ; 

BASE @ HEX 

FALSE VALUE OPT? 
084 VALUE J_COD 
0 VALUE MM_SIZE 
0 VALUE :-SET 
0 VALUE J-SET 
0 VALUE LAST-HERE 
0x4 CELLS DUP CONSTANT OpBuffSize 
CREATE OP0 HERE >T  , 0 ,  ALLOT 
: SetOP ; IMMEDIATE 
: ClearJpBuff ; IMMEDIATE 
: SetJP ; IMMEDIATE 
: ?SET ; IMMEDIATE 
FALSE VALUE ?C-JMP 
0 CONSTANT INLINE? 
: OPT_CLOSE ; IMMEDIATE 
: OPT_INIT ; IMMEDIATE 
: INLINE, 
 BEGIN COUNT DUP C3 <> 
 WHILE C, 
 REPEAT 2DROP ; 
: ???BR-OPT 
  C00B W,    \ OR EAX, EAX 
  'DROP INLINE, ; 
: OPT ; IMMEDIATE 
TRUE CONSTANT CON>LIT 
 FALSE VALUE J_OPT? 
: RESOLVE_OPT DROP ; 

: INIT-MACROOPT-LIGHT ;

: MACRO, INLINE, ;
: SET-OPT TRUE TO OPT? ;
: DIS-OPT FALSE TO OPT? ;

 BASE !

( \ ���� �����⢮���� � Bernd Paysan { bernd.paysan@gmx.de }
_______����� 1.1.2______

���஡㥬 ᤥ���� ���� �த� ᮯ����� ��� �࣠����樨 ��ࠫ������ 
���᫥��� - 4 �⥪� � ����஬ �㭪権, ���� ��� ����� ॣ���஢ R N L {F}
��� � ������ {�. ��� ���㬥����}

-> �⥪ - ��� ��᫥����⥫��� ���� � ����� ���� - ��ᥬ� �祥�, + 㪠��⥫�
�� ������� ����� ��� ����ண� ��࠭����
-> ���室�, �᫮��� � ��祥 ���� �� ᮢ��� ���
-> 
)
\ �।���������� ������� ��� �ࠢ����� �⨬ �����ࠧ���
(
����� - ���-�� ��⨢��� �⥪��, ⥪�騩 �⥪;
��� ������� �⥪� - ���, 㪠��⥫�;
)

\ �������� �㤥� �������
.( loading multi stacks extention v1.01 ) CR

( for gforth capability define this...
: 2- 2 - ;
: CELL 4 ; : CELLS 4 * ;
: CELL- CELL - ; 
)

VOCABULARY 4xSTACK
USER StNum USER stack 	\ ��饥 ���-��, ⥪�騩, ��㡨��
USER Sp USER St	USER ssize	\ 㪠��⥫�, ���
USER RealS0 USER RealSP

256 ssize ! \ -��㡨�� �� 㬮�砭��

\ _________�ࠢ�����

: -stacks ( N -- )	 	\ �⢥�� ����� ��� �⥪�, ��������� ����
				\ ��� ����権 � ����栬�, ����� ����� �⥪
				\ �⠭������ �⮫�殬\��ப�� �뤥����� ����
				\ ����让 ��᮪ �����
DUP StNum ! 4 CELLS * ALLOCATE THROW \ ." ����� ��﫨" \ addr
DUP St ! StNum @ CELLS + Sp !
ssize @ CELLS StNum @ * DUP ALLOCATE THROW CELL- +
StNum @ 0 ?DO
  DUP ssize @ CELLS I * - 
  DUP St @ I CELLS + ! Sp @ I CELLS + !
LOOP DROP
S0 @ RealS0 ! SP@ RealSP ! ;

: -stacks_diff ( xn ... x0 N -- ) 	\ �⢥�� ����� ��� �⥪�, ��������� ����
				\ ��� ����権 � ����栬�, ����� ����� �⥪
				\ �⠭������ �⮫�殬\��ப�� �뤥����� ����
				\ ����让 ��᮪ �����
DUP StNum ! 4 CELLS * ALLOCATE THROW \ ." ����� ��﫨" \ addr
DUP St ! StNum @ CELLS + Sp !
ssize @ CELLS StNum @ * DUP ALLOCATE THROW CELL- + \ ��᫥���� ������ ����
StNum @ 0 ?DO
  DUP ROT CELLS I * - 
  DUP St @ I CELLS + ! Sp @ I CELLS + !
LOOP DROP
S0 @ RealS0 ! SP@ RealSP ! ;

: :st ( n -- ) 		\ ������ ⥪�騬 �⥪ � ������� ����஬ - ��४��祭�� �⥪��
CELLS SP@ CELL+ stack @ Sp @ + ! \ ." ��������� ⥪�騩 㪠��⥫� " .S
DUP Sp @ + @ SWAP 	\ ." ��﫨 㪠��⥫� " .S
DUP St @ + @ 		\ ." ��﫨 ��� " .S 
SWAP stack !
S0 ! SP! ;

: :named_stack ( n -- ) \ ᮧ���� ���������� �⥪, n ��� �����
CREATE ,
DOES> @ :st \ �� �ᯮ������ ��४��砥� ⥪�騩 �⥪
\ ���쭥�訥 ����樨 �ந�室�� �� ���
;

: start_stacks		\ �����⨫� ��⥬�: 0-� �⥪ ⥪�騩.
SP@ S0 @ RealS0 ! RealSP ! 0 stack ! Sp @ @ St @ @ S0 ! SP! ;

: end_stacks		\ ��⠭�� ��⥬� - �� ������ �� �᢮���������
			\ ������ �� �⥪, ����� ��
SP@ CELL+ stack @ Sp @ + !
RealSP @ RealS0 @ S0 ! SP! ;

: free_stacks		\ �᢮������� ������ ��᫥ ���� �ࠦ�����
St @ @ ssize @ CELLS StNum @ * CELL- - FREE THROW ;

\ _________����樨 � ����⠬� �⥪��

: s@ ( n i - x )	\ n-� ����� i-��� �⥪� �� ���設� ⥪�饣�
CELLS Sp @ + @ SWAP CELLS + @ ;

: s! ( x n i - )	\ ������ n-�� ����� i-��� �⥪� � ���設� ⥪�饣�
CELLS Sp @ + @ SWAP CELLS + ! ;

	\ ����� � ���祭�� �⥪� ��� � ���ᨢ�
	\ �.�. �㬥��� ⥯��� � ���, �஢�ન ���-�� �������

: []@ ( n i - x )	\ ����砥� n-� ����� � ��� i-�� �⥪�
CELLS St @ + @ SWAP 1+ CELLS - @ ;

: []! ( x n i -  )	\ �����뢠�� n-� ����� � ��� i-�� �⥪�
CELLS St @ + @ SWAP 1+ CELLS - ! ;

: PIN ( xn ... x0 -- x0 xn ... x1 ) \ ���設� �⥪� ��६�頥� �� ���
DUP SP@ DUP CELL- DEPTH 2- CELLS CMOVE S0 @ CELL- ! ;

\ PREVIOUS

\ ALSO FORTH DEFINITIONS

\ PREVIOUS FORTH DEFINITIONS
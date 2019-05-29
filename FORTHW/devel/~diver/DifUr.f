 lib\include\float2.f

0.E FVALUE Xn \ - ��砫�� �᫮���
0.E FVALUE Yn \ -
0.E FVALUE tn \ - ��砫�� ������ �६���
0.E FVALUE h \ 蠣
0.E FVALUE X0 \ - ��砫�� �᫮���
0.E FVALUE Y0 \ -
0.E FVALUE t0
0.E FVALUE k1
0.E FVALUE q1
0.E FVALUE k2
0.E FVALUE q2
0.E FVALUE k3
0.E FVALUE q3
0.E FVALUE k4
0.E FVALUE q4
0.E FVALUE temp
\ ______������� �-�� ��⥬�_________

: F() ( f: r1 r2 r3 -- r )
S" f()" SFIND IF EXECUTE ELSE FDROP FDROP FDROP 0.E DROP THEN ;
: G() ( f: r1 r2 r3 -- r )
S" g()" SFIND IF EXECUTE ELSE FDROP FDROP FDROP 0.E DROP THEN ;

\ ______�� ��⮤�__________________

: K1 tn Xn Yn F() FTO k1 ;
: Q1 tn Xn Yn G() FTO q1 ;

: XY_ ( -- f: t x y )
h 2.E F/ tn F+ \ fdepthmax 2
h 2.E F/ k1 F* Xn F+ \ fdepthmax  3
h 2.E F/ q1 F* Yn F+ \ fdepthmax 5
;

: K2 XY_ F() FTO k2 ;
: Q2 XY_ G() FTO q2 ;

: XY__ ( -- f: t x y )
h 2.E F/ tn F+ \ fdepthmax 1
h 2.E F/ k2 F* Xn F+ \ fdepthmax  3
h 2.E F/ q2 F* Yn F+ \ fdepthmax 4
;

: K3 XY__ F() FTO k3 ;
: Q3 XY__ G() FTO q3 ;

: XY\ ( -- f: t x y )
tn h F+
h k3 F* Xn F+
h q3 F* Yn F+ ;

: K4 XY\ F() FTO k4 ;
: Q4 XY\ G() FTO q4 ;

: Xn+1 ( -- f: Xn+1 )
2.E FDUP k2 F* FSWAP k3 F* F+ k1 F+ k4 F+ h F* 6.E F/ Xn F+ ;

: Yn+1 ( -- f: Yn+1 )
2.E FDUP q2 F* FSWAP q3 F* F+ q1 F+ q4 F+ h F* 6.E F/ Yn F+ ;

: X_Y ( -- f: Xn+1 Yn+1 )
K1 Q1 K2 Q2 K3 Q3 K4 Q4
Xn+1 Yn+1 ;

\ ____᫮�� ��� ���� ���ᨢ�� ������_________

\ __������� �᫠ ���権, 蠣� � ��祣�______
USER N \ �᫮ ���権
USER N_ \ �᫮ ������ �� ��室 - ���� �᪨�뢠���� �� ��������� ���権
USER STEP_ \ 蠣 ���� १���⮢
: �᫮���
\ �⠥� �᫮��� �ࠢ����� �� 䠩��:
\   ������⢮ ���権,
\   蠣,
\   ��砫�� �᫮���
S" default.dat" INCLUDED
FTO h FDUP FTO t0 FTO tn FDUP FTO Y0 FTO Yn FDUP FTO X0 FTO Xn
N_ ! \ 蠣 ����
N ! \ ��������� ������⢮ ���権
;

USER x_addr USER y_addr

: ��襭�� ." ��襭�� "
N @ 4 + 10 * DUP 2* ALLOCATE THROW SWAP OVER + \ x_addr y_addr
y_addr ! x_addr ! \ ������ ����� ��� १����� ���⮢
y_addr @ Y0 F! x_addr @ X0 F! 10 y_addr +! 10 x_addr +!
N @ 0 ?DO
	X_Y 
	FDUP 10 I * y_addr @ + F! FTO Yn
	FDUP 10 I * x_addr @ + F! FTO Xn
	tn h F+ FTO tn
LOOP
-10 y_addr +! -10 x_addr +! y_addr @ F@ ." �����稫� ����!!! " ;

USER fh
: ���࠭塞�� ." ���࠭塞�� "
S" result.dat" R/W CREATE-FILE THROW fh !
PRINT-EXP
N @ N_ @ / STEP_ !
N_ @ 0 ?DO
	x_addr @ 10 I * STEP_ @ * + F@ >FNUM fh @ WRITE-FILE THROW
	S"  " fh @ WRITE-FILE THROW
	y_addr @ 10 I * STEP_ @ * + F@ >FNUM fh @ WRITE-LINE THROW
LOOP
fh @ CLOSE-FILE THROW x_addr FREE DROP
;

: ��蠥��ࠢ�����
�᫮��� ��襭�� ���࠭塞��
;

(
OPT
 ICONS: mine32x32.ico mine16x16.ico
\ TRUE TO ?GUI
 \   FALSE TO ?CONSOLE
     ' NOOP MAINX !
      ' ��蠥��ࠢ����� TO <MAIN>
        S" DiffUr.exe" SAVE

\EOF )

��蠥��ࠢ����� BYE
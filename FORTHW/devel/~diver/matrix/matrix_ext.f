( ᫮�� �࣠����樨 ��� ����䥩� � ᫮���� ������⥪ FSL
����祢 �.�. 8.11.2002 diver@forth.org.ru
�������᪨� ���ᨢ� � ������
�ᯮ������ ᮢ���⭮ � matrix2_1.f � matrix_3d.f
  ᫮��:
      �⮫���_�_�����  ��ப�_�_�����  �����_�_�⮫���  �����_�_��ப�
      �������_�_�����  �����_�_�������
�����⢫��� ����� ����묨 ����� ����楩 � �������᪨� �������� ���ᨢ��,
� �ଠ� fsl-util.f . �ந�室�� �뤥����� ᨣ���� �� �⮫��/��ப� ������,
����㧪� ��� � �������᪨� ���ᨢ, ��� �� ����㯥� ��� ���쭥�襩 ��ࠡ�⪨,
� ���㧪� ��� �� ���� ᤥ���� �२����⢥��� ��� ᮢ���⨬��� � ��⥬����-
��� ������⥪�� Forth Sietific Library { FSL }, ��� �� ��⥣�樨 � SPF

  �����!!!: ����室��� ᫥���� �� ��४��祭��� ⨯�� ������ � �ணࠬ��,
  ࠧ��୮��� {cell} �������᪮�� ���ᨢ� ��⠭���������� set_cell
)

S" fsl-util" SFIND 0= [IF] 2DROP
~diver\fsl-util.f    \ 䠩� "ᮣ��ᮢ����" � FSL
~diver\dynmem.f      \ �뤥����� ����� ��� ���. ���ᨢ�
[ELSE] DROP [THEN]

S" ����������" SFIND 0= [IF] 2DROP ~diver\matrix\matrix2_1.f [ELSE] DROP [THEN]
S" ���������ᨢ" SFIND 0= [IF] 2DROP ~diver\matrix\matrix_3d.f [ELSE] DROP [THEN]

HERE

: set_cell ( new_cell_of_array addr -- ) \ sample: cell & real{ set_cell  
>BODY CELL+ !
;
: �⮫���_�_����� ( col id addr -- )   \ ����� �⮫��, id-������, ���� ���ᨢ�-�����⥫�
addr ! �������3 TO col              \ sample: 5 matr1 real{ �⮫���_�_�����  
��ப3 0 ?DO
I col ��砫�����3 ����읫����� addr @ I } ��������� 
LOOP
;
: ��ப�_�_����� ( col id addr -- )   \ ����� �⮫��, id-������, ���� ���ᨢ�-�����⥫�
addr ! �������3 TO row
�⮫�殢3 0 ?DO
row I ��砫�����3 ����읫����� addr @ I } ���������
LOOP
;
: �����_�_�⮫��� ( addr col id -- )   \ ����� �⮫��, id-������, ���� ���ᨢ�-�����⥫�
�������3 TO col addr !              \ sample: real{ 5 matr1 �����_�_�⮫���  
��ப3 0 ?DO
addr @ I } ����� I col ��砫�����3 ���읫�����
LOOP
;
: �����_�_��ப� ( addr col id -- )    \ ����� �⮫��, id-������, ���� ���ᨢ�-�����⥫�
�������3 TO row addr !              \ sample: real{ 5 matr1 �����_�_��ப�  
�⮫�殢3 0 ?DO
addr @ I } ����� row I ��砫�����3 ���읫�����
LOOP
;
: �������_�_����� ( row col id addr -- )
addr ! -ROT TO col TO row 
DUP W@ OVER 2+ W@ * OVER CELL+ + 
SWAP W@ 0 2SWAP ( sm 0 addr2 addr1 )
 CELL+ DO addr @ OVER } TO �६����� 
    row col I ����읫����� �६����� ���������
    1+ OVER
 +LOOP 2DROP
;
: �����_�_������� ( addr row col id -- )
2SWAP SWAP addr ! TO row SWAP TO col
DUP W@ OVER 2+ W@ * OVER CELL+ + 
SWAP W@ 0 2SWAP ( sm 0 addr2 addr1 )
 CELL+ DO addr @ OVER } ����� 
    row col I ���읫�����
    1+ OVER
 +LOOP 2DROP
;
HERE SWAP -
CR .( MATRIX <=> ARRAYS V1.00          08 November 2002   --  ) . .( bytes) 

\EOF ���஢����
CR
䫮���
  0 VALUE M1
4 4 ���������� TO M1
M1 ��ନ஢���
1.e m, 1.e m, 1.e m, 1.e m,
0.e m, 0.e m, 0.e m, 0.e m,
0.e m, 0.e m, 0.e m, 0.e m,
0.e m, 0.e m, 0.e m, 0.e m,
��������

8 DARRAY real{  \ �������᪨� ���ᨢ ��� ॠ���� �ᥫ
8 DARRAY imm{   \ �������᪨� ���ᨢ ��� ������ �ᥫ

 & real{ 100 }malloc
 2 M1 real{ �⮫���_�_�����
4 real{ }fprint
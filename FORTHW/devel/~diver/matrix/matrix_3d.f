\ ࠡ�� � ��嬥�묨 ���ᨢ���
\ ����祢 �.�. 7.11.2002 diver@forth.org.ru
\ ������ ���ᨢ� - ��㬥�� ������
\ ��ଠ� ���ᨢ� -      4 - 㪠��⥫� �� ������� ����� ᮤ�ঠ騩 ���ᨢ
\ ������� �����:  ࠧ���  ���ᠭ��
\                  2  -  ᫮��(lay) 
\                  2  -  ࠧ��� ��������=====-+
\                  2  -  �⮫�殢(row)       | |
\                  2  -  ��ப(col)          | } ࠧ��� �������� (��� ����७���� ���짮�����)
\                  n  -  ⥫� ������        |_|
\                  .      .                  } �� ���-�� ᫮�� (������� ������) 
\                  .      .                  |
\                  2  -  �⮫�殢            |
\                  2  -  ��ப               |
\                  n  -  ⥫� ������   _____|
\ ������� ��⠢���� ��� ᮢ���⨬��� � ����筮� ������⥪��, ���� �� ����� ��譨� ࠧ
\ ���� � � �� �㭪樨. 

S" ����������" SFIND 0= [IF] 2DROP ~diver\matrix\matrix2_1.f [ELSE] DROP [THEN]   
HERE

: ���������ᨢ ( row col lay -- addr )    \ ���ᨢ �㤥� ������ �������� �����
ROT
2DUP 2>R * ᬥ饭��* CELL+  \ ࠧ��� ����� ��� ���� ᫮�
OVER >R * CELL+             \ ��� ���� ���ᨢ  
ALLOCATE THROW              \ .S ." ( \ ���� ��� ���ᨢ)" CR 
R@ OVER ! CELL+ addr !      \ ��������� ���-�� ᫮��, � addr - ��砫� ������ ������ 
R> 2R> 2DUP * ᬥ饭��* CELL+ ( Z Y X Y*X+cm ) 0 SWAP    \ .S ." ( Z Y X 0 Y*X+cm )" CR
4 ROLL 2DUP * addr @ + NIP  ( Y X 0 Y*X+cm Y*X+cm*Z+addr ) 
  addr @ DO 2OVER I W! I 2+ W! DUP +LOOP  \ .S ." ( Y X 0 Y*X+cm )" \ �����뢠�� �� ���ᨢ� ��ࠬ���� �����
addr @ 2- W! DROP 2DROP addr @ CELL- 
;
: ᫮� ( n addr -- addr1 ) \ �����頥� ���� n-�� ᫮� 
DUP >R 2+ W@ * R> +  
;
: �������? ( row col lay addr --  ) \ !!! �⥪���� ����� ������ �� ⨯� ������
᫮� ����읫�����                   \ �������� ���� ��� ��� ⨯��
;
: �������! ( row col lay addr --  )
᫮� ���읫����� 
;

HERE SWAP - \ .( 	matrix_3d.f -- ) . .( bytes) CR
CR .( MATRIX ARRAYS     V1.00          07 November 2002   --  ) . .( bytes) 
\ CR .( MATRIX            V2.21          09 October  2002   --  ) . .( bytes) 

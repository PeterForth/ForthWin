(
���஢�� ��ࠡ�⪠ �����
����������:
  - �८�ࠧ������ ���� �� �⮫�栬, �� ��ப��, ��㬥୮� { ��� ࠧ��୮�⥩
  2**N };
  - 䨫���� ᨭ⥧�஢���� � �� ����-��ମ�����;
)
~diver\matrix\matrix_ext.f
HERE
c:\temp\diver\forth\fsl\complex.seq
c:\temp\diver\forth\fsl\cmath.fth
c:\temp\diver\forth\fsl\ffourier.seq
HERE SWAP - SPACE . .( bytes)
\ �।�⠢����� ����� ����樨 ࠡ���� � �᫠�� � ������饩 �窮�

HERE

 Private:

\ �㤥� ����� ��� �������᪨� ���ᨢ� �� ��直� ��砩: ��� ॠ�쭮� � ��� ������ ��� �ᥫ
\ ��� �࣠����樨 ��ࠡ�⪨ �����: ���� ᯥ����, 䨫����, ���᫥��� ᯥ�. �㭪権
8 DARRAY real{  \ �������᪨� ���ᨢ ��� ॠ���� �ᥫ
8 DARRAY imm{   \ �������᪨� ���ᨢ ��� ������ �ᥫ
( �ॡ����� - float, complex, complex_ext, fsl, dynmem, matrix, ... )
USER-VALUE direction \ 1 ��אַ� �८�ࠧ������, -1 ���⭮�

 Public:

USER-VECT }}���
' FFT2-2T TO }}��� \ 5ms 16x16

: �⥯���2 ( 2**N -- 2**N 2 ) 
DUP 0 SWAP BEGIN 2/ SWAP 1+ SWAP DUP 1 = UNTIL DROP
;
: �८�ࠧ������_��_�⮫�栬 ( id_real id_imm -- )
   �������2 �������1
    ᬥ饭�� DUP & real{ set_cell  & imm{ set_cell \ ��⠭����� cell ���ᨢ��
    & real{ ��ப1 }malloc   & imm{ ��ப1 }malloc \ �뤥���� �����
  �⮫�殢1 0 ?DO
    I ��砫�����1 real{ �⮫���_�_����� \ ᪮��஢��� �����. �
    I ��砫�����2  imm{ �⮫���_�_����� \ ������ ��� ᨣ����
    real{ imm{ ��ப1 �⥯���2 direction }}���
      direction 1 = IF 
        real{ ��ப1 }FFT-Normalize
         imm{ ��ப1 }FFT-Normalize
      THEN
    real{ I ��砫�����1 �����_�_�⮫��� \ ����ᠫ� �����. �
     imm{ I ��砫�����2 �����_�_�⮫��� \ ������ ��� ᯥ���
  LOOP
  & real{ }free & imm{ }free \ �᢮������ ������
;
: �८�ࠧ������_��_��ப�� ( id_real id_imm -- ) 
   �������2 �������1
    ᬥ饭�� DUP & real{ set_cell  & imm{ set_cell \ ��⠭����� cell ���ᨢ��
    & real{ ��ப1 }malloc   & imm{ ��ப1 }malloc \ �뤥���� �����
  ��ப1 0 ?DO
    I ��砫�����1 real{ ��ப�_�_����� \ ᪮��஢��� �����. �
    I ��砫�����2  imm{ ��ப�_�_����� \ ������ ��� ᨣ����
    real{ imm{ �⮫�殢1 �⥯���2 direction }}���
      direction 1 = IF 
        real{ �⮫�殢1 }FFT-Normalize
         imm{ �⮫�殢1 }FFT-Normalize
      THEN
    real{ I ��砫�����1 �����_�_��ப� \ ����ᠫ� �����. �
     imm{ I ��砫�����2 �����_�_��ப� \ ������ ��� ᯥ���
  LOOP
  & real{ }free & imm{ }free \ �᢮������ ������
;
: 2-�_��� ( id_real id_imm direction -- )
\ ��㬥୮� �८�ࠧ������ ���� - ��� ����� ࠧ��� 2**N X 2**M
  TO direction
  2DUP �८�ࠧ������_��_�⮫�栬
  �८�ࠧ������_��_��ப��
;
: �����-����� ( id -- id_real id_imm )     \ id-�����. �����
\ ���� ��������� ��� ����⢨⥫쭮� ������
  DUP ������ >R ���� R@ ���樠����஢��� \ -- id  r: id_imm
  DUP ������ 2DUP ����஢��쌠�� NIP     \ -- id_real  r: id_imm
  R>                                        \ -- id_real id_imm
;
: ������㤠,䠧� ( id_real id_imm -- ) \
\ �८�ࠧ�� ᯥ��� ���� [�����.] [����.] � [����.] [����.]
�������2 �������1
  ��ப1 0 ?DO
    �⮫�殢1 0 ?DO
    J I ��砫�����2 ����읫����� \ f: -- y x
    J I ��砫�����1 ����읫����� \
    F2DUP FDUP F* FSWAP FDUP F* F+ FSQRT \  f: -- y x r
    J I ��砫�����1 ���읫�����  \ ������㤠
    F/ FATAN
    J I ��砫�����2 ���읫�����  \ 䠧� ( � ࠤ�����)
    LOOP
  LOOP
;

Reset_Search_Order

HERE SWAP -
  CR .( 2D DSP            V1.00          10 November 2002   --  ) . .( bytes)
  CR
\ EOF ���஢����

䫮���

  0 VALUE M1
(
4 4 ���������� TO M1
M1 ��ନ஢���
1.e m, 0.e m, 1.e m, 0.e m,
0.e m, 1.e m, 0.e m, 1.e m,
1.e m, 0.e m, 1.e m, 0.e m,
0.e m, 1.e m, 0.e m, 1.e m,
��������
 )

 WINAPI: GetTickCount KERNEL32.DLL
 16 16 ���������� TO M1
: TEST
\ 16 16 ���������� TO M1
 S" matrix_test" M1 ���樠����஢���_��_�����
M1 
." ___________" CR
DUP �����쌠����
." ___________" CR
�����-�����
2DUP 1 
GetTickCount >R
2-�_���
GetTickCount >R
SWAP 
2DUP
�����쌠���� CR �����쌠����
�᢮������ �᢮������
M1 �᢮������
R> R> - CR . ." - ms processing"
;

STARTLOG
 TEST
ENDLOG \ BYE
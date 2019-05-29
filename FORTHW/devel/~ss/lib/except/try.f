\ ����������� ��������� �������������� �������� ��� � delphi
\ �� ������ ������-����������� by ruvim
\ v0.4 [19.08.2000] - ��������� �����������
\ v0.3 [02.06.2000] - ��������� �� ����� ����� �� �����, ��. try-example.f
\ v0.2 [30.05.2000] - ������ ���������� ����. �� ����������������� ���������
\ v0.1 [14.05.2000] 
\ Sergey Shishmintzev <sergey.shishmintzev/at/gmail.com> 
\
\ ����������:
\ 1. ����� ����������� ����� try-except � ���� �������� ����������� tryDEPTH
\ ������, ���������� ������� ������� �� ��������� ����� CATCH
\ 2. ����� ���������� ����� try-except ��������� ����� ������ � ����� ��������
\ ����������� ����� ����� ��� ���� �� ������������ ����� try (�� ��� ����������
\ ����� ���������, ������� �� �������� �� ������ � ����� �����) ���� ��� 
\ ���������� � ����� ������


BASE @ DECIMAL
44 CONSTANT tryDEPTH
-44 CONSTANT -tryDEPTH

-1 CONSTANT EAbort
-2 CONSTANT EAbort"

' DROP VALUE <os-exc-handler>
\ ' EXC-DUMP1  VALUE <os-exc-handler>

: try-prolog ( -- )
  \ ������ ����� ����� ��� ���������� ��
  \ (������ �������� ��� ����� ������������)
  ['] <EXC-DUMP> >BODY @ R@ 5 + !
  <os-exc-handler> TO <EXC-DUMP> 
;
: try-epilog ( e ^xt -- e )
  \ ������������ ������ ���������� ���������� ��
  @ TO <EXC-DUMP>
; 


: try  ( -- )
\ ����� ����������  ( -- orig1 xt t-addr t-s )
   ?COMP
   POSTPONE try-prolog
   HERE BRANCH, HERE >MARK
   CELL ALLOT \ t-addr = HERE 
   14444444 
; IMMEDIATE
          
: except  ( -- n_exception )
\ ����� ���������� ( orig1 xt t-addr t-s -- e-s )
\ t-s     �����: ������� ��� try?
\ t-addr  ����� ������ ��� �������� ���� ������
\ xt      ���������� �����
\ orig1 
\ e-s     �����: ��� ��� except
   ?COMP 14444444 <> IF -2007 THROW THEN
   RET,
   >RESOLVE1 >R R@ CELL+ LIT, POSTPONE CATCH
   R@ LIT, POSTPONE try-epilog
   POSTPONE ?DUP POSTPONE IF 
   POSTPONE DUP R@ LIT, POSTPONE !
   R>
   14444440
; IMMEDIATE

: finally
\ ����� ���������� (  orig1 xt t-addr t-s -- t-addr f-s )
   ?COMP 14444444 <> IF -2007 THROW THEN
   RET,
   >RESOLVE1 DUP CELL+ LIT, POSTPONE CATCH
   DUP LIT, POSTPONE try-epilog
   DUP LIT, POSTPONE !
   14444441
; IMMEDIATE

: raise  ( -- )
  DUP 14444440 = IF
    12 SP@ + @ LIT, POSTPONE @ POSTPONE THROW 
  ELSE
    ABORT" raise: In except/end-try block only"
  THEN
; IMMEDIATE

: end-try ( t-addr e-s|f-s -- )
  ?COMP
  DUP 14444441 = IF DROP LIT, POSTPONE @ POSTPONE THROW EXIT THEN
  14444440 = IF  DROP POSTPONE THEN EXIT THEN
  -2007 THROW
; IMMEDIATE                               		

\ �������� �����-���� ��������� ���������� � ���������� ����������
: stop-except ( t-s -- ) POSTPONE finally DROP  DROP ; IMMEDIATE


BASE !
\ : tt try except end-try ;

\ S" tt.exe" SAVE BYE
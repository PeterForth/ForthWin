\ ����� ������� �������, ���������������� �� c-��������

S" lib/ext/spf-asm-tmp.f" INCLUDED

CODE (CAPI-CALL) ( EAX - extern-addr, ECX - n -- x )
      OR   ECX, ECX
      JZ   SHORT @@1
      LEA  EBX, [ECX*4]
      SUB  ESP, EBX
      MOV  EDX, EDI
      MOV  EDI, ESP
      MOV  ESI, EBP
      CLD
      REP MOVS DWORD
      MOV EDI, EDX
      ADD EBP, EBX
      CALL EAX
      ADD  ESP, EBX
      RET

@@1:  CALL EAX
      RET
END-CODE

CODE CAPI-CALL ( ... n extern-addr -- x )
\ ����� ������� �������, ���������������� �� c-��������
      MOV  ECX, [EBP]
      LEA  EBP, 4 [EBP]
      CALL ' (CAPI-CALL)
      RET
END-CODE

CODE _CAPI-CODE
      POP  EBX
      MOV  -4 [EBP], EAX
      MOV  EAX, [EBX]
      OR   EAX, EAX
      LEA  EBP, -4 [EBP]
      JNZ  SHORT @@1
      MOV  EAX, EBX
      CALL ' AO_INI
      JZ  SHORT @@2
      MOV [EBX], EAX
@@1:  MOV  ECX, 12 [EBX]
      JMP ' (CAPI-CALL)
@@2:  RET
END-CODE

CODE _CVAPI-CODE
      POP  EBX
      MOV  -4 [EBP], EAX
      MOV  EAX, [EBX]
      OR   EAX, EAX
      LEA  EBP, -4 [EBP]
      JNZ  SHORT @@1
      MOV  EAX, EBX
      CALL ' AO_INI
      JZ  SHORT @@2
      MOV [EBX], EAX
@@1:  JMP ' CAPI-CALL
@@2:  RET
END-CODE

: CAPI: ( "������������" "�������������" n -- )
  ( ������������ ��� ������� c-�������.
    ���������� ����������� ����� ����� ��� "������������".
    ���� address of winproc ����� ��������� � ������ �������
    ���������� ���������� ��������� ������.
    ��� ������ ���������� "���������" ��������� ���������
    ���������� �� ���� ������ � �������, �������� ����������
    � ��-������ ���� ���������. ��������� ���������� �������
    ����� ������� �� ����.
    2 CAPI: strstr msvcrt.dll

    Z" s" Z" asdf" strstr
  )
  >IN @  HEADER  >IN !
  ['] _CAPI-CODE COMPILE,
  __WIN:
;

: CVAPI: ( "������������" "�������������" -- )
\ ��� ������� � ���������� ������ ����������
\ ��� ������ ����� ���������� ���� ������� �� �����
\ CVAPI: sprintf msvcrt.dll

\ 50 ALLOCATE THROW VALUE buf
\ 10 Z" %d" buf 3 sprintf
  >IN @  HEADER  >IN !
  0 ['] _CVAPI-CODE COMPILE,
  __WIN:
;

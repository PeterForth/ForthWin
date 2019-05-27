\ $Id: spf_win_api.f,v 1.10 2008/03/23 08:38:47 ygreks Exp $

( ��������� ������� ���������� ��� WINAPI � WNDPROC
  Windows-��������� �����.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999
)

VARIABLE AOLL
VARIABLE AOGPA
0 VALUE ST-RES

\ ����������� ������������ �-��/����
VECT PROC-ERROR
VECT LIB-ERROR

CODE AO_INI \ � EAX ��������� WINAPI:
      MOV  EBX, EAX
      MOV  EAX, 4 [EBX]
      PUSH EAX
      A; 0xA1 C,  AddrOfLoadLibrary
      ALSO FORTH , PREVIOUS \   MOV  EAX, AddrOfLoadLibrary
A; HERE 4 - ' AOLL EXECUTE !
      CALL EAX
      OR   EAX, EAX
      JZ   @@1

      MOV  ECX, 8 [EBX]
      PUSH ECX
      PUSH EAX
      A; 0xA1 C,  AddrOfGetProcAddress
      ALSO FORTH , PREVIOUS \    MOV  EAX, AddrOfGetProcAddress
A; HERE 4 - ' AOGPA EXECUTE !
      CALL EAX
      OR   EAX, EAX
      JZ   @@2
      RET
      
@@2:  MOV   EAX, EBX \ ����� ��� ��� ���� ��� �� EAX
      JMP ' PROC-ERROR \ can't find a proc
@@1:  MOV   EAX, EBX      
      JMP ' LIB-ERROR \ can't find a library
END-CODE

CODE API-CALL ( ... extern-addr -- x )
\ ����� ������� ������� (API ��� ������ ������� ����� COM)

      PUSH EDI
      PUSH EBP
      SUB  ESP, # 60
      MOV  EBX, EDI
      MOV  EDI, ESP
      MOV  ESI, EBP
      MOV  ECX, # 15
      CLD
      REP MOVS DWORD
      MOV  EBP, ESP
      MOV  EDI, EBX
      CALL EAX
      MOV  EBX, EBP
      SUB  EBX, ESP
      MOV  ESP, EBP
      ADD  ESP, # 60
      POP EBP
      SUB EBP, EBX
      POP EDI
      RET
END-CODE

CODE _WINAPI-CODE
      POP  EBX
      MOV  -4 [EBP], EAX
      MOV  EAX, [EBX]
      OR   EAX, EAX
      LEA  EBP, -4 [EBP]
      JNZ  SHORT @@1
      MOV  EAX, EBX
      CALL ' AO_INI
      JZ  SHORT @@2 \ ����-�� �� �����
      MOV [EBX], EAX
@@1:  JMP ' API-CALL \ call ret
@@2:  RET
END-CODE

' _WINAPI-CODE TO WINAPI-CODE

CODE _WNDPROC-CODE
     MOV  EAX, ESP
     SUB  ESP, # 3968
A;   HERE 4 - ' ST-RES 9 + EXECUTE
     PUSH EBP
     MOV  EBP, 4 [EAX] ( ����� �������� �� CALLBACK )
     PUSH EBP
     MOV  EBP, EAX
     ADD  EBP, # 12
     PUSH EBX
     PUSH ECX
     PUSH EDX
     PUSH ESI
     PUSH EDI
     MOV  EAX, [EAX] ( ����� ������ ����-��������� )
     MOV  EBX, [EAX]
     MOV  EAX, -4 [EBP]
     CALL EBX
     LEA  EBP, -4 [EBP]
     MOV  [EBP], EAX
     POP  EDI
     POP  ESI
     POP  EDX
     POP  ECX
     POP  EBX
     MOV  EAX, ESP
     MOV  ESP, EBP
     MOV  EBP, 4 [EAX] \ ����������� EBP
     MOV  EAX, [EAX]   \ ����� �������� �� CALLBACK
     XCHG EAX, [ESP]
     RET
END-CODE


' _WNDPROC-CODE TO WNDPROC-CODE

VECT FORTH-INSTANCE>  \ ��� ��������� ����� ����������� �� �����
VECT <FORTH-INSTANCE  \ � ������ � WNDPROC-��������� (������������� TlsIndex)

' FORTH-INSTANCE> TO TC-FORTH-INSTANCE>
' <FORTH-INSTANCE TO TC-<FORTH-INSTANCE

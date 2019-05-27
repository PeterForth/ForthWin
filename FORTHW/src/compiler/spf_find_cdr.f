( ����� ���� � �������� [�������� �� spf_find.f]
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org

  �������������� by day, 29.10.2000
  �������������� by mak July 26th, 2001 - 15:45
  ��� ���������� �� SEARCH-WORDLIST, by ~ygrek Nov.2006
  ��������� ��� "Access Violation" � ����������� by ~ruv, Sep.2008 

  $Id: spf_find_cdr.f,v 1.1 2008/10/04 22:55:56 ruv Exp $
)


CODE CDR-BY-NAME0 ( c-addr u nfa1|0 -- c-addr u nfa1|nfa2|0 )
    MOV EBX, [EBP] \ counter (in most cases it is 0)
    MOV EDX, # 0
    JMP SHORT @@1
@@2:
    MOV EAX, 1 [EDX] [EAX]
@@1: OR EAX, EAX
    JZ SHORT @@9   \ ����� ������
    MOV DL, BYTE [EAX]
    CMP EDX, EBX
    JNZ SHORT @@2
@@9:
    RET
END-CODE


CODE CDR-BY-NAME1 ( c-addr u nfa1|0 -- c-addr u nfa1|nfa2|0 )
    MOV ECX, 4 [EBP] \ c-addr
    MOV BL, [EBP]    \ counter
    MOV BH, [ECX]    \ first char
    JMP SHORT @@1
@@2:
    AND EDX, # 0xFF
    MOV EAX, 1 [EDX] [EAX]
@@1: OR EAX, EAX
    JZ SHORT @@9  \ ����� ������
    MOV DX, [EAX]
    CMP DX, BX
    JNZ SHORT @@2 \ ���� ����� - �����
@@9:
    RET
END-CODE


CODE CDR-BY-NAME2 ( c-addr u nfa1|0 -- c-addr u nfa1|nfa2|0 )
    MOV ECX, 4 [EBP] \ c-addr
    MOV EBX, [EBP]   \ counter (in most cases -- 2)
    MOV EDX, EBX     \ copy of the counter
    MOV BX,  [ECX]   \ first and second chars
    SHL EBX, # 0x08
    MOV BL, DL       \ counter
    JMP SHORT @@1
@@2:
    AND EDX, # 0xFF
    MOV EAX, 1 [EDX] [EAX]
@@1: OR EAX, EAX
    JZ SHORT @@9   \ ����� ������
    MOV EDX, [EAX] \ ����������, ��� ��� �� ������� ������ �� ��������.
    AND EDX, # 0x00FFFFFF
    CMP EDX, EBX
    JNZ SHORT @@2 \ ���� ����� - �����
@@9:
    RET
END-CODE


CODE CDR-BY-NAME3 ( c-addr u nfa1|0 -- c-addr u nfa1|nfa2|0 )
    MOV ECX, 4 [EBP] \ c-addr
    MOV BX, 1 [ECX]  \ second and third
    SHL EBX, # 0x10  \ 8+8
    MOV BH, [ECX]    \ first char
    MOV BL, [EBP]    \ counter
    JMP SHORT @@1
@@2:
    AND EDX, # 0xFF
    MOV EAX, 1 [EDX] [EAX]
@@1:
    OR EAX, EAX  
    JZ SHORT @@9    \ ����� ������
    MOV EDX, [EAX]
    CMP EDX, EBX
    JNZ SHORT @@2   \ ���� ����� - �����
@@9:
    RET
END-CODE


CODE CDR-BY-NAME ( c-addr u nfa1|0 -- c-addr u nfa1|nfa2|0 )
\ ����, ��� � CDR (��. � spf_wordlist.f), �� ����� ����� ������ �������� �������� � �������� ���.

    MOV EDX, [EBP]                \ ����� (�������)
    CMP EDX, # 3
    JG   @@1 \  u > 3
    JE   ' CDR-BY-NAME3 \ u = 3
    CMP EDX, # 2
    JE   ' CDR-BY-NAME2 \ u = 2
    CMP EDX, # 1
    JE   ' CDR-BY-NAME1 \ u = 1 
    JMP  ' CDR-BY-NAME0 \ u = 0
@@1: \ u > 3
    CALL ' CDR-BY-NAME3
    OR EAX, EAX
    JNZ SHORT @@5
    RET \ ����� ������
    \ JZ SHORT @@9
@@5:
    PUSH EDI
    MOV ESI, ECX  \ addr � ������� (see CDR-BY-NAME3)
    ADD ESI, # 3
    MOV ECX, # 0
    JMP SHORT @@3
@@2:
    AND EDX, # 0xFF
    MOV EAX, 1 [EDX] [EAX]
    OR EAX, EAX  
    JZ SHORT @@8    \ ����� ������
    MOV EDX, [EAX]
    CMP EDX, EBX
    JNZ SHORT @@2   \ ���� �� ����� - ���� �� ������ ������
@@3: \ ��������� ������� ����� ��������
    MOV EDI, EAX  \ � ������
    ADD EDI, # 4
    MOV CL, BL    \ counter (see CDR-BY-NAME3)
    SUB CL, # 3   \ 3 chars in the code
    PUSH ESI
    REPZ CMPS BYTE
    POP ESI
    JNZ SHORT @@2 \ ������ �� ����� -- ���� �� ������ ������
@@8:
    POP EDI
@@9:
    RET
END-CODE

\ (c) ~micro 2001

REQUIRE ON lib\ext\onoff.f

MODULE: TRACER
  H-STDOUT VALUE File
  10 VALUE MaxDEPTH
  CHAR | VALUE IndentChar
  2 VALUE IndentSize
  VARIABLE Compile  Compile ON
  VARIABLE Flush    Flush   OFF

  : File:
    NextWord DUP >R
    HEAP-COPY DUP R> R/W CREATE-FILE-SHARED THROW TO File FREE THROW
  ;

  MODULE: Private
    VARIABLE Indent
    Indent 0!
    : .S
      H-STDOUT >R File TO H-STDOUT
      DEPTH S>D <# [CHAR] ] HOLD #S [CHAR] [ HOLD #> TYPE SPACE DEPTH MaxDEPTH MIN .SN
      Flush @ IF File FLUSH-FILE THROW THEN
      R> TO H-STDOUT
    ;

    : .Indent
      Indent @ 0 ?DO
        IndentChar EMIT
        IndentSize 1- SPACES
      LOOP
    ;

    : In ( addr u caddr2 -- )
      H-STDOUT >R File TO H-STDOUT
      .Indent ." > " VOC-NAME. ."  " TYPE ."  "
      .S
      CR
      1 Indent +!
      Flush @ IF File FLUSH-FILE THROW THEN
      R> TO H-STDOUT
    ;
    
    : Out ( addr u caddr2 -- )
      H-STDOUT >R File TO H-STDOUT
      -1 Indent +!
      .Indent ." < " VOC-NAME. ."  " TYPE ."  "
      .S
      CR
      Flush @ IF File FLUSH-FILE THROW THEN
      R> TO H-STDOUT
    ;

    VECT vIn
    VECT vOut

    : _: : ;
    : _; POSTPONE ; ; IMMEDIATE

    : 3DROP 2DROP DROP ;

  ;MODULE

{{ Private  
  : TraceON    ['] In    TO vIn   ['] Out   TO vOut   ;
  : TraceOFF   ['] 3DROP TO vIn   ['] 3DROP TO vOut   ;

  : StartTrace
     S" trace.log" R/W CREATE-FILE-SHARED THROW TO File
     TraceON
  ;

}}
  EXPORT
{{ Private
    : DOES>
      Compile @ IF
        LATEST POSTPONE LITERAL POSTPONE COUNT CURRENT @ POSTPONE LITERAL
        POSTPONE vOut
      THEN
      POSTPONE DOES>
      Compile @ IF
        POSTPONE DUP POSTPONE WordByAddr CURRENT @ POSTPONE LITERAL
        POSTPONE vIn
      THEN
    ; IMMEDIATE

    _: :
      _:
      Compile @ IF
        LATEST POSTPONE LITERAL POSTPONE COUNT CURRENT @ POSTPONE LITERAL
        POSTPONE vIn
      THEN
    ;
    
    _: ;
      Compile @ IF
        LATEST POSTPONE LITERAL POSTPONE COUNT CURRENT @ POSTPONE LITERAL
        POSTPONE vOut
      THEN
      POSTPONE ;
    ; IMMEDIATE
}}
  DEFINITIONS
{{ Private
  _: T:
    >IN @ >R
    NextWord SFIND DUP 0= ABORT" not found"
    R> >IN !
    SWAP
    : COMPILE, POSTPONE ;
    1 = IF
      IMMEDIATE
    THEN
  _;
}}
;MODULE

ALSO TRACER
TraceOFF

\EOF    �ਬ�� �� ������� + T:

{{ TRACER
T: *
}}

: qwe { a \ b -- c }
  a 2 * TO b a b *
;

3 qwe

\EOF    �ਬ�� � �������묨 �맮����

: a ;
: b a a ;
: c DUP b DROP a DROP DROP ;
: d 10 0 DO I DUP 1+ DUP 1+ c DROP LOOP ;
2 TRACER::TO MaxDEPTH
d

\EOF    �ਬ�� �� DOES>

: q CREATE , DOES> ." q=" @ . ;
12 q q1
34 q q2
q1 q2

\EOF    ���ᠭ��

����஢騪
�����頥��� � ᫮��� TRACER

NEW! �������� DOES>-᫮��.

��:
----- �ணࠬ�� -----
: parent CREATE , DOES> DROP ;
1 parent child
child
--- �३���� ��� ---
> FORTH parent [1] 1
< FORTH parent [0]
> FORTH child [1] 5588940    (*)
< FORTH parent [0]           (**)
---------------------
(*) ��� ��஦�񭭮�� ᫮�� �뢮����� ⮫쪮 �᫨ � ���� த�⥫�� ��-�
    �뫮 ᪮�����஢���, ���� "<not in image>". ��� ᫮�� ��।������ ᫮��� 
    WordByAddr, ᫥����⥫쭮 ᪮���� ࠡ��� ��������.
(**) �� ��室� �� ��஦�񭭮�� ᫮�� �뢮����� ��� ᫮��-த�⥫�.

NEW! �������� �������. ���祬, �� ��� ���㣠, � ⮣�, �� �ਤ㬠�
     S" ;" EVAL-WORD � ";". �� ������� ���� ��㧨�� �� �३��, �᫨,
     ����筮, �� �⮨� ����� �३��� ᠬ� ������� ;)

�����: (��� ��६����� � VALUEs � ᪮���� - ���祭�� �� 㬮�砭��)

VALUEs:

  File (H-STDOUT) ��� 䠩��, � ����� �㤥� �뢮������ ����
       (��� ��� १���� ������� ;)
  MaxDEPTH (10) ���ᨬ��쭠� �뢮����� ��㡨�� �⥪�
  IndentChar ('|') ᨬ��� ����㯠 �� ��������� �맮���
  IndentSize (2) ����稭� ����㯠

VARIABLEs:

  Compile (ON) �������஢��� trace-info
  Flush (OFF) ��࠭��� 䠩���� ���� �� ������ �����

Colons:

  File: ( "䠩�" -- ) ᮧ���� 䠩�, ��� ��࠭��� � File
  TraceON (�� 㬮�砭��) ����� �뢮� �����
  TraceOFF �४���� �뢮� �����
  T: ("�������饥 ᫮��" -- ) ��८�।���� �������饥 ᫮��, �।��⠢��� 
     ����������� ��᫥������ �室 � ���� � ��室.

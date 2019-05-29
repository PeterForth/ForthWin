\ RipDLL 1.00	7.04.2003
\ ����� �� �।��⠢������ �������᪮� ������⥪� ����� ��� 
\ �ᯮ���㥬�� �㭪権 � ��࠭��� �� � �ଠ� .names ��� 
\ ���쭥�襣� �ਬ������ � USEDLL
\ �. �������, http://www.forth.org.ru/~yz
\ ------------------------------------------
REQUIRE "        ~yz/lib/common.f
REQUIRE )>       ~yz/lib/format.f
REQUIRE MAP-OPEN ~yz/lib/mapfile.f
REQUIRE {        lib/ext/locals.f

\ ------------------------------------------
CREATE input-file  256 ALLOT
CREATE output-file 256 ALLOT

\ ------------------------------------------

: err  ." RIPDLL: " .ASCIIZ CR BYE ;
: ?err ( ? z --) SWAP IF err ELSE DROP THEN ;

: my-error ( ERR-NUM -> ) \ �������� ����஢�� �訡��
  DUP -2 = IF DROP 
                ER-A @ ER-U @ PAD CZMOVE PAD err
           THEN
  >R <( R> DUP " �訡�� ~N (0x~06H)" )> err ;
\ ------------------------------------------

0 VALUE mapin
0 VALUE base
0 VALUE /dosheader

: offset ( n -- n ) base + ;
: file@ ( n -- n) offset @ ;
: filew@ ( n -- w) offset W@ ;
: --> ( n -- ) file@ offset ;

: open-input-file 
  input-file ASCIIZ> MAP-OPEN DUP 0= " �� ���� ������ �室��� 䠩�" ?err
  DUP @ TO base TO mapin
  0 offset W@ 0x5A4D <> " ���� �� ���� �ᯮ��塞�" ?err
  0x3C --> DUP @ 0x4550 <> " ���� �� � �ଠ� Portable Executable" ?err
  0x3C file@ TO /dosheader
  TO base \ � �⮣� ������ ᬥ饭�� � 䠩�� ��稭��� �⬥���� �� ������ ��������� PE
  0x16 file@ 0x2000 AND 0= " ���� �� ���� �������᪮� ������⥪��" ?err
;

\ --------------------------------

0 VALUE out

: open-output-file 
  output-file ASCIIZ> W/O CREATE-FILE 
  " �� ���� ᮧ���� ��室��� 䠩�" ?err TO out ;

: (write) ( a n -- ) out WRITE-FILE " �訡�� �����" ?err ;

: write ( z -- ) ASCIIZ> 1+ (write) ;
: write-cell ( n -- ) HERE ! HERE 4 (write) ;
: write-byte ( c -- ) HERE C! HERE 1 (write) ;

: seek ( d -- ) out REPOSITION-FILE " �� ���� ��६����� 䠩���� 㪠��⥫�" ?err ;

\ ------------------------------------------

: close-all
  out CLOSE-FILE DROP 
  mapin MAP-CLOSE ;

\ ------------------------------------------
\ ��室�� ᥪ��, � ���ன �⭮���� ⠡��� �ᯮ��
\ � ��⠭�������� base ⠪�� ��ࠧ��, �⮡� �� �⬥�� ᬥ饭��
\ �� ��砫� ��ࠧ� �ᯮ��塞��� 䠩��, � ���� ������뢠� 
\ RVA ��襩 ᥪ樨 � ���� �⮡ࠦ������ 䠩��.
: find-export-section ( -- exportRVA)
  0x78 file@ \ RVA ⠡���� �ᯮ��
  \ ��室�� ��砫� ⠡���� ᥪ権: ��� ���� �ࠧ� ��᫥ ��������� 䠩��
  0x74 file@ 2* CELLS 0x78 + 0x6 filew@ ( �᫮ ᥪ権) 
  10 CELLS ( ࠧ��� ����⥫� ᥪ樨) * OVER + SWAP DO
    DUP I 0x0C + file@ ( RVA ᥪ樨)
    DUP I 0x08 + ( ࠧ��� ᥪ樨) file@ + WITHIN IF 
      I 0x14 + ( ᬥ饭�� ᥪ樨 � 䠩��) file@ I 0x0C + file@  - 
      base + /dosheader - TO base
      UNLOOP EXIT
    THEN
  10 CELLS ( ࠧ��� ����⥫� ᥪ樨) +LOOP
  " �室��� 䠩� �ᯮ�祭: ⠡��� �ᯮ�� �� ᮤ�ন��� �� � ����� ᥪ樨" err
;

VARIABLE ptr

: word1 ( n -- z) 10 MOD 1 = IF " ��" ELSE " ��" THEN ;
: word2 ( n -- z)
  DUP 100 MOD 11 20 WITHIN IF 
    DROP " ��"
  ELSE
    10 MOD CASE
    1 OF " �" ENDOF
    DUP 2 5 WITHIN =OF " ���" ENDOF
    DROP " ��"
    END-CASE
  THEN ;

: rip-names { \ export name# libname stable names -- }
  find-export-section TO export
  export 0x18 + file@ TO name#
  export 0x0C + --> TO libname
  export 0x20 + file@ TO names
  <( libname name# DUP word1 OVER word2 " ~Z: ~N �ᯮ���㥬~Z ��~Z~/" )> .ASCIIZ
  name# CELLS GETMEM TO stable
  out FILE-POSITION " �� ���� ��।����� ��������� 䠩������ 㪠��⥫�" ?err
  3 name# + CELLS S>D D+ seek
  3 name# + CELLS ptr !
  name# 0 ?DO
    ptr @ I CELLS stable + !
    -1 write-cell
    ptr CELL+!
    I CELLS names + --> ASCIIZ> DUP 2+ ptr +!
    DUP write-byte (write)
    0 write-byte
  LOOP
  0 write-cell
  libname write
  0. seek
  CELL" NAME" write-cell
  ptr @ write-cell name# write-cell
  stable name# CELLS (write)
  stable FREEMEM
;

\ ------------------------------------------
: ?next ( "name" ��� name<BL> -- a # / 0)
  PeekChar c: " = IF c: " ELSE BL THEN WORD
  DUP C@ 0= IF DROP 0 EXIT THEN
  COUNT OVER C@ c: " = IF 2 - SWAP 1+ SWAP THEN ( �ࠫ ����窨, �᫨ ����)
;

: -ext { a n -- a #1 }
  a n + 1-
  BEGIN DUP a < NOT WHILE
    DUP C@ c: . = IF a - a SWAP EXIT THEN
    1-
  REPEAT DROP a n ;

: +ext ( a # -- a1 #1)
  -ext
  DUP >R PAD SWAP CMOVE R> PAD + 
  S" .names" ROT 2DUP + 1+ >R CZMOVE
  PAD R> OVER - ;

: RUN
  ['] my-error TO ERROR
  -1 TO SOURCE-ID 
  GetCommandLineA ASCIIZ> SOURCE!
  ?next 2DROP  \ �ࠫ� ��� 䠩��
  ?next
  ?DUP 0= IF
    ." RIPDLL 1.00  ��������� �� �������᪮� ������⥪� ����� �㭪権." CR
    ." �맮�: RIPDLL ��䠩� [���䠩�]" CR
    BYE
  THEN
  ( a #) input-file CZMOVE
  ?next 2DUP TYPE ?DUP 0= IF input-file ASCIIZ> +ext THEN output-file CZMOVE 
  open-input-file
  open-output-file
  rip-names
  close-all
  BYE ;

 0 TO SPF-INIT?
 ' RUN MAINX !
 S" ripdll.exe" SAVE  
BYE

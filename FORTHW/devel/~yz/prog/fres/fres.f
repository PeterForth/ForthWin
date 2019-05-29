\ FRES 1.02 09.03.2007
\ �८�ࠧ�� �⠭����� 䠩� .RES � 㤮��� ��� ����㧪� �ଠ�,
\ ����� �।�⠢��� ᮡ�� ���� ����, ��� ᮮ⢥�����騩 ��᮪
\ ᥣ���� ����ᮢ: �� �஢�� ��ॢ� ���� ᠬ� ������.
\ �����⢥���� ࠧ���: ��뫪� �� ������ �⬥������ �� ��砫� 䠩��.
\ �. �������, http://www.forth.org.ru/~yz
\ ------------------------------------------
\ ���������.
\ ��᪮��� �㭪樨 ࠡ��� � �⮬��� (� W95/98) �� �������� �����᪨� ��ப,
\ ��諮�� �८�ࠧ��뢠�� ⠪��� � ����� ��ப�, � ��⮬ �����.
\ ���⢥��⢥���, ����� ����ᮢ � �� ⨯�� ������ ��࠭�稢����� ᨬ������ ASCII.
\ ------------------------------------------
\ ���ᨨ
\ 1.01 (15.12.2001) ��ࠢ���� �訡��: ������ � ������� �������஢����� ��� NT ���ࠢ��쭮
\ 1.02 (09.03.2007) WINAPI: ����� ~yz/lib/api.f
\ ------------------------------------------
REQUIRE "       ~yz/lib/common.f
REQUIRE (|      ~yz/lib/printf.f
REQUIRE {       lib/ext/locals.f
\ ------------------------------------------
CREATE input-file  256 ALLOT
CREATE output-file 256 ALLOT
\ ------------------------------------------

: err  ." FRES: " .ASCIIZ CR BYE ;
: ?err ( ? z --) SWAP IF err ELSE DROP THEN ;

: my-error ( ERR-NUM -> ) \ �������� ����஢�� �訡��
  DUP -2 = IF DROP 
                ER-A @ ER-U @ PAD CZMOVE PAD err
           THEN
  DUP 2 <| " �訡�� #%d (0x%06X)" |) err
;
\ ------------------------------------------
0xF0000000 == numflag
: name? ( n -- ?) numflag AND 0= ;
: setnumflag ( n -- n1) numflag OR ;
: clnumflag ( n -- n1) numflag INVERT AND ;
\ ------------------------------------------
WINAPI: CreateFileMappingA KERNEL32.DLL
WINAPI: MapViewOfFile KERNEL32.DLL
WINAPI: UnmapViewOfFile KERNEL32.DLL

0 VALUE fh
0 VALUE maph
0 VALUE file
0 VALUE input-file-size

0 VALUE ofh
0 VALUE omaph
0 VALUE ofile

VARIABLE file-ptr

: map-input-file
  input-file DUP ZLEN R/O OPEN-FILE " �室��� 䠩� �� ������" ?err TO fh
  0 0 0 2 ( page_readonly) 0 fh CreateFileMappingA ?DUP 0= 
    " �� ���� �⮡ࠧ��� 䠩� � �����" ?err TO maph
  0 0 0 4 ( file_map_read) maph MapViewOfFile ?DUP 0= 
    " �� ���� �⮡ࠧ��� 䠩� � �����" ?err TO file ;

: unmap-input-file
  file UnmapViewOfFile DROP
  maph CloseHandle DROP
  fh CLOSE-FILE DROP ;

: map-output-file
  output-file DUP ZLEN R/W CREATE-FILE " �� ���� ᮧ���� ��室��� 䠩�" ?err TO ofh
  0 input-file-size 2* 0 4 ( page_readwrite) 0 ofh CreateFileMappingA ?DUP 0= 
    " �� ���� �⮡ࠧ��� ��室��� 䠩� � �����" ?err TO omaph
  0 0 0 2 ( file_map_write) omaph MapViewOfFile ?DUP 0= 
    " �� ���� �⮡ࠧ��� ��室��� 䠩� � �����" ?err TO ofile 
  ofile file-ptr ! ;

: unmap-output-file
  ofile UnmapViewOfFile DROP
  omaph CloseHandle DROP
  file-ptr @ ofile - S>D ofh RESIZE-FILE DROP
  ofh CLOSE-FILE DROP ;
\ ------------------------------------------
WINAPI: GetAtomNameA KERNEL32.DLL

: >> ( n -- ) file-ptr @ !  CELL file-ptr +! ;
: W>> ( w -- ) file-ptr @ W! 2 file-ptr +! ;
: block>> ( a n -- ) >R file-ptr @ R@ CMOVE  R> file-ptr +! ;
: atom>> ( atom -- ) >R 300 HERE R> GetAtomNameA >R
  600 file-ptr @ 2+ R> HERE 0 0 MultiByteToWideChar DUP file-ptr @ W!
  2* 2+ file-ptr +! ;
: ?align-file ( -- )
  \ ����㥬�� ⥬, �� ��, �����뢠���� � 䠩�, 㦥 ��ࠢ���� �� �࠭��� ᫮��
  file-ptr @ ofile - CELL MOD IF 0 W>> THEN ;
\ ------------------------------------------
\ ��७� ��ॢ�
0
CELL -- :begin    \ ������ ���� = :next = 0
CELL -- :treeref
CELL -- :names
CELL -- :ords
== root#
\ 㧥� ��ॢ�
0 
CELL -- :next
CELL -- :strref
CELL -- :nextname
CELL -- :id
CELL -- :son
== node#
\ ���� ��ॢ�
0
CELL -- :adr
CELL -- :leafref \ ������ ���� = :treeref
CELL -- :dataref
CELL -- :nextleaf
CELL -- :size
== leaf#

0 VALUE type-tree

: new ( size -- a)
  DUP >R GETMEM DUP R> ERASE ;
: new-tree ( -- a) root# new ;
: new-node ( -- a) node# new ;
: new-leaf ( -- a) leaf# new ;

WINAPI: AddAtomA KERNEL32.DLL

: get-son { tree id newproc \ -- son }
  tree :begin @ ?DUP IF
    BEGIN
      DUP :id @ id = IF ( ��諨) :son @ EXIT THEN
      :next DUP @ ?DUP IF PRESS 0 ELSE -1 THEN
    UNTIL \ �� ��諨: ᮧ���� ���� 㧥� � ����� ��ॢ� � ����
  ELSE
    tree :begin
  THEN
  new-node >R
  tree id name? IF :names ELSE :ords THEN 1+!
  R@ SWAP :next !
  id R@ :id !
  newproc EXECUTE DUP R> :son !
;

: get-tree ( tree id -- tree1 )
  ['] new-tree get-son ;

: get-leaf ( tree id -- leaf )
  ['] new-leaf get-son
;

: >id ( adr -- id)
  DUP W@ 0xFFFF = IF 
    2+ W@ setnumflag \ �⮡� ����� �뫮 �⫨��� �� ��ப
  ELSE
    unicode>buf DUP AddAtomA SWAP FREEMEM
  THEN ;

: add-to-tree
  { type name langid dataadr datasize \ -- }
  type-tree type >id get-tree
  name >id get-tree langid setnumflag get-leaf >R
  dataadr  R@ :adr  !
  datasize R> :size !
;  
\ ------------------------------------------
\ ����� �������� ���஢��. ���᪨ ���⪨�, 墠�� � ⠪��
: sort { tree lt \ start min minnode -- }
  tree :begin @ TO start
  BEGIN start :next @ WHILE
    start TO minnode
    start :id @ TO min
    start
    BEGIN
      ( node)
      DUP :id @ min lt EXECUTE IF
        DUP TO minnode
        DUP :id @ TO min
      THEN
      :next @
    ?DUP 0= UNTIL
    start minnode <> IF \ ���塞 ���⠬� �����
      \ �� ������᭮, ��᪮��� �⭮�⥫��� ��뫮� ⠬ ���
      start CELL+ HERE node# CELL - DUP >R CMOVE
      minnode CELL+ start CELL+ R@ CMOVE
      HERE minnode CELL+ R> CMOVE
    THEN
    start :next @ TO start
  REPEAT
;
\ ����� ��ॢ�, �믮���� � ������ 㧫� ������ xt
\ xt ( node -- )
: traverse-tree ( tree xt --)
  >R
  :begin @ ?DUP IF \ ��ॢ� �����⮥?
    BEGIN
      DUP R@ EXECUTE
      :next @ ?DUP
    0= UNTIL
  THEN
  RDROP
;
\ �믮����� ��� ��ॢ� ������ xt1, � ��⮬ ����� ��ॢ� � ����樥� xt
\ xt ( node -- )
\ xt1 ( tree -- )
: do-it-and-traverse-tree ( tree xt xt1 --)
  >R OVER R> EXECUTE traverse-tree ;

\ ����� ��ॢ� � ����樥� xt, ��⮬ �믮����� ��� ��� ������ xt1
\ xt ( node -- )
\ xt1 ( tree -- )
\ : traverse-tree-and-do-it ( tree xt xt1 --)
\  >R >R DUP R> traverse-tree R> EXECUTE ;  

WINAPI: lstrcmp KERNEL32.DLL

: atom>str ( atom adr -- )
  SWAP 300 ROT ROT GetAtomNameA DROP ;

: ord/uni< ( o/u1 o/u2 -- ?)
  2DUP OR name? IF
    \ �� ��� �⮬�, ����ࠦ��騥 ��ப�
    HERE atom>str HERE 300 + atom>str
    HERE HERE 300 + lstrcmp 0<
  ELSE
    \ 墠�� � ���⮣� �ࠢ�����. �᫨ �� ��� �᫠, � U< �� 㯮�冷稢���.
    \ � �᫨ �᫮ � �⮬, � �⮬� ��࠭�஢���� ����� �ᥫ � ��⠭�������
    \ ���訬 ��⮬, �� � �ॡ����
    U<
  THEN ;

: (.tree2) ( tree -- )
  ." Tree " DUP .H ." names=" DUP :names @ . ." ords=" :ords @ . CR ;
: (.tree1) ( node -- )
  ." node: id=" DUP :id @ .H ." son=" :son @ .H CR ;
: .tree ( tree -- )
  ['] (.tree1) ['] (.tree2) do-it-and-traverse-tree ;

: ord/uni-sort ( tree -- ) ['] ord/uni< sort ;

: sort-lang-tree ( node -- )
  :son @ ['] < sort ;
: sort-name-tree ( node -- )
  :son @ ['] sort-lang-tree ['] ord/uni-sort do-it-and-traverse-tree ;
: sort-all-trees
  type-tree ['] sort-name-tree ['] ord/uni-sort do-it-and-traverse-tree 
;
\ ------------------------------------------
\ ����� ��ॢ�, �믮����� � ������ 㧫� xt ( node -- ) � 㭨�⮦��� ��� 㧥�
\ ��⮬ 㭨�⮦��� ��ॢ�
: last-traverse-tree ( tree xt -- )
  OVER >R
  >R
  :begin @ ?DUP IF \ ��ॢ� �����⮥?
    BEGIN
      DUP R@ EXECUTE
      DUP >R
      :next @ ?DUP 0=
      R> FREEMEM 
    UNTIL
  THEN
  RDROP  R> FREEMEM ;

: chop-lang-son ( node -- )
  :son @ FREEMEM ;
: chop-name-son ( node -- )
  :son @ ['] chop-lang-son last-traverse-tree ;
: chop-type-son ( node -- )
  :son @ ['] chop-name-son last-traverse-tree ;
: chop-tree 
  type-tree ['] chop-type-son last-traverse-tree ;
\ ------------------------------------------
: ?offset>> ( to -- ) \ ������� ⥪�饥 ᬥ饭�� � 䠩�� �� ����� to<>0
  ?DUP IF file-ptr @ ofile - 0x80000000 OR SWAP ! THEN ;

: ?offset0>> ( to -- ) \ ������� ⥪�饥 ᬥ饭�� � 䠩�� �� ����� to<>0
  ?DUP IF file-ptr @ ofile - SWAP ! THEN ;

VARIABLE name-begin
VARIABLE last-name

VARIABLE leaf-begin
VARIABLE last-leaf

\ struct _IMAGE_RESOURCE_DIRECTORY_ENTRY {
\ � ULONG�� Name;
\ � ULONG�� OffsetToData; }
: write-entries ( node -- )
  >R
  R@ :id @ name? IF
    \ �� ��ப�. ����襬 �� �����
    file-ptr @ R@ :strref !
    R@ last-name @ !
    R@ :nextname last-name !
    CELL" NAME" >>
  ELSE
   \ �� �᫮
    R@ :id @ clnumflag >>
  THEN
  \ ��뫪� �� �����ॢ� ����襬 �����
  file-ptr @ R> :son @ :treeref !
  CELL" DOWN" >>
;  

\ struct _IMAGE_RESOURCE_DIRECTORY {
\    ULONG�� Characteristics;
\    ULONG�� TimeDateStamp;
\ �� USHORT� MajorVersion;
\ �� USHORT� MinorVersion;
\ �� USHORT� NumberOfNamedEntries;
\ �� USHORT� NumberOfIdEntries; }
: write-dir ( tree -- )
  DUP :treeref @ ?offset>>
  0 >> 0 >> 0 >>
  DUP :names @ W>>
  DUP :ords  @ W>>
  ['] write-entries traverse-tree ;

: write-lang-nodes ( lang-nodes -- ) 
  :son @ 
  DUP last-leaf @ !
  :nextleaf last-leaf !
;

: write-name-nodes ( name-node -- )
  :son @ ['] write-lang-nodes ['] write-dir do-it-and-traverse-tree ;

: .restype ( restype --)
  DUP name? IF
    HERE atom>str
    HERE
  ELSE
    clnumflag
  CASE
  1 OF " Cursor      " ENDOF
  2 OF " Bitmap      " ENDOF
  3 OF " Icon        " ENDOF
  4 OF " Menu        " ENDOF
  5 OF " Dialog      " ENDOF
  6 OF " String      " ENDOF
  7 OF " FontDir     " ENDOF
  8 OF " Font        " ENDOF
  9 OF " Accelerator " ENDOF
 10 OF " RCdata      " ENDOF
 12 OF " GroupCursor " ENDOF 
 14 OF " GroupIcon   " ENDOF 
 16 OF " Version     " ENDOF 
 17 OF " DlgInclude  " ENDOF 
 19 OF " PlugPlay    " ENDOF 
 20 OF " Vxd         " ENDOF 
 21 OF " AniCursor   " ENDOF 
 22 OF " AniIcon     " ENDOF 
 24 OF " Manifest    " ENDOF
   1 <| " Type%-8d" |)
 END-CASE
 THEN
 .ASCIIZ SPACE ;
 
: write-type-nodes ( type-node -- )
  DUP :id @ .restype  DUP :son @ DUP :names @ SWAP :ords @ + . CR
  :son @ ['] write-name-nodes ['] write-dir do-it-and-traverse-tree ;

: write-names ( -- )
  name-begin @ ?DUP 0= IF EXIT THEN 
  BEGIN
    DUP :strref @ ?offset>>
    DUP :id @ atom>>
    :nextname @ 
  ?DUP 0= UNTIL 
  ?align-file ;

\ struct _IMAGE_RESOURCE_DATA_ENTRY {
\ � ULONG�� OffsetToData;
\ � ULONG�� Size;
\ � ULONG�� CodePage;
\ � ULONG�� Reserved; }
: write-leafs ( -- )
  leaf-begin @ ?DUP 0= IF EXIT THEN 
  BEGIN
    >R 
    R@ :leafref @ ?offset0>>
    file-ptr @ R@ :dataref !
    CELL" RSRC" >>
    R@ :size @ >>
    0 >> 0 >>
    R> :nextleaf @ 
  ?DUP 0= UNTIL ;

: write-data ( -- )
  leaf-begin @ ?DUP 0= IF EXIT THEN 
  BEGIN
    DUP :dataref @ ?offset0>> 
    DUP :adr @ OVER :size @ block>>
    ?align-file
    :nextleaf @ 
  ?DUP 0= UNTIL ;

: write-to-file
  map-output-file
  name-begin last-name !
  leaf-begin last-leaf !
  name-begin 0!
  leaf-begin 0!
  type-tree ['] write-type-nodes ['] write-dir do-it-and-traverse-tree
  write-names
  write-leafs
  write-data
  unmap-output-file
;
\ ------------------------------------------
: align-dword ( a -- a' ) DUP CELL MOD DUP 0= IF DROP EXIT THEN CELL SWAP - + ;
: ord/uni-beyond ( a -- a')
  DUP W@ 0xFFFF = IF
    CELL+
  ELSE
    BEGIN DUP W@ 0 <> WHILE
      2+
    REPEAT
    2+
  THEN ;

\ struct RESOURCEHEADER {  
\    DWORD DataSize; 
\    DWORD HeaderSize; 
\    [Ordinal or name TYPE]; 
\    [Ordinal or name NAME];
\    ? word ��� ��ࠢ�������
\    DWORD DataVersion; 
\    WORD MemoryFlags; 
\    WORD LanguageId; 
\    DWORD Version; 
\    DWORD Characteristics; }; 

: enumerate-resources 
  { \ file-end ptr -- }
  fh FILE-SIZE 2DROP DUP TO input-file-size file + TO file-end
  file TO ptr
  BEGIN ptr file-end < WHILE
  \ �ய�᪠�� ᮬ��⥫��� ������ � DataSize=0, ������ ��������� �⠢�� ��ࢮ�
    ptr @ IF 
      ptr 2 CELLS+ ( type) DUP ord/uni-beyond ( name)
      DUP ord/uni-beyond align-dword CELL+ 2+ DUP W@ ( lang id)
      SWAP 2+ 2 CELLS+ ( dataadr) ptr @ ( datasize) add-to-tree
    THEN
    ptr @ ptr CELL+ @ + align-dword ^ ptr +!
  REPEAT ;
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
  S" .FRES" ROT 2DUP + 1+ >R CZMOVE
  PAD R> OVER -
;

: RUN
  ['] my-error TO ERROR
  -1 TO SOURCE-ID 
  GetCommandLineA ASCIIZ> SOURCE!
  ?next 2DROP  \ �ࠫ� ��� 䠩��
  ?next 
  ?DUP 0= IF
    ." FRES 1.01  �८�ࠧ�� �⠭����� 䠩� ����ᮢ .RES � �ଠ� .FRES" CR
    ." �맮�: FRES ��䠩� [���䠩�]" CR
    BYE
  THEN
  ( a #) input-file CZMOVE
  ?next ?DUP 0= IF input-file DUP ZLEN +ext THEN output-file CZMOVE
  new-tree TO type-tree
  map-input-file
  enumerate-resources
  sort-all-trees
  write-to-file
  chop-tree
  unmap-input-file
  BYE ;

0 TO SPF-INIT?
' RUN MAINX !
S" fres.exe" SAVE  
BYE

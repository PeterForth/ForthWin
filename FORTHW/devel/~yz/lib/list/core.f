\ $Id: core.f,v 1.10 2008/12/11 12:53:56 ygreks Exp $
\ ��� ���� ���� ��� �������
\ �������� ������� -- cons pair -- ���� CELL'�� : car � ������� � cdr �� ������

REQUIRE WORDLIST-NAMED ~pinka/spf/compiler/native-wordlist.f
REQUIRE ALSO! ~pinka/lib/ext/basics.f

S" list" WORDLIST-NAMED DROP

list ALSO!
GET-CURRENT DEFINITIONS

\ ������� ������
0
CELL -- node.car \ ������-������
CELL -- node.cdr \ �����
CONSTANT /NODE

\ ������� ����� �������
: NEW-NODE ( -- node )
    /NODE ALLOCATE THROW
    DUP /NODE ERASE
;

\ ���������� ������ ���������� ��������� ������
: FREE-NODE ( node -- ) FREE THROW ;

\ ���������� ����� node1->node2
: LINK-NODE ( node1 node2 -- ) SWAP node.cdr ! ;

\ ������� ����� ������� ������ � ������� val
: node ( val -- node ) NEW-NODE TUCK node.car ! ;

\ nil ��� () - ������ ������� - ��������� ��� �� ���� - ����� ������
HERE /NODE ALLOT CONSTANT nil
nil CONSTANT ()
() () LINK-NODE

\ TRUE - ������� ����, ��� ��������
\ FALSE - �����
: empty? ( node -- ? ) () = ;

\ ������� � ���������� �������� � ������ ����� �������� node1
: cdr ( node1 -- node2 ) node.cdr @ ;

\ ���������� ������ ������ �������� node
: car ( node -- val ) node.car @ ;

\ ���������� ������ ������
: setcar ( val node -- ) DUP empty? IF 2DROP EXIT THEN node.car ! ;

\ ���������� :)
: cddr cdr cdr ;
: cdddr cdr cdr cdr ;
: cdar cdr car ;
: cddar cdr cdr car ;

\ ������ �� ������� ��������� �� ���������� - ������������ �� ()
: end ( node -- node2 )
   BEGIN
   DUP cdr empty? IF EXIT THEN
   cdr
   AGAIN ;

\ �������� ������� � ������ ������ � ������� ������������ ������
\ node1->node2
: cons-node ( node1 node2 -- node1 ) OVER SWAP LINK-NODE ;

\ node1(value)->node
: cons ( value node -- node1 ) SWAP node SWAP cons-node ;

\ ������������ ���� ������ node1 � ������ ������ node2
: concat ( node1 node2 -- node )
   OVER empty? IF NIP EXIT THEN
   OVER end SWAP LINK-NODE ;

\ �������� n-�� ������� ������, ������ ��������
: nth ( n node -- node ) SWAP 0 ?DO cdr LOOP ;

\ �������� ����� ������ - ������ �������� �� ����� ������
: length ( node -- n )
   0 >R
   BEGIN
    DUP empty? IF DROP R> EXIT THEN
    cdr
    RP@ 1+!
   AGAIN ;

\ ���������� ������ ������� �������
: free ( node -- )
   BEGIN
    DUP empty? IF DROP EXIT THEN
    DUP cdr
    SWAP FREE-NODE
   AGAIN ;

: append-node ( node1 node2 -- node ) 
  DUP empty? IF 
    DROP DUP () LINK-NODE 
  ELSE 
    TUCK end OVER LINK-NODE () LINK-NODE 
  THEN ;

\ �������� val � ����� ������ node2 (����� ������ ���������)
\ node2->...->node(val)->nil
: append ( val node2 -- node ) SWAP node SWAP append-node ;

\ ���������� ������ � �������� �������
: reverse ( node -- node1 )
   () >R
   BEGIN
    DUP empty? 0=
   WHILE
    DUP cdr
    SWAP
    R> cons-node >R
   REPEAT
   DROP R> ;

SET-CURRENT PREVIOUS

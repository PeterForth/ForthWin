\ $Id: ext.f,v 1.11 2008/12/12 09:36:23 ygreks Exp $
\ Higher-order functions over lists
\ Single style 
\ - all words take xt as the topmost parameter
\ - all xt's operate on node value

REQUIRE list ~ygrek/lib/list/core.f
REQUIRE { lib/ext/locals.f
REQUIRE LAMBDA{ ~pinka/lib/lambda.f
REQUIRE /TEST ~profit/lib/testing.f
REQUIRE PRO ~profit/lib/bac4th.f

list ALSO!
GET-CURRENT DEFINITIONS

\ ��������� xt � ������ ���� ��������� ������
\ xt: ( val -- ) \ xt �������� ���������� car ������ ������� �������� �� ���������� �����
: iter { node xt -- }
   BEGIN
    node empty? IF EXIT THEN
    node car xt EXECUTE
    node cdr -> node
   AGAIN ;

\ ��������� xt � ���� ��������� ������
\ xt: ( node -- ) \ xt �������� ���������� ������ cons-���� �� ���������� �����
: EACH-NODE { node xt -- }
   BEGIN
    node empty? IF EXIT THEN
    node xt EXECUTE
    node cdr -> node
   AGAIN ;

\ ����� �� ������
\ � ������ ������ (xt ������ TRUE) ������������ node �� ������� ����� ��� ����������
\ ����� - ������ ������
\ xt: ( v -- ? ) \ TRUE - stop search, FALSE - continue search
: find { node xt -- node -1 | empty-list 0 }
   BEGIN
    node empty? IF () FALSE EXIT THEN
    node car xt EXECUTE IF node TRUE EXIT THEN
    node cdr -> node
   AGAIN ;

: exist? ( node xt -- ? ) find NIP ;

\ xt: ( v -- )
: free-with { node xt -- }
   BEGIN
    node empty? IF EXIT THEN
    node car xt EXECUTE
    node cdr 
    node FREE-NODE
    -> node
   AGAIN ;

\ ������� xt ��� ������� �������� ������
\ ���� xt ���������� 0 - ������� ��������� �� ������ (������ ���������� ����� ������� �������������)
\ ����� �������
\ ������������ �������������� ������
\ xt: ( node -- ? ) \ TRUE - remain, FALSE - free node
\ : filter-this { node xt -- node2 }
\   lst(
\    BEGIN
\     node empty? NOT
\    WHILE
\     node xt EXECUTE ( ? ) node DUP cdr -> node SWAP ( ? ) IF add-node ELSE FREE-NODE THEN
\    REPEAT
\   )lst ;

\ xt: ( v -- ? ) \ clean-stack
: partition ( node xt -- l1 l2 )
  { l xt | l1 l2 }
  () -> l1
  () -> l2
  BEGIN
   l empty? 0=
  WHILE
   l car xt EXECUTE l cdr l ROT IF l1 cons-node -> l1 ELSE l2 cons-node -> l2 THEN -> l
  REPEAT
  l1 reverse 
  l2 reverse ;

\ map ������ ����� ������
\ map! �������� �������

\ �������������� ������ ������� ������ � ������� xt
\ xt: ( v1 -- v2 ) \ val ����� �������� � ������� �������������� ������� ������
: map! { node xt -- }
  BEGIN
   node empty? IF EXIT THEN
   node car xt EXECUTE node setcar
   node cdr -> node
  AGAIN ;

\ xt: ( v2 -- )
\ : mapv% { node xt -- l }
\   lst(
\   BEGIN
\    node empty? NOT
\   WHILE
\    node car xt EXECUTE
\    node cdr -> node
\   REPEAT )lst ;

\ ������� ������ ����� n �� ��������� �� ����� v1...vn
: make ( v1 ... vn n -- l ) () { l } 0 ?DO l cons -> l LOOP l ;

\ �������� ������ �� ����
: all ( node -- x1 x2 ... xn n ) DUP length >R ['] NOOP iter R> ;

\ bac4th-�������� �� ������
: each-> ( node --> val \ <-- ) \ clean-stack
   PRO
   BEGIN
    DUP empty? 0=
   WHILE
    DUP >R
    car CONT
    R> cdr
   REPEAT DROP ;

\ bac4th-�������� �� ������
: each=> ( node --> val \ <-- )
   PRO
   BEGIN
    DUP empty? 0=
   WHILE
    DUP car CONT cdr
   REPEAT DROP ;

\ �������� ������� node1 � ������ list ����� ������� ��������
\ ���� list ���� - ������ �� ������
\ list->...->nil
\ list->node1->...->nil
\ : insert-after ( node1 list -- )
\    DUP empty? IF 2DROP EXIT THEN
\    >R
\    R@ cdr cons-node
\    R> SWAP cons-node DROP ;

\ ��������� xt ��������������� � ����� �������� ���������
\ � ��������� ��������� � ������� ������
\ � ���������� ���� ������ ������������� �� ���� �������
\ NB � ���� ������ �� �� ������ �.�. FREE-NODE �� ������ ���� ��������, � ������ ������ ������
\ NB MAY CHANGE
\ xt: ( v1 v2 -- v )
: zip! { l xt -- }
   l cdr empty? IF EXIT THEN
   BEGIN
    l car l cdar xt EXECUTE l setcar
    l cddr empty? IF l cdr FREE-NODE l () LINK-NODE EXIT THEN
    l cdr -> l
   AGAIN ;

\ ��������� xt ��������������� � ������ ���� �������� ���������
\ xt: ( v1 v2 -- )
: zip { l xt | lcdr }
   BEGIN
    l cdr -> lcdr
    l car lcdr DUP empty? NOT
   WHILE
    car
    xt EXECUTE
    lcdr -> l
   REPEAT 2DROP ;

\ ��������� xt � "���������������" ����� ��������� ������� node1 node2
\ xt: ( v1 v2 -- )
: iter2 { node1 node2 xt -- }
   BEGIN
    node1 empty? 0=
   WHILE
    node2 empty? 0=
   WHILE
    node1 car node2 car xt EXECUTE
    node1 cdr -> node1
    node2 cdr -> node2
   REPEAT
   THEN ;

\ �������� �� ��������� �� ��������
: equal0? ( node1 node2 -- ? )
   BEGIN
    DUP empty? IF DROP empty? EXIT THEN
    OVER empty? IF 2DROP FALSE EXIT THEN
    2DUP car SWAP car <> IF 2DROP FALSE EXIT THEN
    cdr SWAP cdr
   AGAIN ;
\    OVER list-what OVER list-what <> IF 2DROP FALSE EXIT THEN
\     DUP value? IF 2DUP car SWAP car <> IF 2DROP FALSE EXIT THEN THEN
\     DUP str? IF 2DUP car STR@ ROT car STR@ COMPARE IF 2DROP FALSE EXIT THEN THEN
\     DUP list? IF 2DUP car SWAP car RECURSE 0= IF 2DROP FALSE EXIT THEN THEN
\     cdr SWAP cdr
\    AGAIN TRUE ;

SET-CURRENT PREVIOUS

0 CONSTANT list-ext

\ -----------------------------------------------------------------------

/TEST

REQUIRE TESTCASES ~ygrek/lib/testcase.f
REQUIRE list-make ~ygrek/lib/list/make.f

TESTCASES list-ext

list ALSO!

0 VALUE l1
0 VALUE l2


: equal? equal0? ;

\
\ map!

%[ 1 % 2 % 3 % ]% TO l1
(( 0 l1 ' + iter -> 6 ))
l1 :NONAME 2 + ; map!
%[ 3 % 4 % 5 % ]% TO l2
(( l1 l2 equal? -> TRUE ))
l1 free
l2 free

\
\ zip!

%[ 1 % 2 % 3 % 4 % 5 % ]% TO l1
l1 ' + zip!
%[ 3 % 5 % 7 % 9 % ]% TO l2
(( l1 l2 equal? -> TRUE ))
l1 free
l2 free

\
\ iter2

1 1 1 1 1 1 DEPTH make TO l1
2 3 0 -2 3 4 DEPTH make TO l2
%[ l1 l2 :NONAME + % ; iter2 ]% ( list )
l1 free
l2 free
( list ) TO l1
%[ 3 % 4 % 1 % -1 % 4 % 5 % ]% TO l2
(( l1 l2 equal? -> TRUE ))
l1 free
l2 free

\
\ concat

%[ 1 % 2 % 3 % 4 % ]%  %[ 5 % 6 % 7 % ]%  concat TO l1
%[ 1 % 2 % 3 % 4 % 5 % 6 % 7 % ]% TO l2
(( l1 l2 equal? -> TRUE ))
l1 free
l2 free

%[ 1 % 2 % 3 % 4 % ]% () concat TO l1
%[ 1 % 2 % 3 % 4 % ]% TO l2
(( l1 l2 equal? -> TRUE ))
l1 free
l2 free

() %[ 1 % 2 % 3 % 4 % ]% concat TO l1
%[ 1 % 2 % 3 % 4 % ]% TO l2
(( l1 l2 equal? -> TRUE ))
l1 free
l2 free

\
\ zip
%[ 1 % 2 % 3 % 3 % 4 % 5 % ]% TO l1
%[ l1 :NONAME + % ; zip ]% TO l2
l1 free
%[ 3 % 5 % 6 % 7 % 9 % ]% TO l1
(( l1 l2 equal? -> TRUE ))
l1 free
l2 free

\
\ partition
%[ 1 % 2 % 3 % 4 % 5 % 6 % ]% :NONAME 2 MOD 0= ; partition ( la lb )
TO l1
%[ 1 % 3 % 5 % ]% TO l2
(( l1 l2 equal? -> TRUE ))
l1 free
l2 free
TO l1
%[ 2 % 4 % 6 % ]% TO l2
(( l1 l2 equal? -> TRUE ))
l1 free
l2 free

PREVIOUS

END-TESTCASES

\ $Id: gcd.f,v 1.1 2007/08/25 09:03:46 ygreks Exp $
\ ���������� ����� ��������
\ �������� ����� �� ������
\ ������ ��� ������� ����� - 1 CELL

REQUIRE ENSURE ~ygrek/lib/debug/ensure.f
REQUIRE /TEST ~profit/lib/testing.f
REQUIRE { lib/ext/locals.f

\ x = n*y+r
: gcd-step ( x y -- y r n ) TUCK /MOD ;

: (gcd) ( x y -- n )
   BEGIN
    gcd-step DROP
    DUP 0= IF DROP EXIT THEN
   AGAIN ;

: check-gcd-conditions ( x y -- x y )
   DUP 0 > ENSURE
   OVER 0 > ENSURE
   2DUP < IF SWAP THEN ;

\ z = ���(x,y)
\ ���������� ����� �������� 
: GCD ( x y -- z ) check-gcd-conditions (gcd) ;

\ x : ax = 1 (mod m)
\ �������� ����� �� ������ 
: InvertNumber { a m | z q p0 p1 -- x }

  a m GCD 1 = ENSURE

  0 -> p0
  1 -> p1
  0 -> z

  m a
  BEGIN
   gcd-step -> q
   DUP
  WHILE
   p1 q * p0 + p1 -> p0 -> p1
   1 z - -> z
  REPEAT
  2DROP
  p1 z IF m SWAP - THEN ; 

/TEST

REQUIRE TESTCASES ~ygrek/lib/testcase.f

TESTCASES GCD
(( 1089 152 GCD -> 1 ))
(( 1 1 GCD -> 1 ))
(( 1000000 200 GCD -> 200 ))
END-TESTCASES

TESTCASES InvertNumber
(( 152 1089 InvertNumber -> 566 ))
(( 1 2 InvertNumber -> 1 ))
(( 3 5 InvertNumber -> 2 ))
(( 103 5 InvertNumber -> 2 ))
END-TESTCASES

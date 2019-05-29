\ $Id: parse.f,v 1.4 2008/05/09 11:30:56 ygreks Exp $

REQUIRE { lib/ext/locals.f
REQUIRE /STRING lib/include/string.f
REQUIRE /TEST ~profit/lib/testing.f

\ ����� ��������� ����� �� �������� ������ �� ������� ������
: PICK-NAME ( -- a u ) >IN @ PARSE-NAME ROT >IN ! ;

\ �������� ��� ����� �� ������� ����
: CUT-FILENAME ( a u -- a2 u2 ) 2DUP CUT-PATH NIP /STRING ;

\ ������� ������ a u �� ��� ���������, ������ ����� n, ������ - ������� ����� u-n
\ ������ �� ������ n > u - ������������ �������� ������ � ������
: /GIVE { a u n -- a+n u-n a n }
    u n < IF u -> n THEN

    a n + 
    u n -
    a
    n ;

\ ������������� ������ a u � ����� (������ ������� ������� ������� ���������)
: NUMBER ( a u -- n -1 | 0 ) 0 0 2SWAP >NUMBER NIP IF 2DROP FALSE ELSE D>S TRUE THEN ; 

/TEST

REQUIRE TESTCASES ~ygrek/lib/testcase.f

TESTCASES ~ygrek/lib/parse.f

S" createdoes>" 6 /GIVE ( S" does>" S" create" ) S" create" TEST-ARRAY S" does>" TEST-ARRAY
S" createdoes>" 15 /GIVE ( S" " S" createdoes>" ) S" createdoes>" TEST-ARRAY S" " TEST-ARRAY 
S" D:\WORK\FORTH\spf4\devel\~ygrek\lib\parse.f" CUT-FILENAME S" parse.f" TEST-ARRAY

(( S" 323" NUMBER -> 323 -1 ))
(( S" 0x323" NUMBER -> 0 ))
(( S" " NUMBER -> 0 -1 )) \ bug or feature?
(( S" -1" NUMBER -> 0 ))

END-TESTCASES

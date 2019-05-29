\ $Id: filelines.f,v 1.5 2007/09/23 18:26:14 ygreks Exp $
\ ���������� ������ �����
\ READ-LINE ������������� ����� ���������� ����

REQUIRE STR@ ~ac/lib/str5.f
REQUIRE PRO ~profit/lib/bac4th.f
REQUIRE { lib/ext/locals.f

CREATE buf 1026 ALLOT

: MY-READ-LINE ( c-addr u1 fileid -- u2 eof-flag flag ior ) \ 94 FILE
  DUP >R
  FILE-POSITION IF 2DROP 0 0 THEN _fp1 ! _fp2 !
  LTL @ +
  OVER _addr !

  R@ READ-FILE ?DUP IF NIP RDROP >R 0 0 0 R> EXIT THEN

  DUP >R 0= IF RDROP RDROP 0 0 0 0 EXIT THEN \ ���� � ����� �����

  _addr @ R@ LT LTL @ SEARCH
  IF   \ ������ ����������� �����
     DROP _addr @ -
     DUP
     LTL @ + S>D _fp2 @ _fp1 @ D+ RDROP R> REPOSITION-FILE DROP
     TRUE
  ELSE \ �� ������ ����������� �����
     2DROP
     R> RDROP  \ ���� ������ ��������� �� ��������� - ����� ���������
     FALSE
  THEN
  TRUE 0
;

: file-get-line { h | str -- s | 0 }
   "" TO str
   BEGIN
    buf 1024 h MY-READ-LINE THROW 0= IF 2DROP str STRFREE 0 EXIT THEN
    IF buf SWAP str STR+ str EXIT
    ELSE buf SWAP str STR+ THEN
   AGAIN
;

: FileLines=> ( a u --> s \ <-- s )
  PRO
  R/O OPEN-FILE-SHARED IF DROP EXIT THEN
\  START{
  BEGIN
   DUP file-get-line DUP
  WHILE
   CONT
   STRFREE
  REPEAT
  DROP
\  }EMERGE
  CLOSE-FILE THROW ;

\EOF

: TEST S" filelines.f" FileLines=> CR DUP STR@ TYPE ;
TEST

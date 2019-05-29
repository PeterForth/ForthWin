\ $Id: sqlite3.f,v 1.1 2007/02/18 14:59:42 ygreks Exp $
\
\ bac4th ��������� ��� SQLite

REQUIRE db3_open ~ac/lib/lin/sql/sqlite3.f
REQUIRE PRO ~profit/lib/bac4th.f
REQUIRE STATIC ~profit/lib/static.f
REQUIRE /TEST ~profit/lib/testing.f

\ ������ ��� db3_enum
: sql.enum { a u xt db | rrr }
   \ xt: i par ppStmt -- ? \ ? - ���� �����������
   a u rrr xt db db3_enum ; 
   \ ['] CATCH ?DUP IF ." !!!" S" sql.enum" db db3_error? 2DROP 2DROP DROP THEN

\ ������� sql-rows
\ � �������� ������ ������ ��������� ���� �����������
: sql.enum=> ( a u db --> i par pp \ <-- ? ) R> SWAP sql.enum ;

\ ����� �� ���� ���� ����� ������ pp (������ ���� ��� ������ a u)
: pp.data { pp -- i*x }
   pp db3_cols 0 DO
    pp db3_cols 1- I - pp db3_col 
   LOOP ;

\ �������� �� ����� ������
: pp.data=> ( pp --> a u \ <-- a u )
   PRO
   STATIC pp
   pp !
   pp @ db3_cols 0 DO
    I pp @ db3_col CONT 2DROP
   LOOP ;

\ ������ ���� pp �� sql �������, ����� ������� ������ sql.enum=>
: sql.pp=> ( sql-a sql-u db --> pp \ <-- pp )
  PRO
   sql.enum=>
   NIP NIP
   ( pp ) CONT ( pp )
   DROP
   TRUE ;

\ ������� ���� ���� a u
\ ��� ������ - �������
: db.open=> ( a u --> db \ <-- db )
   PRO db3_open CONT db3_close ;

/TEST

: create-tables 
  S" CREATE TABLE IF NOT EXISTS TEST1 (ID INTEGER PRIMARY KEY AUTOINCREMENT,str TEXT);begin;commit;" 
  ROT
  db3_exec_ ;

: test
   S" test1.db3" db.open=> DUP 
   >R
   R@ create-tables 
   S" INSERT INTO TEST1 (str) VALUES ('hello1')" R@ db3_exec_
   S" INSERT INTO TEST1 (str) VALUES ('hello2')" R@ db3_exec_
   S" SELECT * FROM TEST1" R@ START{ sql.pp=> DUP pp.data CR TYPE 2 SPACES TYPE }EMERGE
   RDROP
   ;
   
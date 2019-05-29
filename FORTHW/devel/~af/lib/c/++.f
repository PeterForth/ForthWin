\ Andrey Filatkin, af@forth.org.ru
\ ������ ���������\��������� ��� VAR-����������.

REQUIRE AT	~af/lib/var.f
REQUIRE 1-!	~af/lib/decr.f

\ ��������� �������� VAR-���������� �� �������
: ^++ ( "var" -- )
  POSTPONE AT
  STATE @ IF POSTPONE 1+! ELSE 1+! THEN
; IMMEDIATE

\ �������� �������� VAR-���������� �� ����, ��������� ���������� �� 1
: @++ ( "var" -- n )
  POSTPONE AT
  STATE @ IF POSTPONE DUP POSTPONE @ POSTPONE SWAP POSTPONE 1+!
  ELSE DUP @ SWAP 1+! THEN
; IMMEDIATE

\ ��������� �������� VAR-���������� �� �������, �������� ��� �������� �� ����
: ++@ ( "var" -- n )
  POSTPONE AT
  STATE @ IF POSTPONE DUP POSTPONE 1+! POSTPONE @
  ELSE DUP 1+! @ THEN
; IMMEDIATE

: ^-- ( "var" -- )
  POSTPONE AT
  STATE @ IF POSTPONE 1-! ELSE 1-! THEN
; IMMEDIATE

: @-- ( "var" -- n )
  POSTPONE AT
  STATE @ IF POSTPONE DUP POSTPONE @ POSTPONE SWAP POSTPONE 1-!
  ELSE DUP @ SWAP 1-! THEN
; IMMEDIATE

: --@ ( "var" -- n )
  POSTPONE AT
  STATE @ IF POSTPONE DUP POSTPONE 1-! POSTPONE @
  ELSE DUP 1-! @ THEN
; IMMEDIATE

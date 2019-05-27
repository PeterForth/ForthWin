\ $Id: spf_win_io.f,v 1.19 2008/11/12 09:53:19 spf Exp $

( �������� ����-�����.
  Windows-��������� �����.
  Copyright [C] 1992-1999 A.Cherezov ac@forth.org
  �������������� �� 16-���������� � 32-��������� ��� - 1995-96��
  ������� - �������� 1999
)

: CLOSE-FILE ( fileid -- ior ) \ 94 FILE
\ ������� ����, �������� fileid.
\ ior - ������������ ����������� ��� ���������� �����/������.
  CloseHandle ERR
;

: CREATE-FILE ( c-addr u fam -- fileid ior ) \ 94 FILE
\ ������� ���� � ������, �������� c-addr u, � ������� ��� � ������������
\ � ������� ������� fam. ����� �������� fam ��������� �����������.
\ ���� ���� � ����� ������ ��� ����������, ������� ��� ������ ���
\ ������ ����.
\ ���� ���� ��� ������� ������ � ������, ior ����, fileid ��� �������������,
\ � ��������� ������/������ ���������� �� ������ �����.
\ ����� ior - ������������ ����������� ��� ���������� �����/������,
\ � fileid �����������.
  NIP SWAP >R >R
  0 FILE_ATTRIBUTE_ARCHIVE ( template attrs )
  CREATE_ALWAYS
  0 ( secur )
  0 ( share )  
  R> ( access=fam )
  R> ( filename )
  CreateFileA DUP -1 = IF GetLastError ELSE 0 THEN
;

CREATE SA 12 , 0 , 1 ,

: CREATE-FILE-SHARED ( c-addr u fam -- fileid ior )
  NIP SWAP >R >R
  0 FILE_ATTRIBUTE_ARCHIVE ( template attrs )
  CREATE_ALWAYS
  SA ( secur )
  3 ( share )  
  R> ( access=fam )
  R> ( filename )
  CreateFileA DUP -1 = IF GetLastError ELSE 0 THEN
;
: OPEN-FILE-SHARED ( c-addr u fam -- fileid ior )
  NIP SWAP >R >R
  0 FILE_ATTRIBUTE_ARCHIVE ( template attrs )
  OPEN_EXISTING
  SA ( secur )
  7 ( share )  
  R> ( access=fam )
  R> ( filename )
  CreateFileA DUP -1 = IF GetLastError ELSE 0 THEN
;

: DELETE-FILE ( c-addr u -- ior ) \ 94 FILE
\ ������� ���� � ������, �������� ������� c-addr u.
\ ior - ������������ ����������� ��� ���������� �����/������.
  DROP DeleteFileA ERR
;

USER lpDistanceToMoveHigh

: FILE-POSITION ( fileid -- ud ior ) \ 94 FILE
\ ud - ������� ������� � �����, ���������������� fileid.
\ ior - ������������ ����������� ��� ���������� �����/������.
\ ud �����������, ���� ior �� ����.
  >R FILE_CURRENT lpDistanceToMoveHigh DUP 0! 0 R>
  SetFilePointer
  DUP -1 = IF GetLastError ELSE 0 THEN
  lpDistanceToMoveHigh @ SWAP
;

: FILE-SIZE ( fileid -- ud ior ) \ 94 FILE
\ ud - ������ � �������� �����, ���������������� fileid.
\ ior - ������������ ����������� ��� ���������� �����/������.
\ ��� �������� �� ������ �� ��������, ������������ FILE-POSITION.
\ ud �����������, ���� ior �� ����.
  lpDistanceToMoveHigh SWAP
  GetFileSize
  DUP -1 = IF GetLastError ELSE 0 THEN
  lpDistanceToMoveHigh @ SWAP
;

: OPEN-FILE ( c-addr u fam -- fileid ior ) \ 94 FILE
\ ������� ���� � ������, �������� ������� c-addr u, � ������� ������� fam.
\ ����� �������� fam ��������� �����������.
\ ���� ���� ������� ������, ior ����, fileid ��� �������������, � ����
\ �������������� �� ������.
\ ����� ior - ������������ ����������� ��� ���������� �����/������,
\ � fileid �����������.
  NIP SWAP >R >R
  0 FILE_ATTRIBUTE_ARCHIVE ( template attrs )
  OPEN_EXISTING
  0 ( secur )
  0 ( share )  
  R> ( access=fam )
  R> ( filename )
  CreateFileA DUP -1 = IF GetLastError ELSE 0 THEN
;

USER lpNumberOfBytesRead

: READ-FILE ( c-addr u1 fileid -- u2 ior ) \ 94 FILE
\ �������� u1 �������� � c-addr �� ������� ������� �����,
\ ����������������� fileid.
\ ���� u1 �������� ��������� ��� ����������, ior ���� � u2 ����� u1.
\ ���� ����� ����� ��������� �� ��������� u1 ��������, ior ����
\ � u2 - ���������� ������� ����������� ��������.
\ ���� �������� ������������ ����� ��������, ������������
\ FILE-POSITION ����� ��������, ������������� FILE-SIZE ��� �����
\ ����������������� fileid, ior � u2 ����.
\ ���� �������� �������������� ��������, �� ior - ������������ �����������
\ ��� ���������� �����/������, � u2 - ���������� ��������� ���������� �
\ c-addr ��������.
\ �������������� �������� ���������, ���� �������� �����������, �����
\ ��������, ������������ FILE-POSITION ������ ��� ��������, ������������
\ FILE-SIZE ��� �����, ����������������� fileid, ��� ��������� ��������
\ �������� �������� ������������ ����� �����.
\ ����� ���������� �������� FILE-POSITION ��������� ��������� �������
\ � ����� ����� ���������� ������������ �������.
  >R 2>R
  0 lpNumberOfBytesRead R> R> R>
  ReadFile ERR
  lpNumberOfBytesRead @ SWAP
;

: REPOSITION-FILE ( ud fileid -- ior ) \ 94 FILE
\ ������������������� ����, ���������������� fileid, �� ud.
\ ior - ������������ ����������� ��� ���������� �����-������.
\ �������������� �������� ���������, ���� ��������������� ���
\ ��� ������.
\ ����� ���������� �������� FILE-POSITION ���������� �������� ud.
  >R lpDistanceToMoveHigh ! FILE_BEGIN lpDistanceToMoveHigh ROT R>
  SetFilePointer
  -1 = IF GetLastError ELSE 0 THEN
;


USER _fp1
USER _fp2
USER _addr

: READ-LINE ( c-addr u1 fileid -- u2 flag ior ) \ 94 FILE
\ �������� ��������� ������ �� �����, ��������� fileid, � ������
\ �� ������ c-addr. �������� �� ������ u1 ��������. �� ����
\ ������������ ����������� �������� "����� ������" ����� ����
\ ��������� � ������ �� ������ ������, �� �� �������� � ������� u2.
\ ����� ������ c-addr ������ ����� ������ ��� ������� u1+2 �������.
\ ���� �������� �������, flag "������" � ior ����. ���� ����� ������
\ ������� �� ���� ��� ��������� u1 ��������, �� u2 - ����� �������
\ ����������� �������� (0<=u2<=u1), �� ������ �������� "����� ������".
\ ����� u1=u2 ����� ������ ��� �������.
\ ���� �������� ������������, ����� ��������, ������������
\ FILE-POSITION ����� ��������, ������������� FILE-SIZE ��� �����,
\ ����������������� fileid, flag "����", ior ����, � u2 ����.
\ ���� ior �� ����, �� ��������� �������������� �������� � ior -
\ ������������ ����������� ��� ���������� �����-������.
\ �������������� �������� ���������, ���� �������� �����������, �����
\ ��������, ������������ FILE-POSITION ������ ��� ��������, ������������
\ FILE-SIZE ��� �����, ����������������� fileid, ��� ��������� ��������
\ �������� �������� ������������ ����� �����.
\ ����� ���������� �������� FILE-POSITION ��������� ��������� �������
\ � ����� ����� ���������� ������������ �������.
  DUP >R
  FILE-POSITION IF 2DROP 0 0 THEN _fp1 ! _fp2 !
  LTL @ +
  OVER _addr !

  R@ READ-FILE ?DUP IF NIP RDROP 0 0 ROT EXIT THEN

  DUP >R 0= IF RDROP RDROP 0 0 0 EXIT THEN \ ���� � ����� �����

  _addr @ R@ EOLN SEARCH
  IF   \ ������ ����������� �����
     DROP _addr @ -
     DUP
     LTL @ + S>D _fp2 @ _fp1 @ D+ RDROP R> REPOSITION-FILE DROP
  ELSE \ �� ������ ����������� �����
     2DROP
     R> RDROP  \ ���� ������ ��������� �� ��������� - ����� ���������
  THEN
  TRUE 0
;


USER lpNumberOfBytesWritten  \ not used by core more

: WRITE-FILE ( c-addr u fileid -- ior ) \ 94 FILE
\ �������� u �������� �� c-addr � ����, ���������������� fileid,
\ � ������� �������.
\ ior - ������������ ����������� ��� ���������� �����-������.
\ ����� ���������� �������� FILE-POSITION ���������� ���������
\ ������� � ����� �� ��������� ���������� � ���� ��������, �
\ FILE-SIZE ���������� �������� ������� ��� ������ ��������,
\ ������������� FILE-POSITION.
  OVER >R
  >R 2>R
  0 SP@ 0 SWAP R> R> R>
  WriteFile ERR ( u2 ior )
  ?DUP IF RDROP NIP EXIT THEN
  R> <>
  ( ���� ���������� �� �������, ������� �����������, �� ���� ������ )
;
\ ������������ ���������� ������ �� ����� ������ ������ USER-����������,
\ ����� ����� ������� ���� ��� �������� �������� TlsIndex.
\ ������� ��������� ���������� � ��������������, �� ���� ���������
\ Windows ������������ �������, ��� ����� ����� ;-)
\ ~ruv, 11.2008


: RESIZE-FILE ( ud fileid -- ior ) \ 94 FILE
\ ���������� ������ �����, ����������������� fileid, ������ ud.
\ ior - ������������ ����������� ��� ���������� �����-������.
\ ���� �������������� ���� ���������� ������, ��� �� ��������,
\ ����� �����, ����������� � ���������� ��������, ����� ����
\ �� ��������.
\ ����� ���������� �������� FILE-SIZE ���������� �������� ud
\ � FILE-POSITION ���������� �������������� ��������.
  DUP >R REPOSITION-FILE  ?DUP IF RDROP EXIT THEN
  R> SetEndOfFile ERR
;

: WRITE-LINE ( c-addr u fileid -- ior ) \ 94 FILE
\ �������� u �������� �� c-addr � ����������� ��������� �� ���������� ������ 
\ ������ � ����, ���������������� fileid, ������� � ������� �������.
\ ior - ������������ ����������� ��� ���������� �����-������.
\ ����� ���������� �������� FILE-POSITION ���������� ���������
\ ������� � ����� �� ��������� ���������� � ���� ��������, �
\ FILE-SIZE ���������� �������� ������� ��� ������ ��������,
\ ������������� FILE-POSITION.
  DUP >R WRITE-FILE ?DUP IF RDROP EXIT THEN
  EOLN R> WRITE-FILE
;

: FLUSH-FILE ( fileid -- ior ) \ 94 FILE EXT
  FlushFileBuffers ERR
;

\ TRUE ���� ���������� ���� addr u
: FILE-EXIST ( addr u -- f )
  DROP GetFileAttributesA -1 <>
;

\ TRUE ���� ���� addr u ���������� � �� �������� ���������
: FILE-EXISTS ( addr u -- f )
  DROP GetFileAttributesA INVERT 16 ( FILE_ATTRIBUTE_DIRECTORY) AND 0<>
;

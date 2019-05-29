\ From: "Ilya S. Potrepalov" <potrepalov@asc-ural.ru>
\ Newsgroups: fido7.su.forth
\ Date: Tue, 28 Feb 2006 04:25:10 +0000 (UTC)

\ READ-LINE  ������ ������ �� �����
BASE @ DECIMAL

USER _lt_
USER _ltl_

USER sb

: DOS-LINES     ( -- )      13 _lt_ C!  10 _lt_ 1+ C!  2 _ltl_ ! ;
: UNIX-LINES    ( -- )      10 _lt_ C!                 1 _ltl_ ! ;

DOS-LINES

: (shift-position)       ( n file_id -- ior )
    TUCK FILE-POSITION ?DUP
    IF   NIP NIP EXIT  THEN
    ROT S>D D+ ROT REPOSITION-FILE 
;


: READ-LINE    ( ac u1 file_id -- u2 f ior )
    \ �������� ��������� ������ �� �����, �������� file_id, � ������
    \  �� ������ ac.  �������� �� ������ u1 �������� (������).  �� ����
    \  ������������ ����������� �������� "����� ������" ����� ����
    \  ��������� � ������ �� ������ ������, �� �� �������� � ������� u2.
    \  ����� ������ ac ������ ����� ������ ��� ������� u1+2 �������.
    \ ���� �������� �������, ���� f "������" � ior ����.  ���� ����� ������
    \  ������� �� ����, ��� ��������� u1 ��������, �� u2 - ����� �������
    \  ����������� �������� (0<=u2<=u1), �� ������ ������� "����� ������".
    \ ����� u2=u1 ����� ������ ��� �� ��������.
    \ ��� ��� (� WINDOWS) ���� file_id ������ ���� ������ � ������ BIN

    >R  SWAP DUP sb !       ( �������-�����-��������� �����-����-������ )
    BEGIN
        2DUP SWAP 255 MIN 
        _ltl_ @ 1- +        \ ������ ����� ������ ��� ������ �����
        R@ OVER >R READ-FILE        ( u a  u2' ior )
        ?DUP IF
            SWAP 2SWAP NIP +                        ( ior a+u2' )
            sb @ -                                  ( ior u2 )
            0 ROT   RDROP RDROP EXIT
        THEN                                        ( u a u2' )

        2DUP OVER sb @ <>                           ( u a u2' a u2' f )
        \ ���� ������ �� ������ ��������, �� ��� ������
        \ ����� ������ ���������� ���� ���� �� ����������� ���������
        IF  1+ SWAP 1- SWAP  THEN
        _lt_ _ltl_ @ SEARCH
        IF
            RDROP >R
            sb @ -  NIP NIP NIP R>                  ( u2 u3 )
            _ltl_ @ -       \ �� ���������� crlf
            ?DUP IF
                NEGATE R@ (shift-position)
                ?DUP  IF  0 SWAP  RDROP EXIT  THEN
            THEN
            -1 0  RDROP EXIT
        THEN
        2DROP                                       ( u a u2' )
        R> OVER <>
        IF  \ �������� �� ����� �����               ( u a u2' )
            \ ��������� ������ ����� ��������� �������, �� ���� ��� �����������
            + NIP sb @ - DUP 0<> 0 RDROP EXIT
        THEN
        TUCK  + >R  - R>
        OVER 1 <
    UNTIL
    OVER +  sb @ -
    SWAP ?DUP IF
        R@ (shift-position)
        ?DUP IF  0 SWAP  RDROP EXIT  THEN
    THEN
    -1 0  RDROP
;

BASE !

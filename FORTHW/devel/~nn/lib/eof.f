: \EOF  ( -- )
  POSTPONE \
  BEGIN REFILL 0= UNTIL
  POSTPONE \
;

: <EOF> \EOF ;

: >EOF ( h --)
    >R
    R@ FILE-SIZE 0=
    IF R@ REPOSITION-FILE DROP
    ELSE 2DROP THEN
    RDROP
;

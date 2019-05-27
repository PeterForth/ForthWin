
\  Work with the buffer of the last entered lines


  8 CONSTANT LruNum \ number of memorized messages lru
255 CONSTANT LruLen \ single line buffer size lru

  0 VALUE CurrFromLru
  0 VALUE LruBuf \ history buffer (last recently used)

: LruAddr ( n -- addr )
  LruLen * LruBuf +
;

: NextLru
  CurrFromLru
  LruNum 1- = IF 0 TO CurrFromLru
           ELSE CurrFromLru 1+ TO CurrFromLru
           THEN
;
: PrevLru
  CurrFromLru
  0  =     IF LruNum 1- TO CurrFromLru
           ELSE CurrFromLru 1- TO CurrFromLru
           THEN
;

: AddToLru ( addr u )
  DUP 0= IF 2DROP EXIT THEN
  CurrFromLru
  LruAddr 2DUP C!
  1+ 2DUP 2>R
  SWAP CMOVE
  2R> + 0 SWAP C!
  NextLru
;

: UpLru ( -- addr u )
   PrevLru
   CurrFromLru
   LruAddr COUNT
;

: DownLru ( -- addr u )
   NextLru
   CurrFromLru
   LruAddr COUNT
;

: LruList
  LruNum 0
  DO
    I LruAddr ?DUP IF COUNT TYPE CR THEN
  LOOP
;

WINAPI: InitializeCriticalSection KERNEL32.DLL
WINAPI: DeleteCriticalSection KERNEL32.DLL
WINAPI: EnterCriticalSection KERNEL32.DLL
WINAPI: LeaveCriticalSection KERNEL32.DLL
WINAPI: TryEnterCriticalSection KERNEL32.DLL

24 CONSTANT /CRITICAL-SECTION
VARIABLE CRITICAL-SECTION-LIST
: CRITICAL-SECTION
    CREATE HERE /CRITICAL-SECTION ALLOT
    /CRITICAL-SECTION ERASE
      HERE CRITICAL-SECTION-LIST @ ,
      CRITICAL-SECTION-LIST !
;

: FREE-CRITICAL-SECTIONS
    CRITICAL-SECTION-LIST
    BEGIN @ ?DUP WHILE
        DUP /CRITICAL-SECTION - /CRITICAL-SECTION ERASE
    REPEAT
;

: ?CRIT-INIT ( a -- ) DUP @ 0= IF InitializeCriticalSection DROP ELSE DROP THEN ;

: CRIT-ENTER ( a -- )
    DUP ?CRIT-INIT
    EnterCriticalSection DROP ;

: CRIT-LEAVE ( a -- )  LeaveCriticalSection DROP ;

: CRIT-TRY ( a -- ?) DUP ?CRIT-INIT TryEnterCriticalSection 0<> ;

: CRIT-DELETE ( a -- ?) DeleteCriticalSection DROP ;

WARNING @ WARNING 0!
: SAVE
    FREE-CRITICAL-SECTIONS
    SAVE
;
WARNING !

\EOF

CRITICAL-SECTION cs1

: test
    cs1 ?CRIT-INIT
    cs1 24 DUMP CR
;
test

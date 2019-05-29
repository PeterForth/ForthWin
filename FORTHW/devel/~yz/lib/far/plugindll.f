DIS-OPT

REQUIRE {       ~ac\lib\locals.f

' NOOP ' MAINX EXECUTE !

VECT FARPLUGIN-INIT
' NOOP TO FARPLUGIN-INIT

: (INIT1)
  0 TO H-STDLOG
  0 TO H-STDIN
  CONSOLE-HANDLES
  ['] CGI-OPTIONS ERR-EXIT
  ['] AT-PROCESS-STARTING ERR-EXIT
  MAINX @ ?DUP IF ERR-EXIT THEN
  FARPLUGIN-INIT
;

: (INIT2)
  (INIT1)
  SPF-INIT?  IF
    ['] SPF-INI ERR-EXIT
  THEN OPTIONS
  CGI? @ 0= POST? @ OR IF ['] <MAIN> ERR-EXIT THEN
  BYE
;

: (dllinit) ( reserved reason hinstance -- retcode )
  OVER 0 = IF  ( ." DLL_PROCESS_DETACH " )       ELSE
  OVER 1 = IF (INIT1) ( ." DLL_PROCESS_ATTACH ") ELSE
  OVER 2 = IF ( ." DLL_THREAD_ATTACH ")  ELSE
  OVER 3 = IF ( ." DLL_THREAD_DETACH ")  ELSE
\  OVER  .
  THEN THEN THEN THEN \ CR
  2DROP DROP
  1  \ 0 to fail
;

' (dllinit) 3 CELLS CALLBACK: DllMain

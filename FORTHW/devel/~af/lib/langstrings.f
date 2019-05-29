REQUIRE :M           ~af/lib/nwordlist.f
REQUIRE USES         ~af/lib/api-func.f
USES kernel32.dll
0 CONSTANT CP_ACP
REQUIRE S>UNICODE    ~nn/lib/unicode.f

VOCABULARY LStrings
GET-CURRENT ALSO LStrings DEFINITIONS

: lang: POSTPONE \ ;

: : ( "string" -- )
  ALIGN-BYTES @ >R 2 ALIGN-BYTES !
  NHEADER
  R> ALIGN-BYTES !
  SkipDelimiters
  1 PARSE -TRAILING
  S>UNICODE HERE SWAP DUP ALLOT CMOVE 0 W,
;

PREVIOUS SET-CURRENT

: INCLUDED-STRINGS ( addr u wid -- )
  ALSO LStrings
  GET-CURRENT >R ALSO CONTEXT ! DEFINITIONS
  INCLUDED
  PREVIOUS R> SET-CURRENT
  PREVIOUS
;

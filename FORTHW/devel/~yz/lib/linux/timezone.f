\ $Id: timezone.f,v 1.2 2008/12/15 09:50:57 ygreks Exp $
\
\ Get timezone (linux)
\ Provides words required for ~ac/lib/win/date/date-int.f

REQUIRE TIME&DATE lib/include/facil.f
REQUIRE /TEST ~profit/lib/testing.f

VARIABLE TZ

: GET-TIME-ZONE ( -- )
  (( 0 )) time (( SP@ TM )) localtime_r DROP DROP 
  TM tm_gmtoff @ 60 / NEGATE TZ ! ;

..: AT-PROCESS-STARTING GET-TIME-ZONE ;..
GET-TIME-ZONE

/TEST

REQUIRE { lib/ext/locals.f
REQUIRE CurrentDateTime# ~ac/lib/win/date/date-int.f

<# CurrentDateTime# 0 0 #> TYPE CR

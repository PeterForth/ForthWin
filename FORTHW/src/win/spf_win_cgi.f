\ $Id: spf_win_cgi.f,v 1.3 2006/12/04 21:16:00 ygreks Exp $

VARIABLE CGI?
VARIABLE POST?

: CGI-OPTIONS ( -- )
  S" REQUEST_METHOD" ENVIRONMENT? DUP CGI? !
  IF 
    S" POST" COMPARE 0= POST? !
  THEN
;

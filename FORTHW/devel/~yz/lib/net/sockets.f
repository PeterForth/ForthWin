\ $Id: sockets.f,v 1.1 2008/12/08 16:16:43 ygreks Exp $
\ Redirect

REQUIRE [DEFINED] lib/include/tools.f

[DEFINED] WINAPI: [IF]
S" ~ac/lib/win/winsock/SOCKETS.F" INCLUDED
[ELSE]
S" ~ygrek/lib/linux/sockets.f" INCLUDED
: SocketsStartup ( -- ior ) 0 ;
[THEN]

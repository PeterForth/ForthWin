\ $Id: rtrace.f,v 1.2 2007/07/29 20:19:39 ygreks Exp $

5 VALUE RTRACE-DEPTH

: RTRACE
   CR ." R-STACK TRACE : "
   CR
   RP@ RTRACE-DEPTH CELLS
   OVER + SWAP DO I STACK-ADDR. DROP CELL +LOOP ;
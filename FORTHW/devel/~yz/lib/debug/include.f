\ $Id: include.f,v 1.1 2008/05/10 10:20:56 ygreks Exp $
\ 
\ Show all included source files (in order)

:NONAME
  2DROP
  CR
  INCLUDE-DEPTH @ 2 * SPACES
  CURFILE @ ASCIIZ> 2DUP TYPE
  (INCLUDED1) 
; 
TO (INCLUDED)

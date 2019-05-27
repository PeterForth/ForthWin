\ $Id: spf_win_module.f,v 1.11 2008/03/28 10:01:49 ygreks Exp $

: is_path_delimiter ( c -- flag )
  DUP [CHAR] \ = SWAP [CHAR] / = OR
;

: ModuleName ( -- addr u )
  1024 SYSTEM-PAD 0 GetModuleFileNameA
  SYSTEM-PAD SWAP
;

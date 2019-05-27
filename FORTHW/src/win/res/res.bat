REM $Id: res.bat,v 1.1 2008/08/07 08:42:43 ygreks Exp $
REM Create spf.fres
REM You can use any resource compiler instead of rc

rc spf.rc
..\..\..\spf4 ~yz/prog/fres/fres.f
fres spf.res
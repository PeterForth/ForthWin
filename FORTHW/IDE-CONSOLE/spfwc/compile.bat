
if exist spf4wc.res goto res_done
REM Compile resource file. Any standard RC file compiler will do.
 REM For example, rc.exe from the MS Visual Studio distribution, 
REM brcc32.exe from the Borland product distribution
windres spf4wc.rc spf4wc.res
:res_done
if exist ..\..\..\devel\~yz\prog\fres\fres.exe goto fres_exe_done
cd ..\..\..\devel\~yz\prog\fres
..\..\..\..\spf4.exe fres.f
cd ..\..\..\..\samples\win\spfwc
:fres_exe_done
if exist spf4wc.fres goto fres_done
..\..\..\devel\~yz\prog\fres\fres.exe spf4wc.res
:fres_done
..\..\..\spf4.exe spf4wc.f

@if exist spf4wc.exe move /Y spf4wc.exe ..\..\..\spf4wc.exe & echo (!) The spf4wc.exe is located near the spf4.exe & pause


SET SPF_ROOT=../../../..
xsltproc devel_libs.xsl %SPF_ROOT%/docs/devel.xml > devel.docbook
echo @echo off > libs2docbook.bat
xsltproc devel_libs2.xsl %SPF_ROOT%/docs/devel.xml >> libs2docbook.bat
echo @echo off > libs2words.bat
xsltproc devel_libs3.xsl %SPF_ROOT%/docs/devel.xml >> libs2words.bat

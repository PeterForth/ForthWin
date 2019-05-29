<?xml version="1.0" encoding="windows-1251"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/htmlhelp/htmlhelp.xsl"/>

  <xsl:include href="devel.basic.xsl"/>

  <!-- �������� ��������� ������ -->
  <xsl:param name="htmlhelp.chm" select="'devel.chm'"/>
  <xsl:param name="htmlhelp.hhp" select="'devel.hhp'"/>
  <xsl:param name="htmlhelp.hhc" select="'devel.hhc'"/>
  <xsl:param name="htmlhelp.hhk" select="'devel.hhk'"/>

  <xsl:param name="htmlhelp.hhc.binary" select="0"/>

  <!-- ������ �������� � ������ ���������� -->
  <xsl:param name="htmlhelp.hhc.folders.instead.books" select="0"/>

  <!--xsl:param name="htmlhelp.hhc.show.root" select="0"></xsl:param>
  <xsl:param name="htmlhelp.default.topic" select="'pr01.html'"/-->

  <!-- ������� ��� ���������� html ������ -->
  <xsl:param name="base.dir" select="'chm/'"></xsl:param>

  <xsl:param name="htmlhelp.hhc.section.depth" select="1"/>

  <xsl:param name="htmlhelp.chunk.first.sections" select="0"></xsl:param>

  <!-- ������ ���� � index.hhk ����� ������ �� ������� ���������� �� ��������������� ����� -->
  <xsl:param name="htmlhelp.use.hhk" select="1"></xsl:param>

</xsl:stylesheet>

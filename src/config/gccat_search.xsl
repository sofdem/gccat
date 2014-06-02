<?xml version="1.0" encoding="iso-8859-1"?>

<xsl:stylesheet
   version="1.0"
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:bibtex="http://bibtexml.sf.net/"
   xmlns="http://www.w3.org/1999/xhtml"
   xsl:exclude-result-prefixes="bibtex"
>

  <!-- xsl:template name="myindex">
	<xsl:document href="myindex.html" method="xml" version="1.0" encoding="iso-8859-15" indent="yes" omit-xml-declaration="yes"
				  doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN" 
				  doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
	  <html><link rel="stylesheet" href="css/gccat.css" type="text/css" /></head>
	  <body><div id="wrap">
		  <div id="header">HEAD</div>
		  <div id="nav">NAV</div>
		  <div id="mainbody">
			<div id="index"><xsl:apply-templates select="//div1[contains(head,'Catalog')]" mode="search.constraint"/></div>
		  </div>
		  <div id="footer">FOOT</div>
		</div>
	  </body>
	</html>
  </xsl:document>
</xsl:template !-->

  <xsl:template match="div2|div3" mode="toc.alpha" >
	<li><a><xsl:attribute name="href"><xsl:apply-templates select="." mode="WebPage"/></xsl:attribute><xsl:value-of select="head"/></a> </li>
  </xsl:template>
  
  <!-- search constraint alphabetically -->
  <xsl:template match="div1[contains(head,'Catalog')]" mode="search.constraint">
	<div class="indalpha">Find constraint: 
	  <ul><xsl:call-template name="index.alpha"><xsl:with-param name="it" select="1"/></xsl:call-template></ul>
	</div>
	<xsl:call-template name="index.list.alpha"><xsl:with-param name="it" select="1"/></xsl:call-template>
  </xsl:template>

  <xsl:template name="index.alpha" >
	<xsl:param name="it"/>
	<xsl:if test="$it &lt;= 26">
	  <xsl:variable name="letter"><xsl:number value="$it" format="a"/></xsl:variable>
	  <xsl:if test="./div2[starts-with(normalize-space(head),$letter)]">
		<li><a href="#C{$letter}"><xsl:value-of select="$letter"/></a> </li>
	  </xsl:if>
	  <xsl:call-template name="index.alpha"><xsl:with-param name="it" select="$it + 1"/></xsl:call-template>
	</xsl:if>
  </xsl:template>
  
  <xsl:template name="index.list.alpha" >
	<xsl:param name="it"/>
	<xsl:if test="$it &lt;= 26">
	  <xsl:variable name="letter"><xsl:number value="$it" format="a"/></xsl:variable>
	  <xsl:if test="./div2[starts-with(normalize-space(head),$letter)]">
		<div class="alpha"><h3 id="C{$letter}"><xsl:value-of select="$letter"/></h3>
		  <ul><xsl:apply-templates select="./div2[starts-with(normalize-space(head),$letter)]" mode="toc.alpha"/></ul>
		</div>
	  </xsl:if>
	<xsl:call-template name="index.list.alpha"><xsl:with-param name="it" select="$it + 1"/></xsl:call-template>
  </xsl:if>
</xsl:template>

<!-- search keyword alphabetically -->
<xsl:template match="div2[starts-with(normalize-space(head),'Keywords')]" mode="search.keyword">
  <div class="indalpha">Find keyword: <ul><xsl:call-template name="index.key.alpha"><xsl:with-param name="it" select="1"/></xsl:call-template><li><a href="#Kother">other</a></li></ul></div>
  <xsl:call-template name="index.list.key.alpha"><xsl:with-param name="it" select="1"/></xsl:call-template>
  <xsl:call-template name="list.key.other"></xsl:call-template>
</xsl:template>

<xsl:template name="index.key.alpha" >
  <xsl:param name="it"/>
  <xsl:if test="$it &lt;= 26">
	<xsl:variable name="letter"><xsl:number value="$it" format="A"/></xsl:variable>
	<xsl:if test="./div3[starts-with(normalize-space(head),$letter)]">
	  <li><a href="#K{$letter}"><xsl:value-of select="$letter"/></a> </li>
	</xsl:if>
	<xsl:call-template name="index.key.alpha"><xsl:with-param name="it" select="$it + 1"/></xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="index.list.key.alpha" >
  <xsl:param name="it"/>
  <xsl:if test="$it &lt;= 26">
	<xsl:variable name="letter"><xsl:number value="$it" format="A"/></xsl:variable>
	<xsl:if test="./div3[starts-with(normalize-space(head),$letter)]">
	  <div class="alpha"><h3 id="K{$letter}"><xsl:value-of select="$letter"/></h3>
		<ul><xsl:apply-templates select="./div3[starts-with(normalize-space(head),$letter)]" mode="toc.alpha"/></ul>
	  </div>
	</xsl:if>
	<xsl:call-template name="index.list.key.alpha"><xsl:with-param name="it" select="$it + 1"/></xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="list.key.other" >
  <div class="alpha">
	<h3 id="Kother">other</h3>
	<ul><xsl:apply-templates select="./div3[translate(substring(normalize-space(head),1,1),'ABCDEFGHIJKLMNOPQRSTUVWXYZ','')]" mode="toc.alpha"/></ul>
  </div>
</xsl:template>

<!-- 13/11/2010 implementation with key of the global index http://www.dpawson.co.uk/xsl/sect2/N6461.html -->

<xsl:key name="indexbyletter" match="index[@level='1' and not(formula)]" use="translate(substring(normalize-space(./text()),1,1),'abcdefghijklmnopqrstuvwxyzÅÖ0123456789','ABCDEFGHIJKLMNOPQRSTUVWXYZAO9999999999')" /> 

<xsl:template match="theindex" mode="search">
  <div class="indalpha"><ul><xsl:call-template name="theindex.alpha"><xsl:with-param name="it" select="1"/></xsl:call-template><xsl:call-template name="theindex.num"/></ul></div>
  <xsl:call-template name="theindex.list.alpha"><xsl:with-param name="it" select="1"/></xsl:call-template>
  <xsl:call-template name="theindex.list.num"></xsl:call-template>
</xsl:template>

<xsl:template name="theindex.alpha" >
  <xsl:param name="it"/>
  <xsl:if test="$it &lt;= 26">
	<xsl:variable name="letter"><xsl:number value="$it" format="A"/></xsl:variable>
	<xsl:if test="key('indexbyletter', $letter)"><li><a href="#I{$letter}"><xsl:value-of select="$letter"/></a> </li>	</xsl:if>
	<xsl:call-template name="theindex.alpha"><xsl:with-param name="it" select="$it + 1"/></xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="theindex.list.alpha" >
  <xsl:param name="it"/>
  <xsl:if test="$it &lt;= 26">
	<xsl:variable name="letter"><xsl:number value="$it" format="A"/></xsl:variable>
	<xsl:if test="key('indexbyletter', $letter)">
	  <div class="alpha"><h3 id="I{$letter}"><xsl:value-of select="$letter"/></h3>
		<dl><xsl:apply-templates select="key('indexbyletter', $letter)"/></dl>
	  </div>
	</xsl:if>
	<xsl:call-template name="theindex.list.alpha"><xsl:with-param name="it" select="$it + 1"/></xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="theindex.num" >
  <xsl:if test="key('indexbyletter', '9')"><li><a href="#I9">0-9</a> </li> </xsl:if>
</xsl:template>

<xsl:template name="theindex.list.num" >
  <xsl:if test="key('indexbyletter', '9')">
	<div class="alpha"><h3 id="I9">0-9</h3>
	  <dl><xsl:apply-templates select="key('indexbyletter', '9')"/></dl>
	</div>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>

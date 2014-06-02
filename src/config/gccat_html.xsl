<?xml version="1.0" encoding="iso-8859-1"?>

<!-- 
	 Sophie Demassey/ EMN-LINA/ Global Constraint Catalog Project
	 XSLT stylesheet XML to XHTML+CSS
	 $Id: gccathtml.xsl,v 1.0 2006/04/03 16:10:59 demassey $
  -->

<xsl:stylesheet
   version="1.0"
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns="http://www.w3.org/1999/xhtml"
>
<!-- xmlns:m="http://www.w3.org/1998/Math/MathML" exclude-result-prefixes="m" -->
<!-- ??? xsl:output method="xml" version="1.0" encoding="iso-8859-1" indent="yes" omit-xml-declaration="yes" / -->

<xsl:include href="gccat_mathml_css.xsl"/>
<xsl:include href="gccat_search.xsl"/>

<!-- %%%%%%%%%%%%%%%%%%%%%%%% constants: install directory,...... %%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:variable name="projname" select="'gccat'"/>
<xsl:variable name="thedate" select="/gccat/@date"/>
<!-- xsl:variable name="directory" select="concat($projname,$thedate)"/ -->
<xsl:variable name="directory" select="$projname"/>
<xsl:variable name="projtitle" select="/gccat/titlepage/ti"/>
<xsl:variable name="googlepdf"><xsl:text>javascript: pageTracker._trackPageview('/downloads/ctrs/pdf'); </xsl:text></xsl:variable>
<xsl:variable name="googlepl"><xsl:text>javascript: pageTracker._trackPageview('/downloads/ctrs/pl'); </xsl:text></xsl:variable>
<xsl:variable name="googlepng"><xsl:text>javascript: pageTracker._trackPageview('/downloads/ctrs/png'); </xsl:text></xsl:variable>
<xsl:variable name="googlexml"><xsl:text>javascript: pageTracker._trackPageview('/downloads/ctrs/xml'); </xsl:text></xsl:variable>

<!-- %%%%%%%%%%%%%%%%%%%%%%%% common page headers, footers,... %%%%%%%%%%%%%%%%%%%%%%%%% -->
<xsl:variable name="google_analytics">
    <![CDATA[
	<script type="text/javascript">
	  var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
	  document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
	</script>
	<script type="text/javascript">
	  try {
	  var pageTracker = _gat._getTracker("UA-3373473-1");
	  pageTracker._trackPageview();
	  } catch(err) {}
	</script>
    ]]>
  </xsl:variable>

<xsl:template name="html.attributes">
  <xsl:attribute name="xml:lang">en</xsl:attribute><xsl:attribute name="lang">en</xsl:attribute>
</xsl:template>

<xsl:template name="page.head">
	<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-15" />
	<meta http-equiv="Window-target" content="_parent" />
	<meta name="author" content="Nicolas Beldiceanu, Sophie Demassey" />
	<meta http-equiv="pragma" content="no-cache"/>
	<meta name="keywords" content="beldiceanu, global constraint catalog, global constraint catalogue, global constraints, constraint programming, contraintes globales" />
	<meta name="description" content="The catalogue of global constraints: description, origin,... ; search by keywords, number of arguments,... ; reformulation in terms of graph properties or of automata" />
	<link href="../css/gccat.css" rel="stylesheet" type="text/css" />
    <link rel="stylesheet" href="../css/print.css" type="text/css" media="print" />
	<title><xsl:apply-templates select="." mode="page.headtitle"/></title>
</xsl:template>

<xsl:template name="page.header">
  <h1><a href="index.html"><xsl:value-of select="$projtitle"/></a></h1>
</xsl:template>

<xsl:template name="page.footer">
  <h5>last update: <xsl:value-of select="$thedate"/>. <a href="http://www.emn.fr/x-info/sdemasse/">SD</a>.</h5>
</xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% xhtml files  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="/gccat">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tableofcontents|titlepage|biblio|theindex|div1|div2|div3|div4">
  <xsl:variable name="pagename"><xsl:apply-templates select="." mode="page.name"/></xsl:variable>
  <xsl:document href="{$pagename}" method="xml" version="1.0" encoding="iso-8859-1" indent="yes" omit-xml-declaration="yes"
				doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN" 
				doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
	<html><xsl:call-template name="html.attributes"/><head><xsl:call-template name="page.head"/></head>
	  <body><div id="wrap">
		  <div id="header"><xsl:call-template name="page.header"/></div>
		  <div id="nav"><xsl:call-template name="toc.contents"/></div>
		  <div id="mainbody"><xsl:apply-templates select="." mode="page.mainbody"/></div>
		  <div id="footer"><xsl:call-template name="page.footer"/></div>
		</div>
		<xsl:value-of select="$google_analytics" disable-output-escaping="yes"/>
	  </body>
	</html>
  </xsl:document>
</xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% xhtml file name  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="*" mode="page.name" >
  <xsl:value-of select="$directory"/>/<xsl:apply-templates select="." mode="WebPage"/>
</xsl:template>
<xsl:template match="div2|div3|div4" mode="page.name" ><xsl:apply-templates select="." mode="WebPage"/></xsl:template>

<xsl:template match="*" mode="page.headtitle" ><xsl:value-of select="$projtitle"/></xsl:template>
<xsl:template match="biblio|theindex|div2[contains(../head,'Global Constraint Catalog')]|div3[starts-with(normalize-space(../head),'Keywords')]" mode="page.headtitle" >
  <xsl:value-of select="$projtitle"/><xsl:text>: </xsl:text><xsl:apply-templates select="." mode="sec.name"/>
</xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% xhtml file main content  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="mainmatter//*" mode="page.mainbody" >
  <div id="menu"><xsl:apply-templates select="ancestor-or-self::div1" mode="page.menu"/></div>
  <div id="content"><xsl:apply-templates select="." mode="page.main"/></div>
</xsl:template>

<xsl:template match="*" mode="page.mainbody" >
  <div id="contentall"><xsl:apply-templates select="." mode="page.main"/></div>
</xsl:template>

<xsl:template match="tableofcontents" mode="page.main" >
  <div id="main"><h1>Table of Contents</h1>	
	<div id="toc"><xsl:call-template name="toc.contents"><xsl:with-param name="subtoc" select="true()"/></xsl:call-template></div>
  </div>
</xsl:template>

<xsl:template match="titlepage" mode="page.main" >
  <div id="main"><xsl:apply-templates/></div>
  <div id="toc"><xsl:call-template name="toc.contents"/></div>
</xsl:template>

<xsl:template match="biblio" mode="page.main" >
  <div id="main"><h1>Bibliography</h1><dl id="biblio"><xsl:apply-templates/></dl></div>
</xsl:template>

<xsl:template match="theindex" mode="page.main" >
  <!-- div id="main"><h1>Index</h1><dl id="index"><xsl:apply-templates/></dl></div -->
  <div id="index"><h1>Index</h1><xsl:apply-templates select="." mode="search"/></div>
</xsl:template>

<xsl:template match="div1|div2|div3|div4" mode="page.main" >
  <xsl:apply-templates select="." mode="nav"/>
  <div id="main"><xsl:apply-templates/></div>
  <xsl:apply-templates select="." mode="page.toc"/>
</xsl:template>

<xsl:template match="div2[contains(../head,'Global Constraint Catalog')]"  mode="page.main" >
  <xsl:apply-templates select="." mode="nav"/>
  <div id="main"><xsl:apply-templates select="." mode="cnav"/><xsl:apply-templates/></div>
  <xsl:apply-templates select="." mode="page.toc"/>
</xsl:template>

<xsl:template match="div2[contains(../head,'Global Constraint Catalog')]"  mode="cnav" >
  <xsl:variable name="ctrname" select="head"/>
  <div class="download">download: <ul>
	  <li><a href="pdf/{$ctrname}.pdf" onClick="{$googlepdf}">PDF (print)</a></li>
	  <li><a href="src/{$ctrname}.pl" onClick="{$googlepl}">PROLOG (description)</a></li>
	  <li><a href="png/{$ctrname}.png" onClick="{$googlepng}">PNG (arguments)</a></li>
	  <li><a href="xml/{$ctrname}.xml" onClick="{$googlexml}">XML (example)</a></li></ul></div>
</xsl:template>

<!-- titlepage -->
<xsl:template match="titlepage/ti"></xsl:template>
<xsl:template match="titlepage/authors"><div><h5>Authors:</h5><ul><xsl:apply-templates/></ul></div></xsl:template>
<xsl:template match="titlepage/authors/author"><li><xsl:apply-templates/></li></xsl:template>
<xsl:template match="titlepage/reference"><div><h5>Reference:</h5><xsl:apply-templates/></div></xsl:template>
<xsl:template match="titlepage/abstract"><div><h5>Abstract:</h5><xsl:apply-templates/></div></xsl:template>
<xsl:template match="titlepage/keywords">
  <div><h5>Keywords:</h5><div class="keyword"><xsl:value-of select="."/></div></div>
</xsl:template>

<!-- other sections -->
<xsl:template match="div0"><xsl:apply-templates/></xsl:template>
<xsl:template match="div5"><xsl:call-template name="label-id"/><xsl:apply-templates/></xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% tables of content  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="*" mode="page.toc" ></xsl:template>
<xsl:template match="*[div2 or div3 or div4]" mode="page.toc" >
  <div id="toc"><xsl:apply-templates select="." mode="toc.section"/></div>
</xsl:template>
<xsl:template match="div1[contains(head,'Global Constraint Catalog')]" mode="page.toc" priority="1">
  <div id="toc"><xsl:apply-templates select="." mode="search.constraint"/></div>
</xsl:template>
<xsl:template match="div2[starts-with(normalize-space(head),'Keywords')]" mode="page.toc" priority="1">
  <div id="toc"><xsl:apply-templates select="." mode="search.keyword"/></div>
</xsl:template>

<xsl:template match="div1" mode="page.menu" >
  <a><xsl:attribute name="href"><xsl:apply-templates select="." mode="WebPage"/></xsl:attribute>
	<xsl:apply-templates select="head"/></a>
  <xsl:apply-templates select="." mode="toc.section"/>
</xsl:template>

<xsl:template match="div1|div2|div3|div4" mode="toc.item">
  <xsl:call-template name="calculateNumber"/>. <a><xsl:attribute name="href"><xsl:apply-templates select="." mode="WebPage"/></xsl:attribute><xsl:value-of select="head"/></a>
</xsl:template>

<xsl:template match="div1|div2|div3|div4" mode="toc.section"></xsl:template>
<xsl:template match="div1[div2]|div2[div3]|div3[div4]" mode="toc.section">
  <xsl:param name="subtoc" select="true()"/>
  <ul><xsl:for-each select="./div2|./div3|./div4">
	  <li><xsl:apply-templates select="." mode="toc.item"/>
		<xsl:if test="$subtoc"><xsl:apply-templates select="." mode="toc.section"/></xsl:if>
	  </li>
  </xsl:for-each></ul>
</xsl:template>

<xsl:template name="toc.contents">
  <xsl:param name="subtoc" select="false()"/>
  <ul>
    <li><a href="index.html">Home</a></li>
	<li><a href="titlepage.html">Title</a></li> 
	<li><a href="preface.html">Preface</a></li> 
	<li><a href="biblio.html">Bibliography</a></li> 
	<li><a href="theindex.html">Index</a></li> 
	<li><a href="content.html">Content</a></li> 
  </ul>
  <br/>
  <ul>
    <xsl:for-each select="/gccat/mainmatter/div1">
	  <li><xsl:apply-templates select="." mode="toc.item"/>
		<xsl:if test="$subtoc"><xsl:apply-templates select="." mode="toc.section"/></xsl:if>
	  </li><xsl:text> </xsl:text>
    </xsl:for-each>
  </ul>
</xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% head  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="head" >
  <xsl:call-template name="label-id"/><h4><xsl:apply-templates/></h4>
</xsl:template>
<xsl:template match="div1/head" priority="1">
  <xsl:call-template name="label-id"/><h1><xsl:call-template name="calculateNumber"/>. <xsl:apply-templates/></h1>
</xsl:template>
<xsl:template match="div2/head" priority="1">
  <xsl:call-template name="label-id"/><h2><xsl:call-template name="calculateNumber"/>. <xsl:apply-templates/></h2>
</xsl:template>
<xsl:template match="div3/head|div4/head" priority="1">
  <xsl:call-template name="label-id"/><h3><xsl:call-template name="calculateNumber"/>. <xsl:apply-templates/></h3>
</xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% section number and filename  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template name="calculateNumber">
  <xsl:number level="multiple" from="mainmatter" count="div1|div2|div3|div4|div5"/>
</xsl:template>

<!-- NEW 080917 -->
<xsl:template match="*" mode="WebPage"><xsl:apply-templates select="." mode="sec.name"/><xsl:text>.html</xsl:text></xsl:template>
<xsl:template match="*" mode="sec.name" priority="0"><xsl:text>index</xsl:text></xsl:template>

<xsl:template match="tableofcontents" mode="sec.name" ><xsl:text>content</xsl:text></xsl:template>
<xsl:template match="titlepage" mode="sec.name" ><xsl:text>titlepage</xsl:text></xsl:template>
<xsl:template match="biblio" mode="sec.name" ><xsl:text>biblio</xsl:text></xsl:template>
<xsl:template match="theindex" mode="sec.name" ><xsl:text>theindex</xsl:text></xsl:template>
<xsl:template match="//frontmatter/div1" mode="sec.name" ><xsl:text>preface</xsl:text></xsl:template>
<xsl:template match="//mainmatter/div1|div2|div3|div4" mode="sec.name" priority="0.9">
  <xsl:text>sec</xsl:text><xsl:call-template name="calculateNumber"/>
</xsl:template>
<xsl:template match="//div1//*" mode="sec.name" priority="0.5">
  <xsl:apply-templates select="ancestor-or-self::*[self::div4 or self::div3 or self::div2 or self::div1][1]" mode="sec.name"/>
</xsl:template>

<xsl:template match="div2[contains(../head,'Global Constraint Catalog')]" mode="sec.name" priority="1">
  <xsl:text>C</xsl:text><xsl:value-of select="head"/>
</xsl:template>

<xsl:template match="div3[starts-with(normalize-space(../head),'Keywords')]" mode="sec.name" priority="1">
  <xsl:variable name="label"><xsl:apply-templates select="." mode="keylabel"/></xsl:variable>
  <xsl:choose>
	<xsl:when test="normalize-space($label)"><xsl:text>K</xsl:text><xsl:value-of select="$label"/></xsl:when>
	<xsl:otherwise><xsl:text>sec</xsl:text><xsl:call-template name="calculateNumber"/></xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="div3" mode="keylabel">
  <xsl:variable name="theid"><xsl:value-of select="@id"/></xsl:variable>
  <xsl:variable name="ctrid"><xsl:value-of select=".//item[1]//ref/@target"/></xsl:variable>
  <xsl:value-of select="id($ctrid)//xref[normalize-space(./ref/@target)=$theid and @url]/@url"/>
</xsl:template>


<!-- %%%%%%%%%%%%%%%%%%%% navigation / previous / up / next  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->
<!-- NEW 090322 -->
<xsl:template match="//frontmatter/div1" mode="nav"></xsl:template>
<xsl:template match="div1|div2|div3|div4" mode="nav">
  <xsl:variable name="div" select="name()"/>
  <div id="navbar">
	<xsl:apply-templates select="preceding-sibling::*[name(current())=name(.)][1]" mode="nav.prev"/>
  <xsl:text>|</xsl:text>
	<xsl:apply-templates select="following-sibling::*[name(current())=name(.)][1]" mode="nav.next"/>
  </div>
</xsl:template>

<xsl:template match="div1|div2|div3|div4" mode="nav.prev">
  <span class="l"><a><xsl:attribute name="href"><xsl:apply-templates select="." mode="WebPage"/></xsl:attribute>&lt; <xsl:call-template name="calculateNumber"/>. <xsl:apply-templates select="." mode="head.short"/></a></span>
</xsl:template>
<xsl:template match="div1|div2|div3|div4" mode="nav.next">
  <span class="r"><a><xsl:attribute name="href"><xsl:apply-templates select="." mode="WebPage"/></xsl:attribute><xsl:call-template name="calculateNumber"/>. <xsl:apply-templates select="." mode="head.short"/> &gt;</a></span>
</xsl:template>

<xsl:template match="div1|div2|div3|div4" mode="head.short">
  <xsl:value-of select="substring(head,1,40)"/>
</xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% links, anchors and references  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template name="label-id"><xsl:if test="@id"><a name="{@id}"/></xsl:if></xsl:template>
<xsl:template match="anchor"><xsl:call-template name="label-id"/></xsl:template>
<xsl:template match="head|anchor|item|node" mode="xref"><xsl:call-template name="calculateNumber"/></xsl:template>
<xsl:template match="div1|div2|div3|div4|div5" mode="xref"><xsl:call-template name="calculateNumber"/></xsl:template>

<xsl:template match="ref">
  <a><xsl:attribute name="href"><xsl:apply-templates select="id(@target)" mode="WebPage"/><xsl:text>#</xsl:text><xsl:value-of select="@target"/></xsl:attribute>
	<xsl:apply-templates select="id(@target)" mode="xref"/></a>
</xsl:template>

<!-- le target de ref est reporte sur le xref parent... cf template xref -->
<xsl:template match="xref/ref"></xsl:template>

<xsl:template match="xref[ref]">
  <a alt="{@url}">
	<xsl:attribute name="href">
	  <xsl:apply-templates select="id(ref/@target)" mode="WebPage"/>#<xsl:value-of select="ref/@target"/>
	</xsl:attribute>
	<xsl:apply-templates/>
  </a>
</xsl:template>

<xsl:template match="xref[not(ref) and starts-with(normalize-space(@url),'xml/')]">
  <a href="{@url}" onClick="javascript: pageTracker._trackPageview('/downloads/xml'); "><xsl:apply-templates/></a>
</xsl:template>

<xsl:template match="xref[not(ref) and starts-with(normalize-space(@url),'../')]">
	  <a href="{@url}" onClick="javascript: pageTracker._trackPageview('/downloads/fig'); "><xsl:apply-templates/></a>
</xsl:template>

<xsl:template match="xref"><a href="{@url}"><xsl:apply-templates/></a></xsl:template>

<!-- xsl:template match="div3" mode="keylabel">
  <xsl:variable name="theid"><xsl:value-of select="@id"/></xsl:variable>
  <xsl:value-of select="xref[./ref/@target=$theid][1]/@url"/>
</xsl:template -->


<!-- %%%%%%%%%%%%%%%%%%%% index,  bibliography  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->
<xsl:template name="separateur.objet">
 <xsl:choose>
  <xsl:when test="position()!=last()">, </xsl:when>
  <xsl:when test="position()=last()">. </xsl:when>
 </xsl:choose>
</xsl:template>

<xsl:template match="index">
  <xsl:call-template name="label-id"/>
  <xsl:variable name="refs" select="id(@target)"/>
  <!-- div class="idx{@level}" les niveaux ne sont pas geres !!!!!!!!!!! --> 
  <dt class="{@encap}"><xsl:apply-templates/></dt>
  <dd><xsl:for-each select="id(@target)">
	  <xsl:variable name="cursec" select="ancestor-or-self::*[self::titlepage or self::div4 or self::div3 or self::div2 or self::div1][1]"/>
	  <xsl:if test="generate-id(.)=generate-id($refs[ancestor-or-self::*[self::titlepage or self::div4 or self::div3 or self::div2 or self::div1][1]=$cursec][1])">
		<a><xsl:attribute name="href"><xsl:apply-templates select="id(@id)" mode="WebPage"/><xsl:text>#</xsl:text><xsl:value-of select="./@id"/></xsl:attribute>
		  <xsl:apply-templates select="." mode="xref"/>
		</a>
		<!-- xsl:copy-of select="count($refs[ancestor-or-self::*[self::titlepage or self::div4 or self::div3 or self::div2 or self::div1][1]=$cursec])"/ -->
		<xsl:call-template name="separateur.objet"/>
	  </xsl:if>
  </xsl:for-each></dd>
</xsl:template>

<xsl:template match="cit">
  <a><xsl:attribute name="href">biblio.html#<xsl:value-of select="ref/@target"/></xsl:attribute>[<xsl:value-of select="substring-after(id(ref/@target)/@userid,':')"/>]</a>
</xsl:template>

<xsl:template match="citation">
  <xsl:call-template name="label-id"/><dt>[<xsl:value-of select="substring-after(@userid,':')"/>]</dt>
  <dd><xsl:apply-templates select="." mode="backref"/><xsl:apply-templates/></dd>
</xsl:template>
<xsl:template match="bnote|btitle|borganization|
					 bschool|byear|bmonth|bseries|bnumber|bvolume|bedition|
					 binstitution|baddress|bpages|bhowpublished|bbooktitle|
					 bpublisher|bjournal|bchapter|btype">
  <span class="{@bname}"><xsl:apply-templates/></span>
</xsl:template>

<xsl:template match="bauteurs|bediteur">
  <span class="{@bname}">
	<xsl:for-each select="bpers">
      <xsl:value-of select="@prenom"/><xsl:text> </xsl:text><xsl:value-of select="@nom"/><xsl:text>, </xsl:text>
	</xsl:for-each>
  </span>
</xsl:template>

<xsl:template match="citation" mode="backref">
  <xsl:variable name="citid" select="@id"/>
  <span class="ref"><xsl:apply-templates select="//cit/ref[@target=$citid]" mode="backref"/></span>
</xsl:template>
<xsl:template match="cit/ref" mode="backref">
  <a><xsl:attribute name="href"><xsl:apply-templates select="." mode="WebPage"/></xsl:attribute><xsl:apply-templates select="." mode="sec.name"/></a><xsl:text>, </xsl:text>
</xsl:template>


<!-- %%%%%%%%%%%%%%%%%%%% formula  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="simplemath"><xsl:apply-templates/></xsl:template>
<!-- dec 2009: new att in tralics: textype utilisable ? -->
<xsl:template match="formula[@type='display' and @id]">
	  <xsl:call-template name="label-id"/>
      <div class="formulanumbered">
		<div class="formula"><xsl:apply-templates/></div>
		<div class="number">(<xsl:call-template name="calculateFormulaNumber"/>)</div>
      </div>
</xsl:template>

<xsl:template match="formula[@type='display']">
  <div class="formula"><xsl:apply-templates/></div>
</xsl:template>

<xsl:template match="formula">
  <span class="math"><xsl:apply-templates/></span>
</xsl:template>

<xsl:template match="formula" mode="xref"><xsl:call-template name="calculateFormulaNumber"/></xsl:template>
<xsl:template name="calculateFormulaNumber"><xsl:number level="any" count="formula[@id]"/></xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% figure  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<!-- dec 2009: new atts in tralics: starred, extension, place... utilisables ? -->
<xsl:template match="figure">
  <xsl:call-template name="label-id"/>
  <div class="figure"><xsl:apply-templates select="head"/><xsl:call-template name="generate-graphics"/></div>
</xsl:template>
<xsl:template match="figure[@rend='inline']"><xsl:call-template name="generate-graphics"/></xsl:template>
<xsl:template match="figure[@rend='array']">
  <xsl:call-template name="label-id"/><div class="figure"><xsl:apply-templates/></div>
</xsl:template>

<xsl:template name="generate-graphics">
  <img alt="{@file}"><xsl:attribute name="src"><xsl:text>../</xsl:text><xsl:value-of select="@file"/>.png</xsl:attribute></img>
  <!-- xsl:if test="@width"><xsl:attribute name="width"><xsl:value-of select="@width"/></xsl:attribute></xsl:if -->
</xsl:template>

<xsl:template match="figure" mode="xref"><xsl:call-template name="calculateFigureNumber"/></xsl:template>
<xsl:template match="figure/head" priority="1">
  <h5><b>Figure <xsl:call-template name="calculateFigureNumber"/>. </b><xsl:apply-templates/></h5>
</xsl:template>
<xsl:template name="calculateFigureNumber">
  <xsl:number count="div1|div2" level="multiple" format="1.1."/>
  <xsl:number count="figure[@rend!='inline']" level="any" from="div2" format="1"/>
</xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% table  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="table">
  <xsl:call-template name="label-id"/><div class="table"><table align="center"><xsl:call-template name="generate-table"/></table></div>
</xsl:template>
<xsl:template match="table[@rend='inline']"><table><xsl:call-template name="generate-table"/></table></xsl:template>

<xsl:template name="generate-table">
  <xsl:if test="head"><caption><xsl:apply-templates select="head"/></caption></xsl:if>
  <xsl:for-each select="row">
	<tr><xsl:if test="@bottom-border='true'"><xsl:attribute name="class">border</xsl:attribute></xsl:if>
	  <xsl:for-each select="cell">
		<td><xsl:if test="@halign"><xsl:attribute name="align"><xsl:value-of select="@halign"/></xsl:attribute></xsl:if>
		  <xsl:if test="@right-border='true'"><xsl:attribute name="class">border</xsl:attribute></xsl:if>
		  <xsl:apply-templates/></td>
	  </xsl:for-each>
  </tr></xsl:for-each>
</xsl:template>

<xsl:template match="table" mode="xref"><xsl:call-template name="calculateTableNumber"/></xsl:template>
<xsl:template match="table/head" priority="1">
  <h5><b>Table <xsl:call-template name="calculateTableNumber"/>. </b><xsl:apply-templates/></h5>
</xsl:template>
<xsl:template name="calculateTableNumber">
  <xsl:number count="div1|div2" level="multiple" format="1.1."/>
  <xsl:number count="figure[@rend!='inline']" level="any" from="div2" format="1"/>
</xsl:template>
<!-- %%%%%%%%%%%%%%%%%%%% theorem  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="theorem">
  <xsl:call-template name="label-id"/><div class="theorem"><xsl:apply-templates/></div>
</xsl:template>

<xsl:template match="theorem" mode="xref">
  <xsl:variable name="title"><xsl:value-of select="./p/hi"/></xsl:variable>
  <xsl:choose>
	<xsl:when test="starts-with($title,normalize-space('Proposition'))"><xsl:value-of select="normalize-space(substring-after($title,'Proposition'))"/></xsl:when>
	<xsl:when test="starts-with($title,normalize-space('Proof'))"><xsl:value-of select="normalize-space(substring-after($title,'Proof'))"/></xsl:when>
	<xsl:when test="normalize-space($title)=''">&#x40;</xsl:when>
	<xsl:otherwise><xsl:value-of select="normalize-space($title)"/></xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% list  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="list[@type='simple' and starts-with(ancestor::div1/head,normalize-space('Legend'))]">
  <ul class="legend"><xsl:apply-templates/></ul>
</xsl:template>
<xsl:template match="list[@type='ordered']"><ol><xsl:apply-templates/></ol></xsl:template>
<xsl:template match="list[@type='description']"><dl><xsl:apply-templates/></dl></xsl:template>
<xsl:template match="list"><ul><xsl:apply-templates/></ul></xsl:template>

<xsl:template match="list/label"><a name="{following::item/@id}"/><dt><xsl:apply-templates/></dt></xsl:template>
<xsl:template match="label"><xsl:apply-templates/></xsl:template>

<xsl:template match="item[../@type = 'description']"><dd><xsl:apply-templates/></dd></xsl:template>
<!-- dec 2009: new att in tralics: label utilisable ? -->
<xsl:template match="item"><xsl:call-template name="label-id"/><li><xsl:apply-templates/></li></xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% paragraphs  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="note"><span class="note"><xsl:apply-templates/></span></xsl:template>
<xsl:template match="fbox"><div class="fbox"><xsl:apply-templates/></div></xsl:template>
<xsl:template match="minipage"><div class="minipage"><xsl:apply-templates/></div></xsl:template>
<xsl:template match="p[@rend='centered']"><p class="center"><xsl:apply-templates/></p></xsl:template>
<xsl:template match="p"><p><xsl:apply-templates/></p></xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% fonts  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="hi">
  <xsl:choose>
	<xsl:when test="not(normalize-space(.))"><br/></xsl:when> <!-- for the electronic catalog only ? bad -->
	<xsl:when test="@rend = 'it'"><i><xsl:apply-templates/></i></xsl:when>
	<xsl:when test="@rend = 'sup'"><sup><xsl:apply-templates/></sup></xsl:when>
	<xsl:when test="@rend = 'bold'"><b><xsl:apply-templates/></b></xsl:when>
	<xsl:when test="@rend = 'slanted'"><i><xsl:apply-templates/></i></xsl:when>
	<xsl:when test="@rend = 'emph'"><i><xsl:apply-templates/></i></xsl:when>
	<xsl:when test="@rend = 'tt'"><tt><xsl:apply-templates/></tt></xsl:when>
	<xsl:otherwise>
	  <span class="{@rend}"><xsl:apply-templates/></span>
	</xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- %%%%%%%%%%%%%%%%%%%% divers  %%%%%%%%%%%%%%%%%%%%%%%%%%%% -->

<xsl:template match="LaTeX"><xsl:text>LaTeX</xsl:text></xsl:template>
<xsl:template match="rule"><hr/></xsl:template>
<xsl:template match="zws"/>
<xsl:template match="frontmatter"><xsl:apply-templates /></xsl:template>
<xsl:template match="backmatter"><xsl:apply-templates /></xsl:template>
<xsl:template match="mainmatter"><xsl:apply-templates /></xsl:template>
<xsl:template match="mbox"><xsl:apply-templates /></xsl:template>
<xsl:template match="colorpool"><xsl:apply-templates /></xsl:template>
<xsl:template match="color"><xsl:apply-templates /></xsl:template>
<xsl:template match="line"><xsl:apply-templates /></xsl:template>
<xsl:template match="newpage"><xsl:apply-templates /></xsl:template>

<xsl:template match="*">
 <xsl:comment>PASS THROUGH <xsl:value-of select="name()"/></xsl:comment>
 <xsl:apply-templates/>
</xsl:template>

</xsl:stylesheet>

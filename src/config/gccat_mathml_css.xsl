<?xml version="1.0" encoding="iso-8859-1"?>

<!-- 
	 Sophie Demassey/ EMN-LINA/ Global Constraint Catalog Project
	 XSLT stylesheet XML to XHTML+CSS
	 $Id: gccat_mathml_css.xsl,v 1.0 2006/04/03 16:10:59 demassey $
	 Adapted from:
	 pmathmlcss.xsl,v 1.1 2002/03/20 12:20:57 mf Exp $
	 Copyright David Carlisle 2001, 2002.
	 Use and distribution of this code are permitted under the terms of the <a
	 href="http://www.w3.org/Consortium/Legal/copyright-software-19980720"
	 >W3C Software Notice and License</a>.
-->

<xsl:stylesheet
   version="1.0"
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:h="http://www.w3.org/1999/xhtml"
   xmlns:m="http://www.w3.org/1998/Math/MathML"
   xmlns:doc="http://www.dcarlisle.demon.co.uk/xsldoc"
   xmlns:x="data:,x"
   exclude-result-prefixes="x h doc m"
   >

  <h:h2>Dictionary</h:h2>

<h:p> The following elements in the x: namespace form an
implementation of an "Operator Dictionary" for this MathML
Implementation. In the case of stretch operators, the element
specifies the symbol parts via the latin-1 equivalent character based
on the encoding in the symbol font.  It is a clear "failure to comply
to the spec" that using latin 1 characters (or numeric character
references) in the latin 1 range access glyphs in teh symbol font via
font position, however most of these character parts are not in
Unicode (until 3.2), so there is no standard way to access these characters.</h:p>

<x:x x="{" m="0em"  stretch="true" top="ì" middle="í" extend="ï" bottom="î">{</x:x>
<x:x x="}" m="0em"  stretch="true" top="ü" middle="ý" extend="ú" bottom="þ">}</x:x>

<x:x x="(" m="0em"  stretch="true" top="æ" middle="ç" extend="ç" bottom="è">(</x:x>
<x:x x=")" m="0em"  stretch="true" top="ö" middle="÷" extend="÷" bottom="ø">)</x:x>

<x:x x="[" m="0em"  stretch="true" top="é" middle="ê" extend="ê" bottom="ë">[</x:x>
<x:x x="]" m="0em"  stretch="true" top="ù" middle="ú" extend="ú" bottom="û">]</x:x>

<x:x x="&#x301A;" m="0em"  stretch="true" top="éé" middle="êê" extend="êê" bottom="ëë">[[</x:x>
<x:x x="&#x301B;" m="0em"  stretch="true" top="ùù" middle="úú" extend="úú" bottom="ûû">]]</x:x>

<x:x x="|" m="0em"  stretch="true" top="ç" middle="ç" extend="ç" bottom="ç">|</x:x>
<x:x x="||" m="0em"  stretch="true" top="çç" middle="çç" extend="çç" bottom="çç">||</x:x>

<x:x x="&#x2061;" m="0em">&#xFEFF;</x:x><!--  applyfunction -->
<x:x x="&#x2062;" m="0em">&#xFEFF;</x:x><!--  invisibletimes -->
<x:x x="-">&#x2212;</x:x>
<x:x x="&#x2243;"><span style="position: relative;  top: +.1em;">&#x2212;</span>&#xFEFF;<span style="position: relative; left: -.55em; top: -.2em; margin: 0em;">~</span></x:x>
<x:x x="&#xFE38;" m="0em">_v_</x:x>

<x:x x="&#x2133;" v="script"><span class="script">M</span></x:x>
<x:x x="&#x1d49c;" v="script"><span class="script">A</span></x:x>
<x:x x="&#x1d49d;" v="script"><span class="script">B</span></x:x>
<x:x x="&#x1d49e;" v="script"><span class="script">C</span></x:x>
<x:x x="&#x1d49f;" v="script"><span class="script">D</span></x:x>
<x:x x="&#x1d4a0;" v="script"><span class="script">E</span></x:x>
<x:x x="&#x1d4a1;" v="script"><span class="script">F</span></x:x>
<x:x x="&#x1d4a2;" v="script"><span class="script">G</span></x:x>
<x:x x="&#x1d4a3;" v="script"><span class="script">H</span></x:x>
<x:x x="&#x1d4a4;" v="script"><span class="script">I</span></x:x>
<x:x x="&#x1d4a5;" v="script"><span class="script">J</span></x:x>
<x:x x="&#x1d4a6;" v="script"><span class="script">K</span></x:x>
<x:x x="&#x1d4a7;" v="script"><span class="script">L</span></x:x>
<x:x x="&#x1d4a8;" v="script"><span class="script">M</span></x:x>
<x:x x="&#x1d4a9;" v="script"><span class="script">N</span></x:x>
<x:x x="&#x1d4aa;" v="script"><span class="script">O</span></x:x>
<x:x x="&#x1d4ab;" v="script"><span class="script">P</span></x:x>
<x:x x="&#x1d4ac;" v="script"><span class="script">Q</span></x:x>
<x:x x="&#x1d4ad;" v="script"><span class="script">R</span></x:x>
<x:x x="&#x1d4ae;" v="script"><span class="script">S</span></x:x>
<x:x x="&#x1d4af;" v="script"><span class="script">T</span></x:x>
<x:x x="&#x1d4b0;" v="script"><span class="script">U</span></x:x>
<x:x x="&#x1d4b1;" v="script"><span class="script">V</span></x:x>
<x:x x="&#x1d4b2;" v="script"><span class="script">W</span></x:x>
<x:x x="&#x1d4b3;" v="script"><span class="script">X</span></x:x>
<x:x x="&#x1d4b4;" v="script"><span class="script">Y</span></x:x>
<x:x x="&#x1d4b5;" v="script"><span class="script">Z</span></x:x>
<x:x x="&#x1d4b6;" v="script"><span class="script">a</span></x:x>


<h:p>Grab all of the above into a variable.</h:p>
<xsl:variable name="opdict" select="document('')/*/x:x"/>

<h:h2>HTML elements</h:h2>

<h:p>XHTML elements get passed straight through, sans namespace prefix.</h:p>
<xsl:template match="h:*">
  <xsl:element name="{local-name(.)}">
	<xsl:copy-of select="@*"/>
	<xsl:apply-templates/>
  </xsl:element>
</xsl:template>

<xsl:template match="h:head">
  <xsl:element name="{local-name(.)}">
	<xsl:copy-of select="@*"/>
	<xsl:apply-templates/>
	<style>
	  <xsl:text doc:id="css">
		.msubsup { font-size: 80%; position: absolute; }
		.munderover { display: inline; vertical-align: middle; }
		.lr { display: inline; vertical-align: middle; }
		.mi { font-style: serif; }
		.mspace{ display: inline; }
		.mtext { font-style: serif; }
		.ms { font-style: monospace; }
		.mi1 { font-style: italic; }
		.doublestruck { font-family:  castellar, algerian,niagara engraved; }
		.mo { padding-right: .3em; padding-left: .3em; }
		.mn { }
		.msqrt { border-style: solid; border-color: black; border-width: .1em 0pt 0pt .1em; padding-left: .2em; margin-left: 0em; margin-top: .2em; display: inline; }
		.actuarial { border-style: solid; border-color: black; border-width: .1em .1em 0pt 0pt ; padding-right: .2em; margin-right: 0em; margin-top: .2em; display: inline; }
		.ssa {  position:relative; top:+0.5ex; width: 0pt; color: red; }
		.mover { margin: 0pt; padding: 0pt; display: inline; vertical-align: middle; text-align: center; }
		.mtable { display: inline; vertical-align: middle; }
		.mfrac { text-align: center; display:inline; vertical-align: middle; }
		.mfraca { vertical-align: bottom; }
		.mfracaa { border-width: 0em 0em .2ex 0em ; border-style: solid; border-color: black; }
		.mfracb { vertical-align: top; }
		.merror{ background-color: white  ; border-style: solid; border-color: #FF0000; color: #FF0000; }
		.mphantom{ visibility: hidden; }
	  </xsl:text>
	</style>
  </xsl:element>
</xsl:template>

<h:p>Unimplemented MathML elements get copied literally, in red, mainly as a debugging aid. </h:p>
<xsl:template match="m:*">
  <span class="error">&lt;<xsl:value-of select="local-name(.)"/>&gt;</span>
  <xsl:apply-templates/>
  <span class="error">&lt;/<xsl:value-of select="local-name(.)"/>&gt;</span>
</xsl:template>

<h:p>
mi: set default font based on string length, otherwise behaviour based
on entries in the operator dictionary if one exists, or content is
copied through to the output unchanged.
</h:p>
<xsl:template match="m:mi">
  <span class="mi">
	<!--xsl:if test="1=string-length(normalize-space(.))"><xsl:attribute name="class">mi1</xsl:attribute></xsl:if -->
	<xsl:apply-templates select="@mathvariant"/>
	<xsl:variable name="x"  select="normalize-space(.)"/>
	<xsl:choose>
	  <xsl:when test="$opdict[@x=$x and @v]">
		<xsl:attribute name="class"><xsl:value-of select="$opdict[@x=$x]/@v"/></xsl:attribute>
		<xsl:value-of select="$opdict[@x=$x and @v]"/>
	  </xsl:when>
	  <xsl:otherwise><xsl:value-of select="$x"/></xsl:otherwise>
	</xsl:choose>
  </span>
</xsl:template>

<xsl:template match="@mathvariant[.='monospace']"></xsl:template>
<!-- xsl:template match="@mathvariant[.='monospace']"><xsl:attribute name="class">monospace</xsl:attribute></xsl:template -->
<xsl:template match="@mathvariant[.='bold']"><xsl:attribute name="class">bold</xsl:attribute></xsl:template>
<xsl:template match="@mathvariant[.='bold-italic']"><xsl:attribute name="class">italicbold</xsl:attribute></xsl:template>
<xsl:template match="@mathvariant[.='italic']"><xsl:attribute name="class">italic</xsl:attribute></xsl:template>
<xsl:template match="@mathvariant[.='sans-serif']"><xsl:attribute name="class">sansserif</xsl:attribute></xsl:template>
<xsl:template match="@mathvariant[.='bold-sans-serif']"><xsl:attribute name="class">sansserifbold</xsl:attribute></xsl:template>
<xsl:template match="@mathvariant[.='fraktur']"><xsl:attribute name="class">fraktur</xsl:attribute></xsl:template>
<xsl:template match="@mathvariant[.='double-struck']"><xsl:attribute name="class">doublestruck</xsl:attribute></xsl:template>
<xsl:template match="@mathvariant[.='script']"><xsl:attribute name="class">script</xsl:attribute></xsl:template>


<xsl:template match="m:mo">
  <xsl:if test="normalize-space(.)='[' or normalize-space(.)='(' or normalize-space(.)='{'  or normalize-space(.)='&lt;'">
	<xsl:text>&#x200B;</xsl:text>
  </xsl:if>
  <span class="mo"><xsl:apply-templates/></span>
  <xsl:if test="normalize-space(.)=',' or normalize-space(.)=']' or normalize-space(.)=')' or normalize-space(.)='}'  or normalize-space(.)='&gt;'">
	<xsl:text>&#x200B;</xsl:text>
  </xsl:if>
</xsl:template>
<xsl:template match="m:mn"><span class="mn"><xsl:apply-templates/></span></xsl:template>

<h:p>munder: currently only supports underline, with a bottom border</h:p>
<xsl:template match="m:munder">
  <span>
	<xsl:choose>
	  <xsl:when test="normalize-space(*[2])='&#x332;'"><xsl:attribute name="class">munderbar</xsl:attribute></xsl:when>
	  <xsl:when test="normalize-space(*[2])='&#x2192;'"><xsl:attribute name="class">munderrightarrow</xsl:attribute></xsl:when>
	  <xsl:otherwise><xsl:attribute name="class">munder</xsl:attribute></xsl:otherwise>
	</xsl:choose>
	<xsl:apply-templates select="*[1]"/>
  </span>
</xsl:template>

<h:p>mover: currently only supports overline, with a top border</h:p>
<xsl:template match="m:mover">
  <span>
	<xsl:choose>
	  <xsl:when test="normalize-space(*[2])='&#xAF;'"><xsl:attribute name="class">moverbar</xsl:attribute></xsl:when>
	  <xsl:when test="normalize-space(*[2])='&#x2192;'"><xsl:attribute name="class">moverrightarrow</xsl:attribute></xsl:when>
	  <xsl:otherwise><xsl:attribute name="class">mover</xsl:attribute></xsl:otherwise>
	</xsl:choose>
	<xsl:apply-templates select="*[1]"/>
  </span>
</xsl:template>

<xsl:template match="m:munderover">
  <table class="munderover">
	<tr><td><xsl:apply-templates select="*[3]"/></td></tr>
	<tr><td><xsl:apply-templates select="*[1]"/></td></tr>
	<tr><td><xsl:apply-templates select="*[2]"/></td></tr>
  </table>
</xsl:template>

<xsl:template match="m:mtext"><span class="mtext"><xsl:value-of select="normalize-space(.)"/></span></xsl:template>

<h:p>mstyle: not many attributes currently supported</h:p>
<xsl:template match="m:mstyle">
  <span>
	<xsl:attribute name="style">
	  <xsl:if test="@color">color: <xsl:value-of select="@color"/>; </xsl:if>
	  <xsl:if test="@background">background-color: <xsl:value-of select="@background"/>; </xsl:if>
	</xsl:attribute>
	<xsl:apply-templates/>
  </span>
</xsl:template>

<h:p>mglyph: Uses disable output escaping to construct a numeric character reference. Uses IE's non conforming behaviour of using this number to access the font encoding rather than unicode.</h:p>
<xsl:template match="m:mglyph">
  <font face="{@fontfamily}"><xsl:value-of disable-output-escaping="yes" select="'&amp;#'"/>
	<xsl:value-of select="@index"/>;<xsl:text/>
  </font>
</xsl:template>

<h:p>ms: a simple span with left and right character added to the content.</h:p>
<xsl:template match="m:ms">
  <span class="ms">
	<xsl:value-of select="@lquote"/><xsl:value-of select="normalize-space(.)"/><xsl:value-of select="@rquote"/>
  </span>
</xsl:template>

<xsl:template match="m:mfenced">
  <xsl:variable name="l">
	<xsl:choose>
	  <xsl:when test="@open"><xsl:value-of select="@open"/></xsl:when>
	  <xsl:otherwise>(</xsl:otherwise>
	</xsl:choose>
  </xsl:variable>
  <xsl:variable name="r">
	<xsl:choose>
	  <xsl:when test="@close"><xsl:value-of select="@close"/></xsl:when>
	  <xsl:otherwise>)</xsl:otherwise>
	</xsl:choose>
  </xsl:variable>
  <xsl:variable name="nrow" select="concat('op',count(descendant::m:mtr))"/>
  <span><xsl:attribute name="class"><xsl:value-of select="$nrow"/></xsl:attribute><xsl:value-of select="$l"/></span>
  <span class="fenced">
	<xsl:for-each select="*">
	  <xsl:apply-templates select="."/>
	</xsl:for-each>
  </span>
  <span><xsl:attribute name="class"><xsl:value-of select="$nrow"/></xsl:attribute><xsl:value-of select="$r"/></span>
</xsl:template>

<xsl:template match="m:mmultiscripts">
  <table style="display:inline; vertical-align: middle;">
	<tr>
	  <xsl:for-each select="*[preceding-sibling::m:mprescripts and position() mod 2 = 0]">
		<td><xsl:apply-templates select="."/></td>
	  </xsl:for-each>
	  <td rowspan="2"><xsl:apply-templates select="*[1]"/></td>
	  <xsl:for-each select="*[not(preceding-sibling::m:mprescripts) and position() !=1 and position() mod 2 = 1]">
		<td><xsl:apply-templates select="."/></td>
	  </xsl:for-each>
	</tr>
	<tr>
	  <xsl:for-each select="*[preceding-sibling::m:mprescripts and position() mod 2 = 1]">
		<td><xsl:apply-templates select="."/></td>
	  </xsl:for-each>
	  <xsl:for-each select="*[not(preceding-sibling::m:mprescripts) and not(self::m:mprescripts) and position() mod 2 = 0]">
		<td><xsl:apply-templates select="."/></td>
	  </xsl:for-each>
	</tr>
  </table>
</xsl:template>

<xsl:template match="m:math"><xsl:call-template name="mrow"/></xsl:template>
<xsl:template name="mrow"><span class="mrow"><xsl:apply-templates select="*"/></span></xsl:template>
<xsl:template match="m:mrow"><xsl:apply-templates select="*"/></xsl:template>
<xsl:template match="m:merror"><span class="merror"><xsl:apply-templates/></span></xsl:template>
<xsl:template match="m:mphantom"><span class="mphantom"><xsl:apply-templates/></span></xsl:template>
<xsl:template match="m:none">&#xFEFF;</xsl:template>

<xsl:template match="m:msubsup">
  <span><xsl:apply-templates select="*[1]"/></span>
  <span class="msub"><xsl:apply-templates select="*[2]"/></span>
  <span class="msup"><xsl:apply-templates select="*[3]"/></span>
  <span>&#xFEFF;</span>
</xsl:template>

<xsl:template match="h:table//m:msubsup|m:mtable//m:msubsup|m:msubsup" priority="2">
  <span><xsl:apply-templates select="*[1]"/></span>
  <sub><xsl:apply-templates select="*[2]"/></sub>
  <sup><xsl:apply-templates select="*[3]"/></sup>
</xsl:template>

<xsl:template match="m:msup">
  <span ><xsl:apply-templates select="*[1]"/></span>
  <span class="msup"><xsl:apply-templates select="*[2]"/></span>
  <span >&#xFEFF;</span>
</xsl:template>

<xsl:template match="h:table//m:msup|m:mtable//m:msup|m:msup" priority="2">
	<span><xsl:apply-templates select="*[1]"/></span>
	<xsl:choose>
	  <!-- xsl:when test="*[2]/mo[normalize-space(.)='&#x0289;']"><xsl:apply-templates select="*[2]"/></xsl:when -->
	  <xsl:when test="*[2][self::m:mo and normalize-space(.)='&#x0289;']"><xsl:apply-templates select="*[2]"/></xsl:when>
	  <xsl:when test="count(*[2][self::m:mrow]/*)=2 and count(*[2][self::m:mrow]/m:mo[normalize-space(.)='&#x0289;'])=2"><span class="mo">&#x02ba;</span></xsl:when>
	  <xsl:otherwise><sup><xsl:apply-templates select="*[2]"/></sup></xsl:otherwise>
	</xsl:choose>


</xsl:template>

<xsl:template match="m:msub">
  <span><xsl:apply-templates select="*[1]"/></span>
  <span class="msub"><xsl:apply-templates select="*[2]"/></span>
  <span>&#xFEFF;</span>
</xsl:template>

<xsl:template match="h:table//m:msub|m:mtable//m:msub|m:msub" priority="2">
  <span><xsl:apply-templates select="*[1]"/></span><sub><xsl:apply-templates select="*[2]"/></sub>
</xsl:template>


<xsl:template match="m:*/text()" name="text">
  <xsl:param name="x" select="normalize-space(.)"/>
  <xsl:variable name="mo"  select="document('')/*/x:x[@x=$x]"/>
  <xsl:choose>
	<xsl:when test="$mo"><xsl:copy-of select="$mo/node()"/></xsl:when>
	<xsl:otherwise><xsl:copy-of select="$x"/></xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="m:msqrt">
  <span class="msqrtx">\&#xFEFF;</span><span class="msqrt"><xsl:apply-templates/></span>
</xsl:template>
<xsl:template match="m:menclose[@notation='radical']">
  <span class="msqrtx">\&#xFEFF;</span><span class="msqrt"><xsl:apply-templates/></span>
</xsl:template>
<xsl:template match="m:menclose[@notation='actuarial']">
  <span class="actuarial"><xsl:apply-templates/></span>
</xsl:template>
<xsl:template match="m:menclose"><span class="msqrt"><xsl:apply-templates/></span></xsl:template>
<xsl:template match="m:mroot">
  <span class="msqrtx"><sup><xsl:apply-templates select="*[2]"/></sup>\&#xFEFF;</span><span class="msqrt">
	<xsl:apply-templates select="*[1]"/>
  </span>
</xsl:template>


<xsl:template match="m:mfrac">
  <table class="mfrac">
	<tr class="mfraca"><td class="mfracaa"><xsl:apply-templates select="*[1]"/></td></tr>
	<tr class="mfracb"><td><xsl:apply-templates select="*[2]"/></td></tr>
  </table>
</xsl:template>

<xsl:template match="m:padded"><span class="display"><xsl:apply-templates/></span></xsl:template>
<xsl:template match="m:mspace"><span class="mo">&#x2006;</span></xsl:template>

<xsl:template match="m:mtable"><table class="mtable"><xsl:apply-templates/></table></xsl:template>
<xsl:template match="m:mtr"><tr><xsl:apply-templates/></tr></xsl:template>
<xsl:template match="m:mtd"><td><xsl:apply-templates/></td></xsl:template>

<!-- xsl:template match="m:maligngroup">
  <xsl:variable name="g">
	<xsl:choose>
	  <xsl:when test="@groupalign"></xsl:when>
	  <xsl:when test="ancestor::td/@groupalign"></xsl:when>
	  <xsl:when test="ancestor::tr/@groupalign"></xsl:when>
	  <xsl:when test="ancestor::table/@groupalign"></xsl:when>
	  <xsl:otherwise>left</xsl:otherwise></xsl:choose>
  </xsl:variable>
  <span class="aligngroupleft">&#xFEFF;</span>
</xsl:template -->


</xsl:stylesheet>

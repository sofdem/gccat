<?xml version="1.0"?>
<xsl:stylesheet
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:xsd="http://www.w3.org/2001/XMLSchema"
   xmlns="http://www.w3.org/1999/xhtml"
   version="1.0">
  
  <xsl:output method="html" />
  <xsl:variable name="sys" select="document('gccat_systems.xml')"/>  
  <xsl:template match="/">
	<html>
	  <head>
		<title>Global Constraint Catalog: XML Schema</title>
		<style>
		  body { margin: 0px;	padding: 0px; overflow: hidden;	font-family: Georgia;}
		  code { font-family: Courier; }
		  h5 { margin:100px; padding:0; font-weight:normal; font-size:70%; font-style:italic; text-align:right; }
		  a { text-decoration: none; }
		  a:hover { text-decoration: underline; }
		  #indices { position: absolute; left: 0px; width: 200px;	top: 0px;
		  bottom: 0px; height: expression(offsetParent.offsetHeight);	/* only IE uses this */
		  overflow: auto; background: #EEEEEE }
		  #contents {	position: absolute;	left: 210px; top: 0px;
		  right:0px; width: expression(offsetParent.offsetWidth-210);	/* only IE uses this */
		  bottom: 0px; height: expression(offsetParent.offsetHeight);	/* only IE uses this */
		  overflow: auto;	}
		  .val { font-family: Courier; color: green; }
		  .attr { font-family: Courier; font-weight: bold; }
		  .comp {}
		  .layout {}
		  .format {}
		  .note {}
		  .missing { color: lightgray; }
		  .heading { padding: 10px 5px 2px 5px; color: gray; font-weight: bold; font-size: 60%; }
		  h2 { color: #333333; font-weight: bold; font-size: 130%; }
		  .text {	padding-left: 20px;	padding-bottom: 10px; padding-right: 10px; }
		  .args {	padding: 2px 10px 2px 50px; }
		  .index { padding:2px 5px 5px 2px; display: block; font-size: 80%; color: blue; }
		  .index_selected { background: #8888FF; color: white; }
		  .content { display: none; }
		  .content_selected {	display: block; }
		</style>
		
		<script>
		  <xsl:text disable-output-escaping="yes">
			var lastSelected = null;
			function addClass (element, newClass) {
				if (element) {
					var classes = element.className.split (" ");
					for (var i = 0; i &lt; classes.length; ++i)
					if (classes [i] == newClass) break;
					if (i == classes.length) {
						classes.push (newClass);
						element.className = classes.join (" ");
					}
				}
			}
			function removeClass (element, oldClass) {
				if (element) {
					var classes = element.className.split (" ");
					for (var i = 0; i &lt; classes.length; ++i)
					if (classes [i] == oldClass) {
						classes.splice (i, 1);
						element.className = classes.join (" ");
						break;
					}
				}
			}
			function select (selected) {
				if (selected != lastSelected) {
					if (lastSelected) { 		// deselect the last selection
						removeClass (document.getElementById ("index_" + lastSelected.replace (':', '_')), "index_selected");
						removeClass (document.getElementById ("content_" + lastSelected.replace (':', '_')), "content_selected");
					}
					lastSelected = selected;
					var selectedIndex = document.getElementById ("index_" + lastSelected.replace (':', '_'));
					addClass (selectedIndex, "index_selected");
					if (selectedIndex) { // ensure selected index is visible in indices source list
						var indexTop = selectedIndex.offsetTop;
						var indexBottom = indexTop + selectedIndex.offsetHeight;
						var indicesTop = selectedIndex.offsetParent.scrollTop;
						var indicesBottom = indicesTop + selectedIndex.offsetParent.offsetHeight;
						if (indexBottom &lt; indicesTop || indexTop &gt; indicesBottom)
							selectedIndex.scrollIntoView ();
					}
					// display the content associated with the selected index
					addClass (document.getElementById ("content_" + lastSelected.replace (':', '_')), "content_selected");
				}
				return true;
			}
			
			function hashed () {
			// check if we navigated to a new internal location e.g. via the back button, if so we need to update the selection
			// NOTE: this means there are no real anchors in the HTML at all, we're just simulating them
				var hash = window.location.hash;
				if (hash &amp;&amp; hash.charAt (0) == '#') 
					select (hash.substr (1));
				else select ("h:Introduction");
				return true;
			}
			function clicked (event) {
			// check to see if an internal link was clicked, if so we need to update the selection
			// NOTE: this is not strictly necessary since hashed () will catch it but it helps with the responsiveness
				var	clickedElement = event.target;
				if (!clickedElement) clickedElement = event.srcElement;
				switch (clickedElement.tagName)	{
					case 'a':
					case 'A':
						var oldHref = window.location.href;
						var oldHash = oldHref.indexOf ('#');
						if (oldHash == -1) oldHash = oldHref.length;
						var newHref = clickedElement.href;
						var newHash = newHref.indexOf ('#');
						if (newHash == -1) newHash = newHref.length;
						if (oldHref.substr (0, oldHash) == newHref.substr (0, newHash)) {
							if (newHash &lt; newHref.length) select (newHref.substr (newHash + 1));
							else select ("h:Introduction");
						}
						break;
				}
				return true;
			}
			function loaded () {
				hashed ();
				window.setInterval ("hashed ()", 1000);
				return true;
			}
		  </xsl:text>
		</script>
	  </head>
	  <body onload="return loaded ()" onclick="return clicked (event)">
  		<div id="indices">
		  <div class="heading">NOTES</div><xsl:call-template name="Introduction.menu"/><xsl:apply-templates select="/xsd:schema/xsd:annotation[@id]" mode="menu"/>
		  <div class="heading">CONSTRAINTS</div><xsl:apply-templates select="//xsd:element[@name='constraints']/xsd:complexType[1]/xsd:choice[1]/xsd:element" mode="menu"/>
		  <div class="heading">TYPES</div><xsl:apply-templates select="/xsd:schema/xsd:simpleType" mode="menu"/>
		</div>

		<div id="contents">
		  <xsl:call-template name="Introduction.content"/>
		  <xsl:apply-templates select="/xsd:schema/xsd:annotation[@id]" mode="content"/>
		  <xsl:apply-templates select="//xsd:element[@name='constraints']/xsd:complexType[1]/xsd:choice[1]/xsd:element" mode="constraint"/>
		  <xsl:apply-templates select="/xsd:schema/xsd:simpleType" mode="content"/>
		</div>
	  </body>
	</html>
  </xsl:template>
  
		  
  <!-- menu -->		  
  <xsl:template match="xsd:annotation[@id]" mode="menu">
	<a id="index_h_{@id}" class="index note" href="#h:{@id}"><xsl:value-of select="@id" /></a>
  </xsl:template>
  <xsl:template match="xsd:element" mode="menu">
	<a id="index_d_{@name}" class="index attr" href="#d:{@name}"><xsl:value-of select="@name" /></a>
  </xsl:template>
  <xsl:template match="xsd:simpleType" mode="menu">
	<a id="index_k_{@name}" class="index type" href="#k:{@name}"><xsl:value-of select="@name" /></a>
  </xsl:template>
  <xsl:template name="Introduction.menu">
	<a id="index_h_Introduction" class="index note" href="#h:Introduction">Introduction</a>
  </xsl:template>
  <!-- content notes -->
  <xsl:template match="xsd:annotation[@id]" mode="content">
	<div id="content_h_{@id}" class="content">
	  <h1><xsl:value-of select="@id" /> </h1>
	  <div class="text"><xsl:apply-templates select="xsd:documentation/*" mode="html" /></div>
	</div>
  </xsl:template>

  <xsl:template name="Introduction.content">
	<div id="content_h_Introduction" class="content">
	  <h1>Introduction</h1>
	  <div class="text">
		<p>XML Schema for the <a href="http://www.emn.fr/z-info/sdemasse/gccat/">Global Constraint Catalog</a>.</p>
		<p>The schema is described in <a href="http://www.emn.fr/z-info/sdemasse/gccat/sec4.3.2.html">Section 4.3.2.</a></p>
		<p>This stylesheet displays the argument list of each constraint described in the catalog.</p>
		<p>It also presents the implementation of some constraints within the constraint systems <a href="http://choco.emn.fr/">Choco</a>, <a href="http://www.gecode.org/">Gecode</a>, <a href="http://jacop.osolpro.com/">Jacop</a>, and <a href="http://www.sics.se/sicstus/">SICStus</a> (See also: <a href="gccat_systems.xml">gccat_systems.xml</a>).</p>
	  </div>	
	  <h5>last update: <xsl:value-of select="$sys/catalog/@date"/>. <a href="http://www.emn.fr/z-info/sdemasse/">SD</a>.</h5>
	</div>
  </xsl:template>


  <!-- content : simpleType -->
  <xsl:template match="xsd:simpleType" mode="content">
	<div id="content_k_{@name}" class="content">
	  <h1><xsl:value-of select="@name" /> </h1>
	  <xsl:apply-templates select="xsd:restriction"/>
	  <div class="heading">DESCRIPTION</div><div class="text"><xsl:apply-templates select="xsd:annotation/xsd:documentation/*" mode="html" /></div>
	  <div class="heading">CONSTRAINTS</div>
	  <div class="text"><ul>
		<xsl:variable name="typeName" select="@name" />
		<xsl:apply-templates select="//xsd:element[@name='constraints']/xsd:complexType[1]/xsd:choice[1]/xsd:element[.//xsd:attribute[@type=$typeName]]" mode="constype"/>
	  </ul></div>
	</div>
  </xsl:template>
  <xsl:template match="xsd:restriction">
	<div class="heading">TYPE</div><div class="text"><span class="type"><xsl:value-of select="@base"/></span></div>
	<div class="heading">VALUES</div><div class="text"><xsl:apply-templates/></div>
  </xsl:template>
  <xsl:template match="xsd:enumeration|xsd:minInclusive|xsd:maxInclusive">
	<span class="val"><xsl:value-of select="@value" /></span><xsl:text> </xsl:text>
  </xsl:template>
  <xsl:template match="xsd:element" mode="constype">
	<li><a href="#d:{@name}"><code><xsl:value-of select="@name" /></code></a></li>
  </xsl:template>
  

  <!-- content : constraint -->
  <xsl:template match="xsd:element" mode="constraint">
	<div id="content_d_{@name}" class="content">
	  <h1><xsl:value-of select="@name" /> </h1>
	  <h2>Catalog</h2>
	  <div class="heading">ARGUMENTS</div><xsl:apply-templates mode="args"/>
	  <div class="heading">PERMANENT LINK</div><xsl:apply-templates select="." mode="url"/>
	  <div class="heading">GRAPHIC</div><xsl:apply-templates select="." mode="pict"/>
	  <xsl:variable name="ctr" select="@name"/>
	  <xsl:apply-templates select="$sys/catalog/constraint[@name=$ctr]"/>
	</div>
  </xsl:template>
  <xsl:template match="xsd:element" mode="args">
	<div class="args"><xsl:value-of select="@name" /> - <xsl:apply-templates mode="argtypes"/></div>
  </xsl:template>
  <xsl:template match="xsd:element" mode="argtypes">
	<span class="attr"><xsl:value-of select="@name" /></span>
  </xsl:template>
  <xsl:template match="xsd:element[@name='collection']" mode="argtypes" priority="1">
	<span class="attr"><xsl:value-of select="@name" /></span>(<xsl:apply-templates mode="argcollec"/>)
  </xsl:template>
  <xsl:template match="xsd:element" mode="argcollec" priority="1">
	<xsl:apply-templates mode="args"/>
  </xsl:template>
  <xsl:template match="xsd:element[@name='basetype']" mode="argtypes" priority="1">
	<xsl:apply-templates select="xsd:complexType/xsd:attribute[@name='value']" mode="attr"/>
  </xsl:template>
  <xsl:template match="xsd:element[@name='integerset']" mode="argtypes" priority="1">
	<span class="attr">set</span>(<xsl:apply-templates mode="argtypes"/>)
  </xsl:template>
  <xsl:template match="xsd:choice" mode="argtypes">
	[<xsl:apply-templates mode="argchoice"/>]
  </xsl:template>
  <xsl:template match="xsd:choice/xsd:element[position()='1']" mode="argchoice" priority="1">
	<xsl:apply-templates select="." mode="argtypes"/>
  </xsl:template>
  <xsl:template match="xsd:choice/xsd:element" mode="argchoice">
	|| <xsl:apply-templates select="." mode="argtypes"/>
  </xsl:template>

  <xsl:template match="xsd:complexType|xsd:sequence" mode="args"><xsl:apply-templates mode="args"/></xsl:template>
  <xsl:template match="xsd:complexType|xsd:sequence" mode="argtypes"><xsl:apply-templates mode="argtypes"/></xsl:template>
  <xsl:template match="xsd:complexType|xsd:sequence" mode="argcollec"><xsl:apply-templates mode="argcollec"/></xsl:template>

  <xsl:template match="xsd:attribute[contains(@type,':')]" mode="attr">
	<span class="val"><xsl:value-of select="substring-after(@type,':')" /></span>
  </xsl:template>
  <xsl:template match="xsd:attribute" mode="attr">
	<a href="#k:{@type}"><span class="val"><xsl:value-of select="@type" /></span></a>
  </xsl:template>


  <xsl:template match="xsd:element" mode="url">
	<div class="text"><a href="../../gccat/C{@name}.html">http://www.emn.fr/z-info/sdemasse/gccat/C<xsl:value-of select="@name" />.html</a></div>
</xsl:template>

  <xsl:template match="xsd:element" mode="pict">
	<div class="text"><a href="../png/{@name}.png">arguments [png]</a></div>
	<!-- div class="text"><img src="../png/{@name}.png" alt="arguments" width="700" /></div -->
  </xsl:template>

  <xsl:template match="constraint">
	<h2>Constraint Systems</h2><xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="system">
	<div class="heading"><xsl:value-of select="@name" /></div><xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="system/constraint">
	<div class="text"><a href="{@url}"><xsl:value-of select="@name" /></a></div>
  </xsl:template>

  <!-- global -->


  <xsl:template match="html:a[@rel='attr']" mode="html" xmlns:html="http://www.w3.org/1999/xhtml">
	<a href="#d:{text()}" class="attr"><xsl:apply-templates select="@*|node()" mode="html" /></a>
  </xsl:template>
  
  <xsl:template match="html:a[@rel='type']" mode="html" xmlns:html="http://www.w3.org/1999/xhtml">
	<a href="#k:{text()}" class="type"><xsl:apply-templates select="@*|node()" mode="html" /></a>
  </xsl:template>

  <xsl:template match="html:a[@rel='note']" mode="html" xmlns:html="http://www.w3.org/1999/xhtml">
	<a href="#h:{text()}" class="note"><xsl:apply-templates select="@*|node()" mode="html" /></a>
  </xsl:template>
  
  <xsl:template match="@*|node()" mode="html">
	<xsl:copy><xsl:apply-templates select="@*|node()" mode="html" /></xsl:copy>
  </xsl:template>
  
  
</xsl:stylesheet>

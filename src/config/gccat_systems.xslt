<?xml version="1.0"?>
<xsl:stylesheet
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:xsd="http://www.w3.org/2001/XMLSchema"
   xmlns="http://www.w3.org/1999/xhtml"
   version="1.0">
  
  <xsl:output method="html" />
  <xsl:template match="/">
	<html>
	  <head>
		<title>Global Constraint Catalog: XML Schema</title>
		<style>
		  body { margin: 0px;	padding: 0px; overflow: hidden;	font-family: Helvetica; font-size: 11pt;}
		  code { font-family: Courier; }
		  table { width: 99%;   font-family: Courier; font-size: 10pt; }
		  td { width: 25%; padding: 0 10px; }
		  td.a { width: 20%; padding: 1px 10px; }
		  td.b { width: 80%; padding: 0 10px; }
		  tr { background: #EEEEEE; }
		  ul { margin: 0; list-style:none; padding: 0; }
		  li { margin: 0; padding: 0; }
		  h5 { margin:100px; padding:0; font-size:70%; font-weight:normal; font-style:italic; text-align:right; }
		  a { text-decoration: none; }
		  a:hover { text-decoration: underline; }
		  #indices { position: absolute; left: 0px; width: 200px;	top: 0px;
		  bottom: 0px; height: expression(offsetParent.offsetHeight);	/* only IE uses this */
		  overflow: auto; background: #EEEEEE }
		  #contents {	position: absolute;	left: 210px; top: 0px;
		  right:0px; width: expression(offsetParent.offsetWidth-210);	/* only IE uses this */
		  bottom: 0px; height: expression(offsetParent.offsetHeight);	/* only IE uses this */
		  overflow: auto;	}
		  .val a, .val a:hover{ color: green; }
		  .attr { font-family: Courier; font-weight: bold; }
		  .heading { padding: 10px 5px 2px 5px; color: gray; font-weight: bold; font-size: 60%; }
		  h2 { color: #333333; font-weight: bold; font-size: 130%; }
		  .text {	padding-left: 20px;	padding-bottom: 10px; padding-right: 10px; }
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
		  <div class="heading">NOTES</div><xsl:call-template name="Introduction.menu"/>
		  <div class="heading">VIEWS</div>
		  <a id="index_d_catalog" href="#d:catalog" class="index">Catalog</a>
		  <a id="index_d_choco" href="#d:choco" class="index">Choco</a>
		  <a id="index_d_gecode" href="#d:gecode" class="index">Gecode</a>
		  <a id="index_d_jacop" href="#d:jacop" class="index">Jacop</a>
		  <a id="index_d_sicstus" href="#d:sicstus" class="index">SICStus</a>
		</div>

		<div id="contents">
		  <xsl:call-template name="Introduction.content"/>
		  <xsl:call-template name="catalog"/>
		  <xsl:call-template name="choco"/>
		  <xsl:call-template name="gecode"/>
		  <xsl:call-template name="jacop"/>
		  <xsl:call-template name="sicstus"/>
		</div>
	  </body>
	</html>
  </xsl:template>
  
  <!-- menu -->		  
  <xsl:template name="Introduction.menu">
	<a id="index_h_Introduction" class="index note" href="#h:Introduction">Introduction</a>
  </xsl:template>

  <!-- content notes -->
  <xsl:template match="xsd:annotation[@id]" mode="content">
	<div id="content_h_{@id}" class="content"><h1><xsl:value-of select="@id" /> </h1>
	  <div class="text"><xsl:apply-templates select="xsd:documentation/*" mode="html" /></div>
	</div>
  </xsl:template>

  <xsl:template name="Introduction.content">
	<div id="content_h_Introduction" class="content"> <h1>Introduction</h1>
	  <div class="text"><p>This document compares global constraints defined in the <a href="http://www.emn.fr/z-info/sdemasse/gccat/">Global Constraint Catalog</a> and in the constraint systems:</p><ul>
		  <li><a href="http://choco.emn.fr/">Choco</a></li>
		  <li><a href="http://www.gecode.org/">Gecode</a></li>
		  <li><a href="http://jacop.osolpro.com/">Jacop</a></li>
		  <li><a href="http://www.sics.se/sicstus/">SICStus</a></li>
		</ul><p>See also the XML schema for the catalog: <a href="gccat_schema.xml">gccat_schema.xml</a>.</p>
	  </div>	
	  <h5>last update: <xsl:value-of select="/catalog/@date"/>. <a href="http://www.emn.fr/z-info/sdemasse/">SD</a>.</h5>
	</div>
  </xsl:template>

  <!-- content : constraint -->
  <xsl:template name="catalog">
	<div id="content_d_catalog" class="content">
	  <h1>Catalog view</h1>
	  <div class="text">Some global constraints defined in the Catalog and their synonyms within different solvers.</div>
	  <table><tr><td>CATALOG</td><td>CHOCO</td><td>GECODE</td><td>JACOP</td><td>SICSTUS</td></tr>
		<xsl:apply-templates mode="catalog"/></table>
	  </div>
  </xsl:template>

  <xsl:template name="choco">
	<div id="content_d_choco" class="content"><h1>Choco view</h1>
	  <div class="text">Some global constraints defined in Choco and their synonyms within the Catalog and other solvers.</div>
	  <table><tr><td class="a">CHOCO</td><td class="b"><table><tr><td>CATALOG</td><td>GECODE</td><td>JACOP</td><td>SICSTUS</td></tr></table></td></tr>
		<xsl:apply-templates select="//system[@name='choco']/ctr[not(@name=preceding::system[@name='choco']/ctr/@name)]" mode="choco"><xsl:sort select="@name"/></xsl:apply-templates>
	  </table>
	</div>
  </xsl:template>

  <xsl:template name="gecode">
	<div id="content_d_gecode" class="content"><h1>Gecode view</h1>
	  <div class="text">Some global constraints defined in Gecode and their synonyms within the Catalog and other solvers.</div>
	  <table><tr><td class="a">GECODE</td><td class="b"><table><tr><td>CATALOG</td><td>CHOCO</td><td>JACOP</td><td>SICSTUS</td></tr></table></td></tr>
		<xsl:apply-templates select="//system[@name='gecode']/ctr[not(@name=preceding::system[@name='gecode']/ctr/@name) or not(@url=preceding::system[@name='gecode']/ctr/@url)]" mode="gecode"><xsl:sort select="@name"/></xsl:apply-templates>
	  </table>
	</div>
  </xsl:template>

  <xsl:template name="jacop">
	<div id="content_d_jacop" class="content"><h1>Jacop view</h1>
	  <div class="text">Some global constraints defined in Jacop and their synonyms within the Catalog and other solvers.</div>
	  <table><tr><td class="a">JACOP</td><td class="b"><table><tr><td>CATALOG</td><td>CHOCO</td><td>GECODE</td><td>SICSTUS</td></tr></table></td></tr>
		<xsl:apply-templates select="//system[@name='jacop']/ctr[not(@name=preceding::system[@name='jacop']/ctr/@name)]" mode="jacop"><xsl:sort select="@name"/></xsl:apply-templates>
	  </table>
	</div>
  </xsl:template>

  <xsl:template name="sicstus">
	<div id="content_d_sicstus" class="content"><h1>Sicstus view</h1>
	  <div class="text">Some global constraints defined in SICStus and their synonyms within the Catalog and other solvers.</div>
	  <table><tr><td class="a">SICSTUS</td><td class="b"><table><tr><td>CATALOG</td><td>CHOCO</td><td>GECODE</td><td>JACOP</td></tr></table></td></tr>
		<xsl:apply-templates select="//system[@name='sicstus']/ctr[not(@name=preceding::system[@name='sicstus']/ctr/@name)]" mode="sicstus"><xsl:sort select="@name"/></xsl:apply-templates>
	  </table>
	</div>
  </xsl:template>

  <!-- tables -->

  <xsl:template match="system"><ul><xsl:apply-templates mode="sys"/></ul></xsl:template>
  <xsl:template match="ctr" mode="sys"><li><a href="{@url}"><xsl:value-of select="@name" /></a></li></xsl:template>

  <xsl:template match="constraint" mode="cat">
	<a href="../../gccat/C{@name}.html"><xsl:value-of select="@name" /></a>
  </xsl:template>

  <xsl:template match="constraint" mode="catalog">
	<tr><td class="val"><xsl:apply-templates select="." mode="cat"/></td>
	  <td><xsl:apply-templates select="system[@name='choco']"/></td>
	  <td><xsl:apply-templates select="system[@name='gecode']"/></td>
	  <td><xsl:apply-templates select="system[@name='jacop']"/></td>
	  <td><xsl:apply-templates select="system[@name='sicstus']"/></td></tr>
  </xsl:template>

  <xsl:key name="chocok" match="constraint" use="system[@name='choco']/ctr/@name"/>
  <xsl:template match="ctr" mode="choco">
	<!-- tr><td class="a"><a href="{@url}"><xsl:value-of select="@name" /></a></td -->
	<tr><td class="a"><a href="http://www.emn.fr/z-info/choco-solver/choco-kernel/apidocs/choco/Choco.html"><xsl:value-of select="@name" /></a></td>
	  <td class="b"><table><xsl:apply-templates select="key('chocok',@name)" mode="choco"/></table></td></tr>
  </xsl:template>
  <xsl:template match="constraint" mode="choco">
	<tr><td class="val"><xsl:apply-templates select="." mode="cat"/></td>
	<td><xsl:apply-templates select="system[@name='gecode']"/></td>
	<td><xsl:apply-templates select="system[@name='jacop']"/></td>
	<td><xsl:apply-templates select="system[@name='sicstus']"/></td></tr>
  </xsl:template>

  <xsl:key name="jacopk" match="constraint" use="system[@name='jacop']/ctr/@name"/>
  <xsl:template match="ctr" mode="jacop">
	<tr><td class="a"><a href="{@url}"><xsl:value-of select="@name" /></a></td>
	  <td class="b"><table><xsl:apply-templates select="key('jacopk',@name)" mode="jacop"/></table></td></tr>
  </xsl:template>
  <xsl:template match="constraint" mode="jacop">
	<tr><td class="val"><xsl:apply-templates select="." mode="cat"/></td>
	<td><xsl:apply-templates select="system[@name='choco']"/></td>
	<td><xsl:apply-templates select="system[@name='gecode']"/></td>
	<td><xsl:apply-templates select="system[@name='sicstus']"/></td></tr>
  </xsl:template>

  <xsl:key name="sicstusk" match="constraint" use="system[@name='sicstus']/ctr/@name"/>
  <xsl:template match="ctr" mode="sicstus">
	<tr><td class="a"><a href="{@url}"><xsl:value-of select="@name" /></a></td>
	  <td class="b"><table><xsl:apply-templates select="key('sicstusk',@name)" mode="sicstus"/></table></td></tr>
  </xsl:template>
  <xsl:template match="constraint" mode="sicstus">
	<tr><td class="val"><xsl:apply-templates select="." mode="cat"/></td>
	<td><xsl:apply-templates select="system[@name='choco']"/></td>
	<td><xsl:apply-templates select="system[@name='gecode']"/></td>
	<td><xsl:apply-templates select="system[@name='jacop']"/></td></tr>
  </xsl:template>

  <xsl:template match="ctr" mode="gecode">
	<xsl:variable name="ctr" select="@name"/><xsl:variable name="url" select="@url"/> 
	<tr><td class="a"><a href="{@url}"><xsl:value-of select="@name" /></a></td>
	  <td class="b"><table><xsl:apply-templates select="//constraint[system[@name='gecode']/ctr[@name=$ctr and @url=$url]]" mode="gecode"/></table></td></tr>
  </xsl:template>
  <xsl:template match="constraint" mode="gecode">
	<tr><td class="val"><xsl:apply-templates select="." mode="cat"/></td>
	<td><xsl:apply-templates select="system[@name='choco']"/></td>
	<td><xsl:apply-templates select="system[@name='jacop']"/></td>
	<td><xsl:apply-templates select="system[@name='sicstus']"/></td></tr>
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
  

  <xsl:template match="ctr" mode="jacopold">
	<tr><td><a href="{@url}"><xsl:value-of select="@name" /></a></td>
	  <td class="val"><ul><xsl:apply-templates select="key('jacopk',@name)" mode="cat"/></ul></td>
	  <td><xsl:apply-templates select="key('jacopk',@name)/system[@name='choco']"/></td>
	  <td><xsl:apply-templates select="key('jacopk',@name)/system[@name='gecode']"/></td>
	  <td><xsl:apply-templates select="key('jacopk',@name)/system[@name='sicstus']"/></td>
	</tr>
  </xsl:template>


</xsl:stylesheet>

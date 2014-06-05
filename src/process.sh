#! /bin/bash
#	 Sophie Demassey/ EMN-LINA/ Global Constraint Catalog Project
#	 general script for translating the Latex documents to HTML
#	 Time-stamp: <[process.sh] last changed 05-06-2014 15:59 by sofdem>

zepwd=`pwd`/..
xsldir=$zepwd/xsl
indir=$zepwd/catalog
namedir=gccat

############################### scripts
#figpstex=$xsldir/script/figpstex.sh
makeindex=$xsldir/script/make_index.sh
processctrs=$xsldir/script/ctrs_process.sh
processtikz=$xsldir/script/tikz_process.sh
rmhyperlinks=$xsldir/script/remove_hyperlink.sh
extractpages=$xsldir/script/extractnamedpages.sh
gensysxml=$xsldir/script/systemxmltopl.sh
tralicsconf=$xsldir/config/gccat.tcf
for i in $makeindex $processctrs $rmhyperlinks $tralicsconf $gensysxml; do
	if [ ! -f $i ]; then echo "stop: no script file $i"; exit 1; fi
done
tralicsdir=~/bin/tralics-2.13.6
tralics=$tralicsdir/src/tralics
dot=`which dot` 					# GraphViz
fig2dev=`which fig2dev` 			# jfig
for i in $tralics $dot $fig2dev; do
	if [ ! -x $i ]; then echo "stop: no executable $i"; exit 1; fi
done
tralicsconfdir=$tralicsdir/confdir
if [ ! -d $tralicsconfdir ]; then echo "stop: no directory $tralicsconfdir"; exit 1; fi

############################### output
outdir=$zepwd/webcatalog
bindir=$zepwd/bin
savedir=$bindir/savetex
imgdir=$outdir/ctrs
#pstrickdir=$outdir/figpstrick
logdir=$bindir/log
for i in $outdir $bindir $savedir $imgdir $logdir; do
	if [ ! -d $i ]; then mkdir $i; fi
done
texinputfiles="$xsldir/texinput/titlepage.tex"
cp -f $texinputfiles $indir/
log=$logdir/process.log
if [ -f $log ]; then rm $log; fi
if [ -d "$indir/ctrs(web)" ]; then mv $indir/ctrs $bindir/ctrs_pdf; mv "$indir/ctrs(web)" $indir/ctrs; fi


########################################################################################################
echo "**********************************************"
echo "       create images [Y]es / [N]o / [Q]uit ?"
echo "**********************************************"

read ans; case $ans in
	[yY])

		echo "(dot): in/ctrs/XXX.dot --> out/ctrs/XXX.png (initial-final graphs)"; c=0
		for i in $indir/ctrs/*.dot; do basename=`basename $i .dot`; outi=$imgdir/$basename.png
			if [ ! -f $outi ]; then echo "dot $basename" | tee -a $log; c=`expr $c + 1`
				$dot -Tpng $i -o $outi
			fi
		done
		echo "dot: $c images generated" | tee -a $log

		#echo "(fig2dev): in/srcfigs/fig/XXX.fig --> out/ctrs/XXX.png"; c=0
		#for i in $indir/srcfigs/fig/*.fig; do basename=`basename $i .fig`; outi=$imgdir/$basename.png
		#	if [ ! -f $outi ]; then echo "fig2dev $basename" | tee -a $log; c=`expr $c + 1`
		#		$fig2dev -L png -m 1.5 -S 4 -f cmr $i $outi
		#	fi
		#done
		#echo "fig2dev: $c images generated" | tee -a $log

		#echo "(fig2dev): in/srcfigs/pstex/XXX.fig --> out/ctrs/XXX.png"; c=0
		#for i in $indir/srcfigs/pstex/*.fig; do basename=`basename $i .fig`; outi=$imgdir/$basename.png
		#	if [ ! -f $outi ]; then echo "fig2dev $basename" | tee -a $log; c=`expr $c + 1`
		#		$fig2dev -L png -m 1.5 -S 4 -f cmr $i $outi
		#	fi
		#done
		#echo "fig2dev: $c images generated" | tee -a $log

		#echo "(pstex): in/ctrs/XXX.pstex_t --> in/ctrs/XXX.eps"; c=0
		#for i in $indir/ctrs/*.pstex_t; do basename=`basename $i .pstex_t`; outi=$indir/ctrs/$basename.eps
		#	if [ ! -f $outi ]; then echo "figpstex $basename" | tee -a $log; c=`expr $c + 1`
		#		bash $figpstex $indir/ctrs $basename;  mv $basename.eps $indir/ctrs/
		#	fi
		#done
		#echo "pstex: $c missing eps generated" | tee -a $log


		echo "(gs): in/ctrs/XXX.eps --> out/ctrs/XXX.png"; c=0
		for i in $indir/ctrs/*.eps; do basename=`basename $i .eps`; outi=$imgdir/$basename.png
			if [ ! -f $outi ]; then echo "gs $basename" | tee -a $log; c=`expr $c + 1`
				gs -q -dBATCH -dNOPAUSE -sDEVICE=png16m -dSAFER -dEPSCrop -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -r200 -sOutputFile=$outi $i # resolution = -R200 for the biggest images
			fi
		done
		echo "gs: $c images generated" | tee -a $log

		#echo "(cp): in/src/fig_treeXXX.png --> out/ctrs/fig_treeXXX.png" | tee -a $log
		#cp $indir/src/fig*.png $imgdir/

		#echo "(cp): in/figpstrick/*.pl --> out/figpstrick/*.pl" | tee -a $log
		#cp $indir/srcfigs/figpstrick/*.pl $pstrickdir/

		
		#echo "(gs): in/figpstrick/XXX.eps --> out/figpstrick/XXX.png"; c=0
		#echo "(cp): in/ctrs/XXX.xml --> out/figpstrick/XXX.xml"
		#for i in $indir/srcfigs/figpstrick/*.eps; do basename=`basename $i .eps`; outi=$pstrickdir/$basename.png
		#	xmli=$indir/ctrs/$basename.xml 
		#	if [ -f $xmli ]; then cp $xmli $pstrickdir/; else echo "WARNING: file $xmli does not exist" | tee -a $log; fi
		#	if [ ! -f $outi ]; then echo "gs $basename" | tee -a $log; c=`expr $c + 1`
		#		gs -q -dBATCH -dNOPAUSE -sDEVICE=png16m -dSAFER -dEPSCrop -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -r200 -sOutputFile=$outi $i # resolution = -R200 for the biggest images
		#	fi
		#done
		#echo "gs: $c images generated in figpstrick" | tee -a $log
		;;

	[qQ])
		echo "bye"
		exit 1;;
esac

########################################################################################################
echo "*****************************"
echo "    process preface.tex [Y]es / [N]o / [Q]uit ?"
echo "*****************************"

read ans; case $ans in
	[yY])
		cd $indir/
		if [ ! -f $savedir/preface.tex ]; then cp preface.tex $savedir; else cp $savedir/preface.tex ./; fi

		#echo '\hypertarget{bla}{}\subsection{bli} --> \subsection{bli}\label{bla}'
		#sed -i .bak -e 's/^\\hypertarget{\([^}]*\)}{}\(\\subsection{.*}\)/\2\\label{\1}/' preface.tex
		echo '\hypertarget{bla}{}\subsection[bli]{blo} --> \subsection{bli}\label{bla}'
		sed -i "" -e 's/^\\hypertarget{\([^}]*\)}{}\\subsection\[\([^]]*\)\]\(.*\)/\\subsection{\2}\\label{\1}%\3/' preface.tex

		#echo '\path|bla| --> \url{bla}'
		#sed -i .bak -e 's/\(.*\)\\path|\(.*\)|\(.*\)/\1\\url{\2}\3/g' preface.tex
		
		echo "[Bb]oxedverbatim --> verbatim (renewcommand is not enough)..."
		sed -i "" -e 's/[Bb]oxedverbatim/verbatim/' preface.tex
	
		#echo 'href{figpstrick/ --> href{../figpstrick/'
		#sed -i .bak -e 's%href{figpstrick/%href{\.\./figpstrick/%g' preface.tex

		echo '\ref{correspondence_tables} --> \href{xml/gccat_systems.xml}{Systems}'
		sed -i "" -e 's%ref{correspondence_tables}%href{xml/gccat_systems.xml}{Systems}%g;s%ref{sec:systems}%href{xml/gccat_systems.xml}{Systems}%g' preface.tex

		sed -i "" -e 's/\\setcounter{enumi}/%&/' preface.tex
		;;

	[qQ])
		echo "bye"
		exit 1;;
esac

########################################################################################################
echo "*****************************"
echo "    make index [Y]es / [N]o / [Q]uit ?"
echo "*****************************"

read ans; case $ans in
	[yY])
		echo "makeindex restrictions, generators, characteristics..."
		cd $indir/
		bash $makeindex
		;;
	[qQ])
		echo "bye"
		exit 1;;
esac

########################################################################################################
echo "*****************************"
echo "    process ctrs/*.tex [Y]es / [N]o / [Q]uit ?"
echo "*****************************"

read ans; case $ans in
	[yY])
		cd $xsldir
		bash $processctrs $indir/ctrs $savedir

		bash $rmhyperlinks $indir/ctrs/place_in_pyramid.tex $indir/ctrs/disj.tex $indir/ctrs/connect_points.tex $indir/ctrs/tour.tex $indir/ctrs/equilibrium.tex $indir/ctrs/first_value_diff_0.tex $indir/ctrs/k_cut.tex $indir/ctrs/lex_greater.tex $indir/ctrs/lex_greatereq.tex $indir/ctrs/lex_less.tex $indir/ctrs/lex_lesseq.tex $indir/ctrs/minimum_weight_alldifferent.tex $indir/ctrs/roots.tex
		echo "check tralics errors due to hyperlinks in array environment"
		echo "then run remove_hyperlink.sh on these specific files"
		;;
	[qQ])
		echo "bye"
		exit 1;;
esac

########################################################################################################
echo "*****************************"
echo "    Tikz: process preface and ctrs/*.tex [Y]es / [N]o / [Q]uit ?"
echo "*****************************"

read ans; case $ans in
	[yY])
		source $processtikz
		cd $xsldir
		processallfiles
		cd ..
		;;
	[qQ])
		echo "bye"
		exit 1;;
esac

########################################################################################################
echo "*****************************"
echo "    Tikz: generate images [Y]es / [N]o / [Q]uit ?"
echo "*****************************"

read ans; case $ans in
	[yY])
		source $processtikz
		cd $xsldir
		tikz2png
		echo "(mv): in/tikz/*.png --> out/ctrs/*.tikz" | tee -a $log
		cd ..
		mv $xsldir/tikz/*.png $imgdir/
		;;
	[qQ])
		echo "bye"
		exit 1;;
esac

########################################################################################################
echo "*****************************"
echo " Tralics: generate catalog.xml [Y]es / [N]o / [Q]uit ?"
echo "*****************************"

read ans; case $ans in
	[yY])
		cd $indir/
		if [ ! -f $savedir/catalog.tex ]; then cp catalog.tex $savedir; else cp $savedir/catalog.tex ./; fi
		sed -i "" 's/^\\webfalse/%&/;s/^%\\webtrue/\\webtrue/;s/\\usetikzlibrary/%&/' catalog.tex

		# noentname option : replace &nbsp; by its unicode
		#$tralics catalog -config=$tralicsconf -confdir=$tralicsconfdir -xml -noentnames -math_variant > $logdir/tralics.log
		$tralics catalog -config=$tralicsconf -confdir=$tralicsconfdir -xml > $logdir/tralics.log
		grep -A1 "Error" $logdir/tralics.log; grep "errors" $logdir/tralics.log
		;;
	[qQ])
		echo "bye"
		exit 1;;
esac

########################################################################################################
echo "*****************************"
echo " xmllint: validate catalog.xml against dtd [Y]es / [N]o / [Q]uit ?"
echo "*****************************"

read ans; case $ans in
	[yY])
		echo "verif xmllint..."; i=$indir/catalog.xml outi=$xsldir/catalog.xml
		if [ -f  $i ]
		then # temporaire !!!!!!!!!!!!! bug Tralics v.12.3
#			cat $i | sed "s/\(figure [^>]* width='[^']*\)\..*cm/\1cm/g" | sed "s%>  \(<hi rend='bold'>(A)\)%></cell></row><row><cell>\1%" > catalog.xml
			# supprime les formules vides: problemes dans firefox/mathjax (et inutiles de toute maniere) --> a faire dans le xslt
			cat $i | sed "s/\(figure [^>]* width='[^']*\)\..*cm/\1cm/g;s%<formula type='inline'><math xmlns='http://www.w3.org/1998/Math/MathML'><mrow/></math></formula>%%g;s%<unexpected>%%g;s%</unexpected>%%g" > $outi
			#########################################################################################rm $i
		elif [ ! -f $outi ]; then echo "problem !!! no xml file to process"; exit 1
		fi
		xmllint -valid -noout -xinclude $outi 2> $logdir/xmllint.log
		echo "xmllint errors:"; cat $logdir/xmllint.log
		
		echo "verif manuelle..."
		echo "****missing figures (no rend nor file attributes) :" > $logdir/verifxml.log
		grep -n "<figure" $outi | grep -v "<figure.*file=" | grep -v "<figure.*rend=" >> $logdir/verifxml.log
		echo "****missing ref (no attribute) :" >> $logdir/verifxml.log
		grep -n "<ref/>" $outi >> $logdir/verifxml.log
		echo "****not translated by Tralics :" >> $logdir/verifxml.log
		grep -n '!--' $outi | grep -v "Translated from latex" >> $logdir/verifxml.log
		echo "xml errors:"; cat $logdir/verifxml.log
		;;
	[qQ])
		echo "bye"
		exit 1;;
esac

echo "*****************************"
echo " xsltproc: generate xhtml [Y]es / [N]o / [Q]uit ?"
echo "*****************************"

read ans; case $ans in
	[yY])
		cd $xsldir/
		xsltproc -xinclude config/gccat_html_mathjax.xsl catalog.xml 2> $logdir/xsltproc.log
		echo "xstproc errors:"; cat $logdir/xsltproc.log
		if [ -d $namedir ]; then rm -rf $outdir/$namedir; mv $namedir $outdir/; fi
		;;
	[qQ])
		echo "bye"
		exit 1;;
esac

echo "*****************************"
echo " post-processing [Y]es / [N]o / [Q]uit ?"
echo "*****************************"

zedir=$outdir/$namedir
auxdir=$outdir/aux
if [ ! -d $auxdir ]; then mkdir $auxdir; fi
zenumber=`ls -l $zedir/C*.html | wc -l`
zedate=`date +"%Y-%m-%d"`
zedateshort=`echo $zedate | sed 's/-0\([1-9]\)/-\1/g'`

read ans
case $ans in
	[yY])

		cd $zedir

		#echo "removing the DOCTYPE tag... (EMN bullshit server)"
		#for i in `ls`; do cat $i | sed '/^<!DOCTYPE html/d' > tmp.html; mv tmp.html $i ; done

		echo "copy home page..."
		cat $xsldir/config/index.html | sed "s/<span class=\"zedate\">[0-9]*-[0-9]*-[0-9]*<\/span>/<span class=\"zedate\">${zedate}<\/span>/g;s/<span class=\"zenumber\">[0-9]*<\/span>/<span class=\"zenumber\">${zenumber}<\/span>/g" > index.html

		echo "copy css/"
		cp -r $xsldir/css $outdir/

		echo "copy files in aux/xml/"
		mkdir $auxdir/xml
		cp $indir/schema.xsd $indir/schema.pl $indir/schema_dot.pl $indir/ctrs/*.xml $auxdir/xml
		cp $xsldir/config/gccat_systems.xslt $xsldir/config/gccat_schema.xslt $auxdir/xml
 		cat $indir/schema.xsd | sed '/<?xml/{
 a\
<?xml-stylesheet href="gccat_schema.xslt" type="text/xsl"?>
 }
 ' > $auxdir/xml/gccat_schema.xml
		
		echo "generate gccat_systems.xml"
		syspl=$indir/systable.pl
		if [ ! -f $syspl ]; then echo "problem !!! no file $syspl to process"; exit 1; fi
		sh $gensysxml $syspl gccat_systems.xml; mv gccat_systems.xml $auxdir/xml
		
		echo "copy prolog files in aux/src/"
		mkdir $auxdir/src
		cp $indir/src/*.pl $auxdir/src
		
		echo "copy png files in png/"
		mkdir $auxdir/png
		cp $indir/images/*.png $auxdir/png
		
		echo "copy files in doc/"
		mkdir $auxdir/doc
		#########????? rm config/tmp.html
		cp $indir/catalog.pdf $auxdir/doc #$xsldir/config/* $xsldir/$0 $xsldir/script/* $auxdir/doc

# 		echo "*****************************"
# 		echo "process biblio..."
# 		cd $zedir
# 		for i in `cat biblio.html | grep -o '<a name="bid[0-9]*" id' | grep -o 'bid[0-9]*'`
# 		do
# 			echo $i
# 			zerefC=`grep -l "#$i\"" C*.html | sed "s%C\(.*\)\.html%<a href=\"C\1.html\">\1</a>, %g" | tr -s '\n' ' '`
# 			zerefK=`grep -l "#$i\"" K*.html | sed "s%K\(.*\)\.html%<a href=\"K\1.html\">\1</a>, %g" | tr -s '\n' ' '`
# 			zerefsec=`grep -l "#$i\"" sec*.html | sed "s%sec\(.*\)\.html%<a href=\"sec\1.html\">\1</a>, %g" | tr -s '\n' ' '`
# 			zerefo=`grep -l "#$i\"" preface.html | sed "s%\(.*\)\.html%<a href=\"\1.html\">\1</a>, %g" | tr -s '\n' ' '`
# 			zeref=`echo $zerefC $zerefK $zerefsec $zerefo | sed 's/\(.*\), *$/\1/'`
# 			cat biblio.html | sed "/<a name=\\\"$i\\\"/{
# N
# a\\
# <span class =\"ref\">[$zeref]</span>
# }
# " > tmp.html
# 			mv tmp.html biblio.html
#		done
		;;
	[qQ])
		echo "bye"
		exit 1;;
esac

echo "*****************************"
echo " generate pdf [Y]es / [N]o / [Q]uit ?"
echo "*****************************"

read ans; case $ans in
	[yY])
		echo "generate pdf files"
		cd $indir
		dvips catalog.dvi -o $xsldir/catalog.ps
		cd $xsldir
		bash $extractpages $zedir $auxdir/pdf
		rm $xsldir/catalog.ps
		;;
	[qQ])
		echo "bye"
		exit 1;;
esac

##################### TODO
#mathml.xsl: mfenced when child:mtable then count mtr

echo "!!!!!!!!!!!!!!!!!!!! manoprocess !!!!!!!!!!!!"
echo "xml+xslt"
#echo "squared squares: root, branching, left (--> figpstricks) + directory squared/"
echo "table numbers in keywords Flow models for... 3.7.84...."
echo "generate missing figs (tree) and grep -r MISSING *"
echo "update index.html"
echo "remove duplicated backrefs in biblio"
#echo " nouvelles contraintes : "
#for i in C*.html; do if [ ! -f ../../../catalog-0808/xsl/gccat/$i ]; then echo $i; fi; done


# ---------- deplace $ apres le hyperlink dans size_max_starting_seq_alldifferent.tex lignes 83-84 et size_max_seq_alldiff lignes 84-90
# ---------- supprimer le pspicture dans preface et in_interval_reified

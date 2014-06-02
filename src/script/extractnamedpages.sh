#!/bin/bash
# recherche le numero de page correspondant a une section constraint (4.?)
# for i in 1 2 3 4
# do 
#	 echo $i
#	 grep -B20 "(section\.4\.$i).*pdfmark" catalog.ps | grep -o "(page\.[0-9]*)"
#done

if [ $# -lt 2 ]; then echo "Syntax error: $0 html-inrep pdf-outrep"; exit 1; fi

psfile=catalog.ps
numsec=5
htmlrep=$1
pdfrep=$2
if [ ! -d $pdfrep ]; then mkdir $pdfrep; fi

#compte le nombre de contraintes
imax=`ls -1 $htmlrep/C*.html | wc -l`
#imax=12
echo $imax

i=1
prevpage=`grep -B40 "View.*(section\.$numsec\.$i)" $psfile | grep -o "TeXDict begin [0-9]* [0-9]*" | tail -1 | cut -d' ' -f4`
if [ -z $prevpage ]; then echo "Error: no page found for section $i !!! EXIT"; exit 1; fi
prevpage=`expr $prevpage + 1`
previ=$i; i=`expr $i + 1`;

for htmlfile in `ls -1 $htmlrep/C*.html`
do
	ctrname=`basename $htmlfile | sed "s/C\(.*\)\.html/\1/"`
	grep -q "<h2>$numsec\.$previ\. $ctrname</h2>" $htmlfile
	if [ $? -ne 0 ]; then echo "Error: $ctrname.html is not section $numsec.$previ !!! EXIT"; exit 1; fi

	if [ $i -le $imax ]
	then page=`grep -B40 "View.*(section\.$numsec\.$i)" $psfile | grep -o "TeXDict begin [0-9]* [0-9]*" | tail -1 | cut -d' ' -f4`
	else page=`grep -B40 "View.*(appendix\.A)" $psfile | grep -o "TeXDict begin [0-9]* [0-9]*" | tail -1 | cut -d' ' -f4`
	fi
	if [ -z $page ]; then echo "Error: no page found for section $previ !!! EXIT"; exit 1; fi

	echo $numsec.$previ $ctrname $prevpage $page
	if [ $prevpage -gt $page ]; then echo "Error: negative number of pages !!! EXIT"; exit 1; fi

	psselect -p$prevpage-$page $psfile $ctrname.ps; ps2pdf $ctrname.ps $pdfrep/$ctrname.pdf; rm $ctrname.ps
	
	prevpage=`expr $page + 1`
	previ=$i; i=`expr $i + 1`;
done

#psselect -p270-271 catalog.ps sec4.1.ps
#ps2pdf sec4.1.ps


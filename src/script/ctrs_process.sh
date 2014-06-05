#! /bin/bash

# modify all non-data tex files in ctrs/

ctrsrep=$1
saverep=$2
if [ ! -d $repctrs ]; then echo "stop: no $ctrsrep directory"; exit 1; fi
if [ ! -d $saverep ]; then mkdir $saverep; fi

for i in $ctrsrep/*.tex
do
	filei=`basename $i`
	if [ ! -f $saverep/$filei ]; then cp $i $saverep; else cp $saverep/$filei $ctrsrep/; fi
	
	case $filei in
		*_data.tex) ;;
		*)
###### remove \small tag in the \index command -- pb in tralics 2.10
###### remove \hrule
###### replace \hypertarget{C..P..}{} by \label{C..P..}
###### separate the lists at the automaton and the graph description parts (only the first occurrence e.g. tour)
###### remove hyperlink{DT...} !!! why did they reappear ?????
###### replace the colored boxes by command \mygreatbox
###### remove useless \\\ \\
			cat $i | sed 's/^\(\\index{signature.*\)\\small\(.*}\)$/\1\2/g;s/\\hrule//;s/\(\\item\[.*Automaton.*\]\)/\\end{ctrdesc}\\begin{ctrdesc}\1/;s/hypertarget\({C.*P.*}\){}/label\1/;s/\$\\hyperlink{DT[^}]*}{\$\([^$]*\)\$}\$/\1/g;s/\\setlength\\fboxrule{1\.5pt}\\fcolorbox{MyCornflowerBlue}{MyYellowlight}{\\makebox\[\\width\]/{\\mygreatbox/;s/\\setlength\\fboxrule{1\.5pt}\\fcolorbox{MyRed}{white}{\\begin{minipage}{11\.1cm}//;s/\\\\\\ \\\\//;s/\\colorbox{MyAzurelight}{\\begin{minipage}\[t\]{11\.2cm}//;s/\\end{minipage}}//;1,/item\[Arc input(s)\]/s/\(\\item\[Arc input(s)\]\)/\\end{ctrdesc}\\begin{ctrdesc}\1/;s/\(g12.*\)#/\1\\#/'> tmp.tex
# todo: remplacer les clearpage par les hrule ?? 
			mv tmp.tex $i
			
##### declare the example figure at the same place it is called: \examplefig OR \examplefigv
			grep "^\\\examplefig[:blank:]*$" $i > tmp.txt
			if [ $? -eq 0 ]; then
				echo $filei examplefig
				thedef=`cat $i | grep -o '\\\\def\\\\examplefig{\\\\.*}' | head -1 | sed 's/\\\\def\\\\examplefig{\(.*\)}/\1/'  | sed 's/\(\\\\\)/\1\1/g'`
				echo $thedef | grep "@" > tmp.txt
				if [ $? -eq 1 ]; then
					cat $i | sed "s@^\\\\\\examplefig[[:blank:]]*\$@${thedef}@" | sed 's/^\\def\\examplefig/%&/' > temp.tex
					mv temp.tex $i
				else
					echo "\examplefig command cannot be changed in $i !!!"
				fi
			else
				grep "^\\\examplefigv*$" $i > tmp.txt
				if [ $? -eq 0 ]; then
					echo $filei examplefigv
					thedef=`cat $i | grep -o '\\\\def\\\\examplefigv{\\\\.*}' | head -1 | sed 's/\\\\def\\\\examplefigv{\(.*\)}/\1/'  | sed 's/\(\\\\\)/\1\1/g'`
					echo $thedef | grep "@" > tmp.txt
					if [ $? -eq 1 ]; then
						cat $i | sed "s@^\\\\\\examplefigv[[:blank:]]*\$@${thedef}@" | sed 's/^\\def\\examplefig/%&/' > temp.tex
						mv temp.tex $i
					else
						echo "\examplefigv command cannot be changed in $i !!!"
					fi
				else
					echo $filei
				fi
			fi
			rm tmp.txt
			;;
	esac
done


#for i in ../ctrs/*.tex; do grep "^\\\examplefig[a-z]*$" $i >> zetest.txt; done 


# old remove small environment : sed 's/\\begin{small}//' | sed 's/\\end{small}//' 
# old remove hyperlink{DT...} : | sed 's/\$\\hyperlink{DT[^}]*}{\$\([^$]*\)\$}\$/\1/g' 
# old 		cat $i | sed "s/^\(\\\\index{signature.*\)\\\\small\(.*}\)$/\1\2/g" | sed 's/\\clearpage/\\end{ctrdesc}\\hrule\\begin{ctrdesc}/' | sed 's/hypertarget\({C.*P.*}\){}/label\1/' | sed '/label.*Pgraph/{
#N
#s/\(.*\)\n\(.*\)/\2\1/
#}
#' > tmp.tex; 

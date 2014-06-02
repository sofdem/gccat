#!/bin/bash

# create a TeX file containing the lists of constraints corresponding to given descriptors
# such as restrictions, arc generators, graph parameters, set generators
# The file is then included in Section "Legend for the description"

repctr=ctrs
ficctr=ctrstmp.txt
fic=preface.tex
fictmp=legendtmp.tex
if [ ! -f $fic ]; then echo "stop: no file $fic"; exit 1; fi

if [ ! -f $repctr/ctrs.tex ]; then echo "stop: no file $repctr/ctrs.tex for the list of constraints"; exit 1; fi
cat $repctr/ctrs.tex | sed 's/\\input{\(.*\)}$/\1/' > $ficctr

# get the TeX file name (tag IFWEBLEGEND in preface.tex)
ficinc=`cat $fic | sed -n "s/.*\\\\\\input{\(.*\.tex\).*IFWEBLEGEND.*/\1/p"`
if [ ! $ficinc ]; then ficinc="legend_inc.tex"; fi

# create a section for each descriptor type
cat $fic | sed -n '/IFWEBLEGEND/,/FIWEBLEGEND/{
s/^\\paragraph{\(.*\)}.*/\\section{\1}/p
s/^\\item \($.*\$\)[[:blank:]]*~~p\.~\\pageref{\([^}]*\)}.*/\\hyperlink{\2}{\1}/p
s/^\\item \(\\hyperlink{[^}]*}{[^}]*}\)[[:blank:]].*/\1/p
}
' > ${fictmp}

# special cases for MCLIQUE_comparison PRODUCT_comparison
cat $fictmp | sed '/MCLIQUE/{
N
s/\n\(.*_comparison\)/, \1/
}
' | sed '/PRODUCT/{
N
s/\n\(.*_comparison\)/, \1/
}
'> tmp.txt
 mv tmp.txt $fictmp

#start filling the TeX file
numsection=0
if [ -f $ficinc ]; then rm $ficinc; fi

## for each descriptor type
for line in `cat $fictmp | tr -s ' ' '%'`
do
	## read the descriptor type name
	if [ `echo $line | grep '^\\\section'` ]
	then
		numsection=`expr $numsection + 1`
		if [ $numsection -ge 2 ]; then echo '\end{itemize}'  >> $ficinc; fi
		echo $line | tr -s '%' ' ' >> $ficinc
		echo '\begin{itemize}'  >> $ficinc

	## read a descriptor label
	elif [ `echo $line | grep hyperlink` ]
	then
		echo "\\item $line" | tr -s '%' ' ' >> $ficinc

		label=`echo $line | sed -n 's/^\\\\hyperlink{\([^}]*\)}.*/\1/p'`

		## list all TeX files in ctrs referencing the label ~> tmp2.txt
		if [ `echo $label | grep -c restriction` -eq 1 ]
		then
			hypertarget=`cat $fic | sed -n "s/^\\\\\\item \\\\\\hypertarget{\([^}]*\)}.*\\\\\\label{$label}.*/\1/p"`
		else
			hypertarget=$label
		fi
			grep "\\\hyperlink{${hypertarget}}" ${repctr}/*.tex | cut -f1 -d':' | sort | uniq > tmp2prime.txt
			#head tmp2prime.txt
			comm -12 tmp2prime.txt $ficctr > tmp2.txt
			#head tmp2.txt
		#	index=`cat $fic | sed -n "s/.*\\\\\\index{\(.*\)@\(.*\)|indexdef}[[:blank:]]*\\\\\\label{$label}.*/\1@\2/p"`
		#	idlabel=`echo $index | cut -s -f1 -d'@' | sed 's/\(\\\\\)/\1\1/g'`
		#	grep "\\\index{${idlabel}@" ${repctr}/*.tex | cut -f1 -d':' | sort | uniq > tmp2.txt
		#fi
		# count the number of constraints
		number=`cat tmp2.txt | wc -l | tr -d ' '`
		echo $label $number
 		if [ $number -ge 1 ]
		then
 			if [ $number -eq 1 ]
			then echo "(1 constraint):" >> $ficinc
			else echo "($number constraints):" >> $ficinc
			fi
			# create the list of constraints
			echo '\begin{itemize}'  >> $ficinc
 			for file in `cat tmp2.txt`; do
 				labelctr=`cat $file | sed -n '1,/signbox/{
				s/\\\\hypertarget{\(.*\)}{}/\1/p
				}'`
 				ctr=`cat $file | sed -n 's/\\\\index.*@$\\\\constraint{\(.*\)}$.*|indexdef.*/\1/p' | head -1` # | sed 's/\(\\\\\)/\1\1/g'`
 				echo "\\item \\hyperlink{$labelctr}{\\ctrref{$ctr}}" >> $ficinc
 			done
			echo '\end{itemize}'  >> $ficinc
		fi
		rm tmp2.txt tmp2prime.txt
	fi
done
echo '\end{itemize}' >> ${ficinc}
rm $fictmp $ficctr

echo "file $ficinc created"

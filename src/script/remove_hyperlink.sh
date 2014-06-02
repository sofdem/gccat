#! /bin/bash

# for the constraint tex files given in argument
# remove the hyperlinks inside array environments
for f in $*
do
	echo "**********************************************"
	echo "remove hyperlinks in $f..."
	i=$f
	if [ ! -f $i ]
	then echo "warning: no file $i to process"
	else	
		cat $i | sed '/begin{array}/,/end{array}/{
s/\$\\hyperlink{[^}]*}{\\ctrref\(.*\)}\$/\\constraint\1/g
}
'  | sed '/begin{array}/,/end{array}/{
s/\$\\hyperlink{[^}]*}{\$\(.*\)\$}\$/\1/g
}
' > tmp.tex
		echo "diff :"
		diff tmp.tex $i
		case $? in
			0) echo "$f was not modified"
				rm tmp.tex;;
			1) echo "replace $f"
				mv tmp.tex $i;;
			*) echo "error in diff $f";;
		esac
	fi
done

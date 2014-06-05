#! /bin/bash


outrep=tikz

# take a tex file in argument extract each tikzpicture block and copy it in a file tiks/filebasename-imgnumber.tikz
# replace the block by \inputtikz{filebasename-imgnumber}
processfile() {
	texfile=$1
	if [ ! -f $texfile ]; then echo "stop: no $texfile file"; exit 1; fi
	basefile=`basename $texfile .tex`
	echo $basefile
	headfile=tmptikzhead.tex
	tailfile=tmptikztail.tex
	touch $headfile
	cp $texfile $tailfile

	imgnumber=1
	while true; do
		firstline=$(grep -n -m1 -e '^[^%]*\\begin{tikzpicture}' $tailfile | cut -d':' -f1)
		if [ "$firstline" ]
		then
			imgbasename=$basefile-$imgnumber-tikz
			imgname=$outrep/$imgbasename.tikz
			lastline=$(grep -n -m1 '^[^%]*\\end{tikzpicture}' $tailfile | cut -d':' -f1)
			if [ -z "$lastline" ]; then echo "error: begin without end"; exit 1; fi
			head -n $lastline $tailfile | tail -n +$firstline > $imgname
			head -n $((firstline - 1)) $tailfile >> $headfile
			echo "\inputtikz{$imgbasename}" >> $headfile
			tail -n +$((lastline + 1)) $tailfile > tmp
			mv tmp $tailfile
			((imgnumber++))
		else
			break
		fi
		done
		cat $tailfile >> $headfile
		mv $headfile $texfile
		rm $tailfile
		if [ $(grep -n -m1 -e '^[^%]*{tikzpicture}' $texfile) ]; then echo "VERIFY $texfile !!!!!!!!!!!!!!!!!!!!!!!!!"; fi
}


#generate images
tikz2png() {
	compilerep=texinput
	nbimgs=0
	if [ ! -d $compilerep ]; then echo "error: no $compilerep directory"; exit 1; fi
	cd $compilerep
	for i in `ls ../$outrep/*.tikz`
	do
		basetikz=`basename $i .tikz`
		echo $basetikz
		basetex=`echo $basetikz | cut -d'-' -f1`
		ctrfile=../../catalog/ctrs/$basetex.tex
		if [ ! -f $ctrfile ]
		then 
			ctrfile=../../catalog/$basetex.tex
			if [ ! -f $ctrfile ]; then echo "error: no $ctrfile file"; exit 1; fi
		fi
		#if [ ! -f ../$outrep/$basetikz.png ]
		if [ ! -f $basetikz.tex -a  ! -f ../$outrep/$basetikz.png ]
		then 
			cp tikz-template.tex $basetikz.tex
			grep '\\newcommand' $ctrfile | sort | uniq >> $basetikz.tex
			echo "\input{$i}" >> $basetikz.tex
			echo "\end{document}" >> $basetikz.tex
			latex -halt-on-error -file-line-error -shell-escape $basetikz.tex
			if [ $? -eq 0 ]
			then 
				rm $basetikz.tex
			fi
			if [ -f $basetikz-1.png ]
			then
				mv $basetikz-1.png ../$outrep/$basetikz.png
				((nbimgs++))
			fi
			rm -f $basetikz.aux $basetikz.log $basetikz.dvi $basetikz.ps

		fi
	done
	cd ..
	echo $nbimgs "images generated"
}

processallfiles() {
	if [ ! -d $outrep ]; then mkdir $outrep; fi
	inrep="../catalog"
	processfile $inrep/preface.tex
	for i in $inrep/ctrs/*.tex
	do
		processfile $i
	done
	echo `ls $outrep/*.tikz | wc -l` "tikz files generated"
}


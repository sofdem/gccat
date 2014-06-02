#!/bin/bash

ctrsrep=$1
base=$2

tpl=script/templatefigps.tex
if [ ! -f $tpl ]; then echo "stop: no template file $tpl"; exit 1; fi

if [ ! -f $ctrsrep/$base.pstex -o ! -f $ctrsrep/$base.pstex_t ]
then echo "no pstex file for $base"
else
	cp $ctrsrep/$base.pstex ./
	cat $ctrsrep/$base.pstex_t | sed "s%ctrs/$base%$base%" > $base.pstex_t
	cat $tpl | sed "s/#1/$base/g" > $base.tex
	latex $base.tex
	dvips -E $base.dvi -o $base.eps
#	gs -q -dBATCH -dNOPAUSE -sDEVICE=jpeg -dSAFER -dEPSCrop -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -r200 -sOutputFile=$base.jpg $base.eps
	rm $base.log $base.aux $base.dvi $base.tex $base.pstex_t $base.pstex
fi

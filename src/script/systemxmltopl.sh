#!/bin/sh

pl=$1
xml=$2
zedate=`date +"%Y-%m-%d"`

echo '<?xml version="1.0" encoding="UTF-8" ?>' > $xml
echo '<?xml-stylesheet href="gccat_systems.xslt" type="text/xsl"?>' >> $xml
echo '<catalog date="'$zedate'">' >> $xml

for l in `cat $pl | tr -s ' ' '@' | sed "s/</\&lt;/g" | sed "s/>/\&gt;/g"` ; do
	ctr=`echo $l | cut -f1 -d',' | cut -f2 -d'('`
	echo '<constraint name="'$ctr'">' >> $xml
	urlcat=`echo $l | cut -f2 -d',' | tr -d '@' | tr -d "'"`
	for s in `echo $l | cut -f2 -d'[' | cut -f1 -d']' | tr -s ',' ' '`; do
		echo $s | grep -q http
		if [ $? -ne 0 ] 
		then
			sys=`echo $s | cut -f1 -d'('`
			echo '<system name="'$sys'">' >> $xml
			ctrsys=`echo $s | cut -f3 -d'('`
			echo '<ctr name="'$ctrsys'"' >> $xml
		else
			urlsys=`echo $s | cut -f2 -d"'"`
			echo 'url="'$urlsys'"/>' >> $xml
			echo '</system>' >> $xml
		fi
	done
	echo '</constraint>' >> $xml
done
echo '</catalog>' >> $xml

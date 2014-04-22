#!/bin/bash
mydir=/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/
cd $mydir

#get most recent
MYFILE=`ls | grep "contacts.*\.zip$" | tail -1`

MYFILE2=`ls | grep "messages.*\.zip$" | tail -1`


echo $MYFILE
if [ ! -f $MYFILE ]; 
then
	echo "The script failed";
	exit;
else
	echo "Let's keep chugging"
fi
now=$(date +"%Y"_"%m"_"%d"_"%H":"%M")

unzip $MYFILE -d $mydir/contact_$now
unzip $MYFILE2 -d $mydir/message_$now

Rscript ErrorChecksViz.r

#TEXDIR=`ls | grep "tex_2014-[0-9].*$" | tail -1`
#TEXFILE=`ls "$TEXDIR" | grep "VIPTexTemplate" | tail -1`
#cd "$TEXDIR"
#xetex $TEXFILE
#cd ..

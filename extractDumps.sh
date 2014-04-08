#!/bin/bash
mydir=/Volumes/Optibay-1TB/RSA_RCT/QA/LiveData/VIP-LivedData/
cd $mydir

#get most recent
MYFILE=`ls | grep "\.zip$" | tail -1`

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


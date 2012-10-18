#!/bin/bash

# usage: snapshotlogs.sh {repodir} {prefix} {wherethescriptslive} {s3bucketname} {nginxlogdir}
# example: snapshotlogs.sh /home/prod/kontrakcja kontrakcja-logs /home/prod/kontrackja skrivapa-snapshots /var/log/nginx/scrive.com

tocopy=$1
prefix=$2
repo=$3
bucket=$4
nginxlogs=$5

# this has to match what logrotate uses
dateext=`date "+%Y%m%d"`
date=`date -u "+%Y-%m-%d-%H-%M-%S%z"`
zipfile=$prefix-$date.tar.gz
cd /tmp

echo "Zipping logs"
tar zcf "$zipfile"                    \
    $tocopy/log/*.log-$dateext.gz     \
    $nginxlogs/*.log-$dateext.gz
ls -lh "$zipfile"

echo "Generating signature hash"
hashdoc=loghash-$USER-$date.txt
m=`md5sum $zipfile | awk 'BEGIN { FS = " +" } ; { print $1 }'`
echo "SkrivaPa Log Snapshot"  >  "$hashdoc"
echo "Date: $date"            >> "$hashdoc"
echo "Filename: $zipfile"     >> "$hashdoc"
echo "MD5SUM: $m"             >> "$hashdoc"

echo "Signing files with GuardTime"
gtime-sign $zipfile $hashdoc

echo "Building archive for upload"
finalfile=kontrakcja-$USER-signed-$date.tar.gz
tar zcf "$finalfile" "$hashdoc" "$zipfile" "$hashdoc.gtts" "$zipfile.gtts"

# push to amazon
echo "Pushing to amazon"
s3cmd --acl-private put "$finalfile" s3://$bucket

# clean up
rm "$zipfile" "$finalfile" "$hashdoc" "$hashdoc.gtts" "$zipfile.gtts"
exit 0

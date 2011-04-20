#!/bin/bash

#repo=production
repo=/home/admin/staging
date=`date -u "+%Y%m%d%I%M%S%z"`
filename=kontrakcja-snapshot-$date
zipfile=$filename.tar.gz
cd /tmp
#echo "Copying repo"
#cp -r $repo $filename
#echo "Deleting unneeded files"
#rm -rf $filename/_local
#rm -rf $filename/_darcs
#rm -rf $filename/dist
#rm -rf $filename/selenium-test
#rm -f $filename/*.dll
#rm -f $filename/*.exe
echo "Zipping repo"
tar zcf $zipfile --exclude=_local* --exclude=_darcs* --exclude=dist* --exclude=selenium-test* --exclude=*.dll --exclude=*.exe --exclude=log* --exclude=.hpc* $repo
ls -lh $zipfile
#sign with trustweaver
echo "Building soap message"
echo "Multipart MIME"
python $repo/scripts/genmime.py < $zipfile
echo "Constructing SOAP Message"
cat $repo/scripts/top mime.txt $rep/scripts/bottom > soaprequest.xml
twcert=$repo/certs/credentials.pem
twcertpwd=jhdaEo5LLejh
twurl=https://tseiod.trustweaver.com/ts/svs.asmx
echo "Signing with trustweaver"
curl -X POST --verbose --show-error \
    --cert $twcert:$twcertpwd --cacert $twcert \
    --data-binary @/tmp/soaprequest.xml \
    -H "Content-Type: text/xml; charset=UTF-8" \
    -H "Expect: 100-continue" \
    -H "SOAPAction: http://www.trustweaver.com/tsswitch#Sign" \
    -o "soapresponse.xml" \
    $twurl
echo "Parsing XML"
# do it! (output to /tmp/signed.b64)
python $repo/scripts/parsesignresponse.py < soapresponse.xml
echo "Decoding base64 and rezipping"
finalfile=kontrakcja-signed-$date.tar.gz
base64 -d signed.b64 | tar zcf finalfile
#push to amazon
echo "Pushing to amazon"
s3cmd --acl-private put $finalfile s3://skrivapa-snapshots
#check hash from amazon
echo "Checking amazon md5 sum"
md5amazon=`s3cmd info s3://skrivapa-snapshots/$zipfile|grep MD5|awk "{print $3}"`
echo $md5amazon
md5local=`md5sum $finalfile | awk 'BEGIN { FS = " +" } ; { print $1 }'`
if [$md5amazon==$md5local]
then
    echo "MD5 sum matches!"
    #clean up
    rm -r $filename
    rm $zipfile
    rm $finalfile
    exit 0
fi
echo "Something went wrong. Try again."

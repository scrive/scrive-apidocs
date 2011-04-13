#!/bin/bash

#repo=production
repo=/home/eric/haskell/kontrakcja
filename=kontrakcja-snapshot-`date -u "+%Y%m%d%k%M%S%z"`
zipfile=$filename.tar.gz
cd /tmp
echo "Copying repo"
cp -r $repo $filename
rm -rf $filename/_local
rm -rf $filename/_darcs
rm -rf $filename/dist
rm -rf $filename/selenium-test
rm -f $filename/*.dll
rm -f $filename/*.exe
echo "Zipping repo"
tar zcf $zipfile $filename
ls -lh $zipfile
exit 0
#sign with trustweaver
twcert=/home/eric/haskell/kontrakcja/certs/credentials.pem
twcertpwd=jhdaEo5LLejh
twurl=https://tseiod.trustweaver.com/ts/svs.asmx
echo "Signing with trustweaver"
curl -X POST -k --verbose --show-error \
    --cert $twcert:$twcertpwd --cacert $twcert \
    --data-binary $zipfile \
    -H "Content-Type: text/xml; charset=UTF-8" \
    -H "Expect: 100-continue" \
    -H "SOAPAction: http://www.trustweaver.com/tsswitch#Sign" \
    $twurl
#push to amazon
echo "Pushing to amazon"
s3cmd --acl-private put $zipfile s3://skrivapa-snapshots
#check hash from amazon
echo "Checking amazon md5 sum"
md5amazon=`s3cmd info s3://skrivapa-snapshots/$zipfile|grep MD5|awk "{print $3}"`
echo $md5amazon
md5local=`md5sum $zipfile | awk 'BEGIN { FS = " +" } ; { print $1 }'`
if [$md5amazon==$md5local]
then
    echo "MD5 sum matches!"
    #clean up
    rm -r $filename
    rm $zipfile
    exit 0
fi
echo "Something went wrong. Try again."

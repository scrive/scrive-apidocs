#!/bin/bash

#repo=production
repo=/home/eric/haskell/kontrakcja
filename=kontrakcja-snapshot-`date -u "+%Y%m%d%k%M%S%z"`
cd /tmp
echo "Copying repo"
cp -r $repo $filename
rm -rf $filename/_local
rm -rf $filename/_darcs
rm -rf $filename/dist
echo "Zipping repo"
tar zcf $filename.tar.gz $filename
ls -lh $filename.tar.gz
exit 0
#sign with trustweaver
twcert=/home/eric/haskell/kontrakcja/certs/credentials.pem
twcertpwd=jhdaEo5LLejh
twurl=https://tseiod.trustweaver.com/ts/svs.asmx
echo "Signing with trustweaver"
curl -X POST -k --verbose --show-error \
    --cert $twcert:$twcertpwd --cacert $twcert \
    --data-binary $filename.tar.gz \
    -H "Content-Type: text/xml; charset=UTF-8" \
    -H "Expect: 100-continue" \
    -H "SOAPAction: http://www.trustweaver.com/tsswitch#Sign" \
    $twurl
#push to amazon
echo "Pushing to amazon"
s3cmd --acl-private put $filename.tar.gz s3://skrivapa-snapshots
#check hash from amazon
echo "Checking amazon md5 sum"
md5amazon=`s3cmd info s3://skrivapa-snapshots/$filename.tar.gz|grep MD5|awk "{print $3}"`
echo $md5amazon
md5local=`md5sum $filename.tar.gz | awk 'BEGIN { FS = " +" } ; { print $1 }'`
if [$md5amazon==$md5local]
then
    echo "MD5 sum matches!"
    #clean up
    rm -r $filename
    rm $filename.tar.gz
    exit 0
fi
echo "Something went wrong. Try again."

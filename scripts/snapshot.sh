#!/bin/bash

#repo=production
repo=/home/eric/haskell/kontrakcja
filename=kontrakcja-snapshot-`date -u "+%Y%m%d%k%M%S%z"`
cd /tmp
cp -r $repo $filename
rm -rf $filename/_local
tar zcf $filename.tar.gz $filename
#sign with trustweaver
twcert=/
twcertpwd=abc
twurl=http://
curl -X POST -k --silent --show-error \
    --cert $twcert:$twcertpwd --cacert $twcert \
    --data-binary $filename.tar.gz \
    -H "Content-Type: text/xml; charset=UTF-8" \
    -H "Expect: 100-continue" \
    -H "SOAPAction: http://www.trustweaver.com/tsswitch#Sign" \
    $twurl
#push to amazon

#check hash from amazon
#clean up
rm -r $filename
# don't delete file unless you can prove it's in amazon server
#rm $filename.tar.gz

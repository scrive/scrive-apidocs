#!/bin/bash

repo=$1
prefix=$2
date=`date -u "+%Y-%m-%d-%H-%M-%S%z"`
zipfile=$prefix-$date.tar.gz
cd /tmp
echo "Zipping repo"
tar zcf "$zipfile"                    \
    --exclude=_local*                 \
    --exclude=_darcs*                 \
    --exclude=dist*                   \
    --exclude=selenium-test*          \
    --exclude=*.dll                   \
    --exclude=*.exe                   \
    --exclude=log*                    \
    --exclude=*.log                   \
    --exclude=.hpc*                   \
    --exclude=_locakal_ticket_backup* \
    $repo/
ls -lh "$zipfile"

echo "Generating signature hash"
hashdoc=hash-$date.txt
m=`md5sum $zipfile | awk 'BEGIN { FS = " +" } ; { print $1 }'`
echo "SkrivaPa Code Snapshot" >  "$hashdoc"
echo "Date: $date"            >> "$hashdoc"
echo "Filename: $zipfile"     >> "$hashdoc"
echo "MD5SUM: $m"             >> "$hashdoc"

#sign with trustweaver
echo "Building soap message"
echo "Multipart MIME"
mimefile=hash-$date.mime
python $repo/scripts/genmime.py "$hashdoc" "$mimefile"
echo "Constructing SOAP Message"
soaprequest=request-$date.xml
base64 "$hashdoc" | cat $repo/scripts/top - $repo/scripts/bottom > "$soaprequest"
twcert=$repo/certs/credentials.pem
twcertpwd=jhdaEo5LLejh
twurl=https://tseiod.trustweaver.com/ts/svs.asmx
echo "Signing with trustweaver"
soapresponse=response-$date.xml
curl -X POST --verbose --show-error                           \
    --cert $twcert:$twcertpwd --cacert $twcert                \
    --data-binary "@$soaprequest"                             \
    -H "Content-Type: text/xml; charset=UTF-8"                \
    -H "Expect: 100-continue"                                 \
    -H "SOAPAction: http://www.trustweaver.com/tsswitch#Sign" \
    -o "$soapresponse"                                        \
    $twurl
echo "Parsing XML"
signed64=signed-$date.b64
python $repo/scripts/parsesignresponse.py "$soapresponse" "$signed64"
echo "Decoding base64 and rezipping"
finalfile=kontrakcja-signed-$date.tar.gz
signedmime=signed-$date.mime
base64 -d "$signed64" > "$signedmime"
tar zcf "$finalfile" "$signedmime" "$zipfile"
#push to amazon
echo "Pushing to amazon"
s3cmd -c "$repo/scripts/s3cfg" --acl-private put "$finalfile" s3://skrivapa-snapshots
#check hash from amazon
echo "Checking amazon md5 sum"
md5amazon=`s3cmd -c "$repo/scripts/s3cfg" info "s3://skrivapa-snapshots/$finalfile" |grep MD5|awk "{print $3}"`
echo $md5amazon
md5local=`md5sum "$finalfile" | awk 'BEGIN { FS = " +" } ; { print $1 }'`
if [ "$md5amazon" = "$md5local" ]
then
    echo "MD5 sum matches!"
    #clean up
    rm "$zipfile" "$finalfile" "$hashdoc" "$mimefile" "$soaprequest" "$soapresponse" "$signed64" "$signedmime"
    exit 0
fi
echo "Something went wrong. Try again."

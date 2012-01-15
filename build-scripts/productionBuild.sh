#!/bin/sh -e

# This script assumes the existence of BUILD_NUMBER from TeamCity

BUILD_DATE=`date "+%Y-%m-%d %H:%M:%S"`
BUILD_VCS_NUMBER=`git log -1 --pretty=oneline|awk '{print $1;}'`

#build-scripts/runCleanCompile.sh
#build-scripts/runAllUnitTests.sh > test-report.txt

BUILD_ID=$BUILD_DATE"."$BUILD_NUMBER"."$BUILD_VCS_NUMBER

ZIP=$BUILD_ID".production.tar.gz"
#DIR=/home/prod/kontrakcja
DIR=/home/eric/haskell/kontrakcja

echo "Creating zip file"
cd /tmp
tar zcf "$zipfile"                    \
    --exclude=.git*                   \
    --exclude=_local*                 \
    --exclude=_darcs*                 \
    --exclude=.hpc*                   \
    --exclude=_locakal_ticket_backup* \
    $DIR/
ls -lh "$ZIP"

echo "Generating signature hash"
hashdoc=hash-$BUILD_ID.txt
m=`md5sum "$ZIP" | awk 'BEGIN { FS = " +" } ; { print $1 }'`
echo "Scrive Production Build"         >  "$hashdoc"
echo "--------------------------------">> "$hashdoc"
echo "Build_ID:     $BUILD_ID"         >> "$hashdoc"
echo "Date:         $BUILD_DATE"       >> "$hashdoc"
echo "Build Number: $BUILD_NUMBER"     >> "$hashdoc"
echo "Commit ID:    $BUILD_VCS_NUMBER" >> "$hashdoc"
echo "Filename:     $ZIP"              >> "$hashdoc"
echo "MD5SUM:       $m"                >> "$hashdoc"
# TODO: Also put in md5 sums of all binaries

#SIGN WITH TRUSTWEAVER

echo "Building soap message"
echo "Multipart MIME"
mimefile=hash-$BUILD_ID.mime
python $DIR/scripts/genmime.py "$hashdoc" "$mimefile"
echo "Constructing SOAP Message"
soaprequest=request-$BUILD_ID.xml
base64 "$hashdoc" | cat $DIR/scripts/top - $DIR/scripts/bottom > "$soaprequest"

# For https authentication of Trustweaver
twcert=$DIR/certs/credentials.pem
twcertpwd=jhdaEo5LLejh
twurl=https://tseiod.trustweaver.com/ts/svs.asmx

echo "Signing with trustweaver"
soapresponse=response-$BUILD_ID.xml
curl -X POST --verbose --show-error                           \
    --cert $twcert:$twcertpwd --cacert $twcert                \
    --data-binary "@$soaprequest"                             \
    -H "Content-Type: text/xml; charset=UTF-8"                \
    -H "Expect: 100-continue"                                 \
    -H "SOAPAction: http://www.trustweaver.com/tsswitch#Sign" \
    -o "$soapresponse"                                        \
    $twurl

echo "Parsing XML"
signed64=signed-$BUILD_ID.b64
python $DIR/scripts/parsesignresponse.py "$soapresponse" "$signed64"

echo "Decoding base64"
finalfile=$BUILD_ID.production.enhanced.tar.gz
signedmime=$BUILD_ID.signature.mime
base64 -d "$signed64" > "$signedmime"

echo "Creating final enhanced deployment file"
tar zcf "$finalfile" "$signedmime" "$ZIP"

# TODO Back up $finalfile

# TODO Print out path to amazon

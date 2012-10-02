#!/bin/bash -e

# This script was used before October 1, 2012

# This script assumes the existence of BUILD_NUMBER from TeamCity
# This script assumes the existence of TWPASSWORD as the password for trustweaver certificates
# This script assumes the existence of PRIVATEKEY as the location of private key file
# This script assumes TMP which is the directory as a temporary workspace
# example:
#DIR=/home/eric/haskell/kontrakcja

echo "BUILD_NUMBER: "$BUILD_NUMBER
echo "TMP: "$TMP
echo "PRIVATEKEY: "$PRIVATEKEY

#cd $DIR

BUILD_DATE=`date "+%Y-%m-%d-%H-%M-%S"`
#BUILD_VCS_NUMBER=`git log -1 --pretty=oneline|awk '{print $1;}'`

sh build-scripts/runCleanCompile.sh

echo "Computing checksums of all binaries"

rm -rf checksums
mkdir checksums

find dist/build -executable -type f -exec sh -c 'sha512sum {} > checksums/`basename {}`.sha512' \;

echo "Running unit tests"
sh build-scripts/runAllUnitTests.sh > test-report.txt

BUILD_ID=$BUILD_DATE"."$BUILD_NUMBER"."$BUILD_VCS_NUMBER

ZIP="$BUILD_ID".".production.tar.gz"

echo "Creating zip file"

tar zcf "$TMP/$ZIP"                        \
    --exclude=.git*                   \
    --exclude=_local*                 \
    --exclude=_darcs*                 \
    --exclude=_locakal_ticket_backup* \
    *
ls -lh "$TMP/$ZIP"

openssl dgst -sha256 -sign $PRIVATEKEY -out "$TMP/signature.256" "$TMP/$ZIP"

echo "Generating signature hash"
hashdoc="$TMP/hash-$BUILD_ID.txt"
m=`sha512sum "$TMP/$ZIP" | awk 'BEGIN { FS = " +" } ; { print $1 }'`
echo "Scrive Production Build"         >  "$hashdoc"
echo "--------------------------------">> "$hashdoc"
echo "Build_ID:     $BUILD_ID"         >> "$hashdoc"
echo "Date:         $BUILD_DATE"       >> "$hashdoc"
echo "Build Number: $BUILD_NUMBER"     >> "$hashdoc"
echo "Commit ID:    $BUILD_VCS_NUMBER" >> "$hashdoc"
echo "Filename:     $ZIP"              >> "$hashdoc"
echo "SHA512SUM:    $m"                >> "$hashdoc"

echo ""                                 >> "$hashdoc"
echo "SHA512SUMS of Binaries"           >> "$hashdoc"
echo "--------------------------------" >> "$hashdoc"

find dist/build -executable -type f -exec sha512sum {} \; >> "$hashdoc"

echo "------END------" >> "$hashdoc"

echo "Building soap request for Trustweaver signing"

echo "Multipart MIME"
mimefile="$TMP/hash-$BUILD_ID.mime"
python scripts/genmime.py "$hashdoc" "$mimefile"

echo "Constructing SOAP Message"
soaprequest="$TMP/request-$BUILD_ID.xml"
base64 "$hashdoc" | cat scripts/top - scripts/bottom > "$soaprequest"

# For https authentication of Trustweaver
twcert=certs/credentials.pem
twcertpwd=$TWPASSWORD
twurl=https://tseiod.trustweaver.com/ts/svs.asmx

echo "Signing with trustweaver"
soapresponse="$TMP/response-$BUILD_ID.xml"
curl -X POST --verbose --show-error                           \
    --cert $twcert:$twcertpwd --cacert $twcert                \
    --data-binary "@$soaprequest"                             \
    -H "Content-Type: text/xml; charset=UTF-8"                \
    -H "Expect: 100-continue"                                 \
    -H "SOAPAction: http://www.trustweaver.com/tsswitch#Sign" \
    -o "$soapresponse"                                        \
    $twurl

echo "Parsing XML response"
signed64="$TMP/signed-$BUILD_ID.b64"
python scripts/parsesignresponse.py "$soapresponse" "$signed64"

echo "Decoding base64 response"
finalfile="$BUILD_ID.production.enhanced.tar.gz"
signedmime=$BUILD_ID.signature.mime
base64 -d "$signed64" > "$TMP/$signedmime"

echo "Creating final enhanced deployment file"

cd $TMP
tar zcf "$finalfile" "$signedmime" "$ZIP" signature.sha256
cd -
ls -lh "$TMP/$finalfile"

echo "Pushing to amazon"
s3cmd --config=/home/builds/.s3cfg --acl-private put "$TMP/$finalfile" s3://production-builds

echo "Checking amazon md5 sum"
md5amazon=`s3cmd --config=/home/builds/.s3cfg info "s3://production-builds/$finalfile" | grep MD5 | awk '{print $3}'`
echo "MD5SUM from Amazon S3: "$md5amazon
md5local=`md5sum "$TMP/$finalfile" | awk 'BEGIN { FS = " +" } ; { print $1 }'`
echo "MD5SUM from local    : "$md5local
if [ "$md5amazon" = "$md5local" ]
then
    echo "MD5 sum matches!"
else
    echo "MD5 sum does not match. Please try again."
    exit 1
fi

echo "s3://production-builds/$finalfile"

exit 0

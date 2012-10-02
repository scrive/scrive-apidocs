#!/bin/bash -e

# This script assumes the existence of BUILD_NUMBER from TeamCity
# This script assumes TMP which is the directory as a temporary workspace
# example:
#DIR=/home/eric/haskell/kontrakcja

BUILD_DATE=`date "+%Y-%m-%d-%H-%M-%S"`
#BUILD_VCS_NUMBER=`git log -1 --pretty=oneline|awk '{print $1;}'`

BUILD_ID=$BUILD_DATE"."$BUILD_NUMBER"."$BUILD_VCS_NUMBER

echo "BUILD ID: "$BUILD_ID
echo "TMP: "$TMP

echo "Building Clean"
sh build-scripts/runCleanCompile.sh > build-report.txt

echo "Running unit tests"
sh build-scripts/runAllUnitTests.sh > test-report.txt

ZIP="$BUILD_ID"".production.tar.gz"

echo "Creating zip file"

tar zcf "$TMP/$ZIP"                        \
    --exclude=.git*                   \
    --exclude=_local*                 \
    --exclude=_darcs*                 \
    --exclude=*.conf \
    --exclude=log \
    *
ls -lh "$TMP/$ZIP"

cd $TMP

opensslfile="$BUILD_ID.signature.sha256"
signaturefile="$BUILD_ID.signature.gtts"
finalfile="$BUILD_ID.production.enhanced.tar.gz"

echo "Signing with private key"
openssl dgst -sha256 -sign "/home/builds/key/builds.scrive.com.key" -out "$opensslfile" "$TMP/$ZIP"

echo "Signing with GuardTime"
gtime -s -f "$TMP/$ZIP" -o "$signaturefile"

echo "Creating final enhanced deployment file"
tar zcf "$finalfile" "$signaturefile" "$opensslfile" "$ZIP"
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

echo ""
echo "Backed-up file on Amazon:"
echo "s3://production-builds/$finalfile"

echo ""

echo "Copying deployment file to /tmp on production server"
ssh builds@prod.scrive.lan 'rm -rf /tmp/deployment && mkdir /tmp/deployment'
scp "$TMP/$finalfile" builds@prod.scrive.lan:/tmp/deployment/.
scp "/home/builds/key/builds.scrive.com.pubkey.pem" builds@prod.scrive.lan:/tmp/deployment/.

echo "Verifying and unzipping deployment file"
ssh builds@prod.scrive.lan "cd /tmp/deployment && tar -zxf $finalfile && gtime -v -f $ZIP -i $signaturefile && openssl dgst -sha256 -verify builds.scrive.com.pubkey.pem -signature $opensslfile $ZIP && mkdir kontrakcja && tar -C kontrakcja -zxf $ZIP ; exit \$?"

echo "Deployed to /tmp/deployment on Production server. Deployment file has been verified."

exit 0

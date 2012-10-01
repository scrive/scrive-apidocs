#!/bin/bash -e

# This script assumes the existence of BUILD_NUMBER from TeamCity
# This script assumes TMP which is the directory as a temporary workspace
# example:
#DIR=/home/eric/haskell/kontrakcja

echo "BUILD_NUMBER: "$BUILD_NUMBER
echo "TMP: "$TMP

BUILD_DATE=`date "+%Y-%m-%d-%H-%M-%S"`
#BUILD_VCS_NUMBER=`git log -1 --pretty=oneline|awk '{print $1;}'`

sh build-scripts/runCleanCompile.sh

echo "Running unit tests"
sh build-scripts/runAllUnitTests.sh > test-report.txt

BUILD_ID=$BUILD_DATE"."$BUILD_NUMBER"."$BUILD_VCS_NUMBER

ZIP="$BUILD_ID".".production.tar.gz"

echo "Creating zip file"

tar zcf "$TMP/$ZIP"                        \
    --exclude=.git*                   \
    --exclude=_local*                 \
    --exclude=_darcs*                 \
    *
ls -lh "$TMP/$ZIP"

cd $TMP

finalfile="$BUILD_ID.production.enhanced.tar.gz"
signaturefile="$BUILD_ID.signature.gtts"

echo "Signing with GuardTime"
gtime -s -f "$TMP/$ZIP" -o "$signaturefile"

echo "Creating final enhanced deployment file"
tar zcf "$finalfile" "$signaturefile" "$TMP/$ZIP"
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

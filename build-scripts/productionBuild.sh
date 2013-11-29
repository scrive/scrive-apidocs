#!/bin/bash -e

# This script assumes the existence of BUILD_NUMBER from TeamCity
# This script assumes TMP which is the directory as a temporary workspace
# This script assumes SRV which is the name of the server (ie, production, staging)
# This script assumes TRGMH which is ssh string to target server (builds@prod.scrive.lan)
# This script assumes TRGMH2 which is ssh string to target secondary server (builds@staging.scrive.lan)
# This script assumes SRV2 which is the name of the secondary server (api-testbed)
# This script assumes AMZN which is a boolean whether to upload to Amazon

if [ -z "$1" ]; then
  echo "Usage: $0 <cabal-dev sandbox directory path>"
  exit 1
fi

BUILD_DATE=`date "+%Y-%m-%d-%H-%M-%S"`
#BUILD_VCS_NUMBER=`git log -1 --pretty=oneline|awk '{print $1;}'`

BUILD_ID=$BUILD_DATE"."$BUILD_NUMBER"."$BUILD_VCS_NUMBER

echo "BUILD ID: "$BUILD_ID
echo "TMP: "$TMP
echo "Server: "$SRV
echo "Target server: "$TRGMH

echo "Building Clean"
./build-scripts/runCleanCompile.sh "$1" > build-report.txt

echo "Running unit tests"
./build-scripts/runAllUnitTests.sh > test-report.txt

ZIP="$BUILD_ID.$SRV.tar.gz"

echo "Creating zip file"

tar zcf "$TMP/$ZIP"                              \
    --exclude=.git*                              \
    --exclude=_local*                            \
    --exclude=_darcs*                            \
    --exclude=log                                \
    --exclude=pdfsealjava/itextpdf-5.4.2-sources \
    --exclude=dist/build/*/*-tmp                 \
    *
ls -lh "$TMP/$ZIP"

cd $TMP

opensslfile="$BUILD_ID.signature.sha256"
signaturefile="$BUILD_ID.signature.gtts"
finalfile="$BUILD_ID.$SRV.enhanced.tar.gz"

echo "Signing with private key"
openssl dgst -sha256 -sign "/home/builds/key/builds.scrive.com.key" -out "$opensslfile" "$TMP/$ZIP"

echo "Signing with GuardTime"
gtime -s -f "$TMP/$ZIP" -o "$signaturefile"

echo "Creating final enhanced deployment file"
tar zcf "$finalfile" "$signaturefile" "$opensslfile" "$ZIP"

cd -
ls -lh "$TMP/$finalfile"

if [ $AMZN -ne 0 ]
then
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
fi

echo "Copying deployment file to /tmp on $SRV server"
ssh $TRGMH  "rm -rf /tmp/"$SRV"_deployment && mkdir /tmp/"$SRV"_deployment"
cat "$TMP/$finalfile" | ssh $TRGMH  "cd /tmp/"$SRV"_deployment && tar -zx"
scp "/home/builds/key/builds.scrive.com.pubkey.pem" $TRGMH":/tmp/"$SRV"_deployment/."

echo "Verifying and unzipping deployment file"
ssh $TRGMH  "cd /tmp/"$SRV"_deployment && gtime -v -f $ZIP -i $signaturefile && openssl dgst -sha256 -verify builds.scrive.com.pubkey.pem -signature $opensslfile $ZIP ; exit \$?"

echo "Deployed to /tmp/"$SRV"_deployment on $SRV server. Deployment file has been verified."

if [ ! -z "$SRV2" ]; then
  if [! -z "$TRGMH2"]; then

   echo "Copying deployment file to /tmp on $SRV2 server"
   ssh $TRGMH2 "rm -rf /tmp/"$SRV2"_deployment && mkdir /tmp/"$SRV2"_deployment"
   cat "$TMP/$finalfile" | ssh $TRGMH2 "cd /tmp/"$SRV2"_deployment && tar -zx ; exit \$?"
  fi
fi

exit 0

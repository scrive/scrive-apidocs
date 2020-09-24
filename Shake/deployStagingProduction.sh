#!/bin/bash -xe

# This script assumes that file _build/kontrakcja.tar.gz exists (i.e. built previously)
# This script assumes the existence of BUILD_NUMBER from TeamCity
# This script assumes TMP which is the directory as a temporary workspace
# This script assumes SRV which is the name of the server (ie, production, staging)
# This script assumes TRGMH which is ssh string to target server (builds@prod.scrive.lan)
# This script assumes AMZN which is a boolean whether to upload to Amazon

export BUILD_DATE="${BUILD_DATE:-`date "+%Y-%m-%d-%H-%M-%S"`}"
BUILD_ID=$BUILD_DATE"."$BUILD_NUMBER"."$BUILD_VCS_NUMBER
ZIP="$BUILD_ID.$SRV.tar.gz"
echo "BUILD ID: "$BUILD_ID
echo "TMP: "$TMP
echo "ZIP: "$ZIP
echo "Server: "$SRV
echo "Target server: "$TRGMH

cp _build/kontrakcja.tar.gz $TMP/$ZIP
ls -lh $TMP/$ZIP
cd $TMP

opensslfile="$BUILD_ID.signature.sha256"
signaturefile="$BUILD_ID.signature.gtts"
finalfile="$BUILD_ID.$SRV.enhanced.tar.gz"
gtsigningurl="http://internal-gt-signer-848430379.eu-west-1.elb.amazonaws.com:8080/gt-signingservice"
gtextendingurl="http://internal-gt-extender-2081608339.eu-west-1.elb.amazonaws.com:8081/gt-extendingservice"

echo "Signing with private key"
openssl dgst -sha256 -sign "$HOME/key/builds.scrive.com.key" -out "$opensslfile" "$TMP/$ZIP"

# echo "Signing with GuardTime"
# gtime -S $gtsigningurl -s -f "$TMP/$ZIP" -o "$signaturefile"

echo "Creating final enhanced deployment file"
tar zcf "$finalfile" "$opensslfile" "$ZIP"

cd -
ls -lh "$TMP/$finalfile"

if [ $AMZN -ne 0 ]
then
  echo "Pushing to amazon"
  s3cmd --config=$HOME/.s3cfg --acl-private put "$TMP/$finalfile" s3://production-builds

  # Checking of MD5 sums temporarily disabled as s3cmd lost the ability to return proper MD5 sum from Amazon.
  #
  # Longer explanation: s3cmd info get 'ETag' and treats it as MD5. It
  # was like this, now something has changed either in S3 or in s3cmd
  # and it does not work.
  #
  # echo "Checking amazon md5 sum"
  # md5amazon=`s3cmd --config=/home/builds/.s3cfg info "s3://production-builds/$finalfile" | grep MD5 | awk '{print $3}'`
  # echo "MD5SUM from Amazon S3: "$md5amazon
  # md5local=`md5sum "$TMP/$finalfile" | awk 'BEGIN { FS = " +" } ; { print $1 }'`
  # echo "MD5SUM from local    : "$md5local
  # if [ "$md5amazon" = "$md5local" ]
  # then
  #     echo "MD5 sum matches!"
  # else
  #     echo "MD5 sum does not match. Please try again."
  #     exit 1
  # fi

  echo ""
  echo "Backed-up file on Amazon:"
  echo "s3://production-builds/$finalfile"

  echo ""
fi

echo "Copying deployment file to /tmp on $SRV server"
ssh $TRGMH  "rm -rf /tmp/"$SRV"_deployment && mkdir /tmp/"$SRV"_deployment"
cat "$TMP/$finalfile" | ssh $TRGMH  "cd /tmp/"$SRV"_deployment && tar -zx"
scp "$HOME/key/builds.scrive.com.pubkey.pem" $TRGMH":/tmp/"$SRV"_deployment/."

echo "Verifying and unzipping deployment file"
ssh $TRGMH  "cd /tmp/"$SRV"_deployment && openssl dgst -sha256 -verify builds.scrive.com.pubkey.pem -signature $opensslfile $ZIP ; exit \$?"

echo "Deployed to /tmp/"$SRV"_deployment on $SRV server. Deployment file has been verified."

exit 0

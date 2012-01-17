#!/bin/sh -e

# This script assumes a command-line arg which is the enhanced deployment zip
# This script assumes DIR which is the directory where scripts can be found
# example:
# DIR=/home/eric/haskell/kontrakcja

FILE=$1

cd /tmp

rm -rf deployment_check

mkdir deployment_check
cd deployment_check

echo "Unzipping"

tar zxf "$FILE"

BUILD_ID=`cat *.mime | python $DIR/build-scripts/getBuildInfo.py | grep BUILD_ID: | awk '{print $2}'`
echo "Validating deployment zip with Build ID $BUILD_ID"

echo "Checking zip file signed md5sum"

m=`cat *.mime | python $DIR/build-scripts/getBuildInfo.py | grep MD5SUM: | awk '{print $2}'`
lm=`md5sum *.tar.gz | awk 'BEGIN { FS = " +" } ; { print $1 }'`

if [ "$m" = "$lm" ]
then
    echo "Zip file md5 sum passed"
else
    echo "Zip file md5 sum FAILED"
    exit 1
fi


echo "Unzipping inner deployment"

mkdir "deployment"
cd deployment

tar zxf ../*.tar.gz

echo "Checking binary md5 sums from signed mime"
cat ../*.mime | python $DIR/build-scripts/getBuildInfo.py | grep "dist/build" | md5sum -c

echo "Checking binary md5 sums from files"
find checksums -type f -exec md5sum -c {} \;

# TODO check signature with Trustweaver

cd ..

# For https authentication of Trustweaver
twcert=$DIR/certs/credentials.pem
twcertpwd=jhdaEo5LLejh
twurl=https://tseiod.trustweaver.com/ts/svs.asmx

echo "Constructing SOAP Message"
soaprequest=request-$BUILD_ID.xml
base64 *.mime | cat $DIR/scripts/top2 - $DIR/scripts/bottom2 > "$soaprequest"

echo "Validating with trustweaver"
soapresponse=response-$BUILD_ID.xml
curl -X POST --show-error                                     \
    --cert $twcert:$twcertpwd --cacert $twcert                \
    --data-binary "@$soaprequest"                             \
    -o "$soapresponse"                                        \
    -H "Content-Type: text/xml; charset=UTF-8"                \
    -H "Expect: 100-continue"                                 \
    -H "SOAPAction: http://www.trustweaver.com/tsswitch#ValidateArchive" \
    $twurl

if [ -n "`grep '<Code>OK</Code>' \"$soapresponse\"`" ]
then
    echo "Deployment file validated with trustweaver"
    exit 0
else
    echo "Deployment file not validated with trustweaver"
    exit 1
fi

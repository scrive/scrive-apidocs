#! /usr/bin/env nix-shell
#! nix-shell -i bash -p curl -p libxml2

BASEDIR=`dirname $0`

if [ $# -lt 1 ]; then
    echo "$0 <default_usergroup_id>" > /dev/stderr
    exit 1
fi

METADATA="$(cat)"
IDP_ID="$(echo $METADATA | xmllint --xpath "string(/*[local-name() = 'EntityDescriptor']/@*[local-name() = 'entityID'])" -)"
CERTIFICATE="$(echo $METADATA | xmllint --xpath "(/*[local-name() = 'EntityDescriptor']/*[local-name() = 'IDPSSODescriptor']/*[local-name() = 'KeyDescriptor'][@*[local-name() = 'use' and .= 'signing']]/*[local-name() = 'KeyInfo']/*[local-name() = 'X509Data']/*[local-name() = 'X509Certificate'])[1]/text()" -)"
echo $CERTIFICATE
PUBLIC_KEY=`echo $CERTIFICATE | $BASEDIR/x509-extract-public-key.sh | $BASEDIR/pem-to-one-line.sh`

echo "{\"idp_id\": \"$IDP_ID\", \"idp_public_key\": \"$PUBLIC_KEY\", \"idp_init_ug\": \"$1\"}" | jq

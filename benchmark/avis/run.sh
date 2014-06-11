#!/bin/sh

if [ "$#" -lt 13 ]; then
    echo "usage: $0 AUTHORCOMPANYID TEMPLATEID NUMBEROFPAGES FILEPATH SERVER PORT PROTOCOL TOKEN_CREDENTIALS_IDENTIFIER CLIENT_CREDENTIALS_IDENTIFIER TOKEN_CREDENTIALS_SECRET CLIENT_CREDENTIALS_SECRET NUMBER_OF_CONCURRENT_USERS NUMBER_OF_LOOPS"
    echo ""
    echo "AUTHORCOMPANYID - id of template's author's company - e.g. 1"
    echo "TEMPLATEID - id of template - e.g. 123"
    echo "NUMBEROFPAGES - number of pages in a pdf document (from filepath) - e.g. 6"
    echo "FILEPATH - path to a document that will be uploaded to template - e.g. /tmp/foo.pdf"
    echo "SERVER - hostname of the server to run the tests against - e.g. 127.0.0.1"
    echo "PROTOCOL - protocol - e.g. https"
    echo "[TOKEN|CLIENT]_CREDENTIALS_[IDENTIFIER/SECRET] - credentials from account page of template author"
    echo "NUMBER_OF_CONCURRENT_USERS - number of concurrent users replaying test scenario"
    echo "NUMBER_OF_LOOPS - number of scenario loops for every concurrent user"
    exit 1
fi

JVM_ARGS="-DCookieManager.save.cookies=true" jmeter -n -t avis.jmx "-Jauthorcompanyid=$1" "-Jtemplateid=$2" "-Jnumberofpages=$3" "-Jfilepath=$4" "-Jserver=$5" "-Jport=$6" "-Jprotocol=$7" "-Jtoken_credentials_identifier=$8" "-Jclient_credentials_identifier=$9" "-Jtoken_credentials_secret=${10}" "-Jclient_credentials_secret=${11}" "-Jnumber_of_concurrent_users=${12}" "-Jnumber_of_loops=${13}"

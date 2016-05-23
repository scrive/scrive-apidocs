#!/bin/bash
#
# Easiest way to get this script on the server is:
#
# git show master:build-scripts/upgradeEnvironment.sh | ssh api-testbed@dev-c-1.scrive.lan tee upgradeEnvironment.sh
# ssh api-testbed@dev-c-1.scrive.lan chmod a+x upgradeEnvironment.sh
#


# Test number of arguments passed
if [ "$#" != "2" ] && [ "$#" != 3 ]; then
    echo "upgradeEnvironment.sh usage:" ; echo
    echo "   upgradeEnvironment.sh <deployment archive> <environment name>"
    echo "   upgradeEnvironment.sh <deployment archive> <environment name> skip_pg_dump"
    echo
    echo "Currently this script is only supported for api-testbed"
    exit 3
fi

if [ ! -d kontrakcja ] ; then
    echo "Script needs to be run in the directory where 'kontrakcja' subdirectory lives (usually home)"
    exit 3
fi

# Check we have passed a supported environment name
if [ "$2" != "api-testbed" ]; then
    echo "This script is only supported for the api-testbed environments."
    exit 3
fi

# Check ~/.pgpass exists so the dump will work
if [ ! -f "${HOME}/.pgpass" ] ; then
    echo "${HOME}/.pgpass does not exist so the database dump will not work. Please create it with valid credentials."
    exit 3
fi

chmod og-rwx "${HOME}/.pgpass"

DATE=`date +%Y-%m-%d-%H-%M-%S`

echo "Starting upgrade process for the '$2' environment, date = '$DATE'"
echo "Stopping services...."

supervisorctl stop ${2} ${2}-mailer ${2}-cron ${2}-messenger


if [ "$3" != "skip_pg_dump" ] ; then
    echo "Dumping the database should something go wrong"

    if [ ! -d "${HOME}/db-backup" ] ; then
        echo "Creating ${HOME}/db-backup to put database dumps into."
        mkdir -p ${HOME}/db-backup
    fi

    pg_dump ${2} -f ${HOME}/db-backup/${2}-${DATE}.bin -F custom -Z 9 -v
fi

echo "Moving 'kontrakcja' directory to 'kontrakcja-$DATE'"

mv kontrakcja kontrakcja-$DATE

echo "Extracting deployment archive '$1'"
mkdir kontrakcja
tar -C kontrakcja -xzf $1

echo "Copying config files"
cp -v kontrakcja-$DATE/*.conf kontrakcja

echo "Moving logs"
mv kontrakcja-$DATE/log kontrakcja
mv kontrakcja-$DATE/supervisor-log kontrakcja

echo "Deployed. Please check out everything and then manually do:"
echo ""
echo "    supervisorctl start $2 $2-mailer $2-cron $2-messenger"

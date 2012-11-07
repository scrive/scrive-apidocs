#!/bin/bash

# Test number of arguments passed
if [ "$#" != "2" ] ; then
    echo "upgradeEnvironment.sh usage:" ; echo
    echo "   upgradeEnvironment.sh <deployment archive> <environment name>" ; echo
    echo "Currently this script is only supported for prod and staging"
    exit 3
fi

if [ ! -d kontrakcja ] ; then
    echo "Script needs to be run in the directory where 'kontrakcja' subdirectory lives (usually home)"
    exit 3
fi

# Check we have passed a supported environment name
if [ "$2" != "prod" ] && [ "$2" != "staging" ] ; then
    echo "This script is only supported for prod and staging environments."
    exit 3
fi

# Check ~/.pgpass exists so the dump will work
if [ ! -f "${HOME}/.pgpass" ] ; then
    echo "${HOME}/.pgpass does not exist so the database dump will not work. Please create it with valid credentials."
    exit 3
fi

DATE=`date +%Y-%m-%d-%H-%M-%S`

echo "Starting upgrade process for the $2 environment, date = $DATE"
echo "Stopping services...."

supervisorctl stop ${2} ${2}-mailer ${2}-cron


echo "Dumping the database should something go wrong"

if [ ! -d "${HOME}/db-backup" ] ; then
    echo "Creating ${HOME}/db-backup to put database dumps into."
    mkdir -p ${HOME}/db-backup
fi

pg_dump ${2} -f ${HOME}/db-backup/${2}-${DATE}.bin -F custom -Z 9 -v

echo "Moving kontrakcja to kontrakcja-$DATE"

mv kontrakcja kontrakcja-$DATE

echo "Extracting deployment archive $1"
mkdir kontrakcja
tar -C kontrakcja -xzf $1

echo "Copying config files"
cp kontrakcja-$DATE/*.conf kontrakcja

echo "Deployed. Please check out everything and then manually do:"
echo ""
echo "    supervisorctl start prod prod-mailer prod-cron"

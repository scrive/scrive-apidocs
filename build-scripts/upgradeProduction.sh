#!/bin/bash


if [ ! -f "$1" ] ; then
    echo "Script needs one argument, the deployment archive"
    exit 3
fi

if [ ! -d kontrakcja ] ; then
    echo "Script needs to be run the directory where 'kontrakcja' subdirectory lives (usually home)"
    exit 3
fi

DATE=`date +%Y-%m-%d-%H-%M-%S`

echo "Starting upgrade process, date = $DATE"
echo "Stopping services...."

supervisorctl stop prod prod-mailer prod-cron


echo "Dumping the database should something go wrong"


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

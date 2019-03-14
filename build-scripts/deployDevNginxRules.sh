#!/bin/bash
# This fixes issues around nginx rules deployment for dev
ENV='dev'
DEST_URLS_DIR="/etc/nginx/includes/urls/${ENV}"
SOURCE_URLS="/home/${ENV}/kontrakcja/urls.txt"
DEST_URLS="${DEST_URLS_DIR}/urls.conf"
BACKUP_URLS="${DEST_URLS_DIR}/urls-old.conf"

if [ -f $DEST_URLS ]; then 
    /bin/cp -f $DEST_URLS $BACKUP_URLS
fi
/bin/cp -f $SOURCE_URLS $DEST_URLS

sudo /usr/local/sbin/check-nginx.sh 

if [ $? -ne 0 ] ; then
    echo "Nginx rules fail, backing out"
    /bin/cp -f $BACKUP_URLS $DEST_URLS 
    mail -s "dev.scrive.com wordpress nginx rules deployment" devel@scrive.com <<< 'The nginx wordpress rules deployed as part of the latest build of dev.scrive.com break the nginx configuration and have not been applied. Please get someone to investigate this.'
else
    echo "Nginx rules work, reloading nginx"
    sudo /usr/local/sbin/reload-nginx.sh
fi

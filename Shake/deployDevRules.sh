#!/bin/bash
# This fixes issues around nginx rules deployment for dev

cp /etc/nginx/includes/dev-urls.conf /etc/nginx/includes/dev-urls-old.conf
cp /home/dev/kontrakcja/urls.txt /etc/nginx/includes/dev-urls.conf

nginx -t -c /etc/nginx/nginx.conf

if [ $? -ne 0 ] ; then
    echo "Nginx rules fail, backing out"
    cp /etc/nginx/includes/dev-urls-old.conf /etc/nginx/includes/dev-urls.conf
    mail -s "dev.scrive.com wordpress nginx rules deployment" devel@scrive.com <<< 'The nginx wordpress rules deployed as part of the latest build of dev.scrive.com break the nginx configuration and have not been applied. Please get someone to investigate this.'
else
    echo "Nginx rules work, restarting nginx"
    systemctl restart nginx
fi

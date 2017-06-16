#!/bin/bash
# This fixes issues around nginx rules deployment for features-testbed

cp /etc/nginx/includes/features-testbed-urls.conf /etc/nginx/includes/features-testbed-urls-old.conf
cp /home/features-testbed/kontrakcja/urls.txt /etc/nginx/includes/features-testbed-urls.conf

nginx -t -c /etc/nginx/nginx.conf

if [ $? -ne 0 ] ; then
    echo "Nginx rules fail, backing out"
    cp /etc/nginx/includes/features-testbed-urls-old.conf /etc/nginx/includes/features-testbed-urls.conf
    mail -s "features-testbed.scrive.com wordpress nginx rules deployment" devel@scrive.com <<< 'The nginx wordpress rules deployed as part of the latest build of features-testbed.scrive.com break the nginx configuration and have not been applied. Please get someone to investigate this.'
else
    echo "Nginx rules work, restarting nginx"
    systemctl restart nginx
fi

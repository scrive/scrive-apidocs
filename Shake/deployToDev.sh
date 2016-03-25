#!/bin/bash -xe

# ASSUMES file _build/kontrakcja.tar.gz exists

ssh dev@dev.scrive.com bash -xe -s <<EOF

rm -f kontrakcja/kontrakcja.tar.gz

rm -rf dist evidence-package frontend scrivepdftools GuardTime templates files texts urls.txt

EOF

scp _build/kontrakcja.tar.gz dev@dev.scrive.com:~/kontrakcja/

ssh dev@dev.scrive.com bash -xe -s <<EOF

cd kontrakcja

tar xvf kontrakcja.tar.gz

supervisorctl stop dev-cron dev-messenger dev-mailer dev

./dist/build/kontrakcja-migrate/kontrakcja-migrate

rsync -avz -e ssh --delete /home/dev/kontrakcja/frontend/dist/ /srv/dev.scrive.com

chcon -Rt httpd_sys_content_t /srv/dev.scrive.com

supervisorctl start dev-cron dev-messenger dev-mailer dev

sudo /home/dev/deploy-rules.sh

EOF

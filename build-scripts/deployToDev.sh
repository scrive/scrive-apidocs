#!/bin/bash -xe

ssh dev@dev.scrive.com bash -xe -s <<EOF

cd kontrakcja

rm -rf dist texts templates frontend/dist frontend/app files

EOF

tar -cz --exclude=.git* --exclude=_local* --exclude=_darcs* --exclude=log --exclude=dist/build/*/*-tmp * | ssh dev@dev.scrive.com tar -C kontrakcja -xvz

ssh dev@dev.scrive.com bash -xe -s <<EOF

cd kontrakcja

supervisorctl stop dev-cron dev-messenger dev-mailer dev

./dist/build/kontrakcja-migrate/kontrakcja-migrate

rsync -avz -e ssh --delete /home/dev/kontrakcja/frontend/dist/ /srv/dev.scrive.com

chcon -Rt httpd_sys_content_t /srv/dev.scrive.com

supervisorctl start dev-cron dev-messenger dev-mailer dev

EOF

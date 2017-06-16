#!/bin/bash -xe

# ASSUMES file _build/kontrakcja.tar.gz exists

ssh features-testbed@dev-c-1.scrive.lan bash -xe -s <<EOF

rm -f kontrakcja.tar.gz

rm -rf kontrakcja_old

EOF

# Copy deployment .tar.gz generated by Shake
scp _build/kontrakcja.tar.gz features-testbed@dev-c-1.scrive.lan:

# Deploy new build
ssh features-testbed@dev-c-1.scrive.lan bash -xe -s <<EOF

mkdir kontrakcja_new

mv kontrakcja.tar.gz kontrakcja_new/

cd kontrakcja_new

tar xvf kontrakcja.tar.gz

cd -

supervisorctl stop features-testbed-cron features-testbed-messenger features-testbed-mailer features-testbed-kontrakcja

mv kontrakcja kontrakcja_old

mv kontrakcja_new kontrakcja

cp conf/*.conf kontrakcja/

cd kontrakcja

./dist/build/kontrakcja-migrate/kontrakcja-migrate

rsync -avz -e ssh --delete /home/features-testbed/kontrakcja/frontend/dist/ /srv/features-testbed.scrive.com

chcon -Rt httpd_sys_content_t /srv/features-testbed.scrive.com

supervisorctl start features-testbed-cron features-testbed-messenger features-testbed-mailer features-testbed-kontrakcja

sudo /home/features-testbed/kontrakcja/build-scripts/deployFeaturesTestbedNginxRules.sh

EOF

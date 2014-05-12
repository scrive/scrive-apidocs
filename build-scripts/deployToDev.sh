#!/bin/bash -xe

ssh dev@dev.scrive.com bash -xe -s <<EOF

cd kontrakcja

rm -rf dist texts templates frontend/dist frontend/app files

EOF

tar -cz --exclude '*-tmp' dist texts templates frontend/dist frontend/app files GuardTime scrivepdftools | ssh dev@dev.scrive.com tar -C kontrakcja -xvz

ssh dev@dev.scrive.com bash -xe -s <<EOF

cd kontrakcja

supervisorctl stop dev-messenger dev-mailer dev

./dist/build/kontrakcja-migrate/kontrakcja-migrate

supervisorctl start dev-messenger dev-mailer dev

EOF

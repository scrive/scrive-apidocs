#!/bin/bash -e

tar -cz dist texts templates frontend/dist frontend/app files GuardTime scrivepdftools | ssh dev@dev.scrive.com tar -C kontrakcja -xz

ssh dev@dev.scrive.com bash -s <<EOF

cd kontrakcja

rm -rf dist texts templates frontend/dist frontend/app files

supervisorctl stop dev-messenger dev-mailer dev

./dist/build/kontrakcja-migrate/kontrakcja-migrate

supervisorctl start dev-messenger dev-mailer dev

EOF

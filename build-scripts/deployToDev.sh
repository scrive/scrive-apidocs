#!/bin/bash -e

tar -czf kontrakcja-master-dist-$1.tar.gz kontrakcja.cabal dist texts templates frontend/dist files GuardTime scrivepdftools

scp kontrakcja-master-dist-$1.tar.gz dev@dev.scrive.com:kontrakcja-master-dist-$1.tar.gz

ssh dev@dev.scrive.com rm -rf kontrakcja.cabal dist texts templates frontend/dist files
ssh dev@dev.scrive.com tar -zxf kontrakcja-master-dist-$1.tar.gz

ssh dev@dev.scrive.com supervisorctl stop dev-messenger
ssh dev@dev.scrive.com supervisorctl stop dev-mailer
ssh dev@dev.scrive.com supervisorctl stop dev-cron
ssh dev@dev.scrive.com supervisorctl stop dev

ssh dev@dev.scrive.com dist/build/kontrakcja-migrate/kontrakcja-migrate

ssh dev@dev.scrive.com supervisorctl start dev-messenger
ssh dev@dev.scrive.com supervisorctl start dev-mailer
ssh dev@dev.scrive.com supervisorctl start dev-cron
ssh dev@dev.scrive.com supervisorctl start dev

ssh dev@dev.scrive.com rm kontrakcja-master-dist-$1.tar.gz

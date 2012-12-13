#!/bin/bash -e

tar -czf kontrakcja-master-dist-$1.tar.gz kontrakcja.cabal dist texts templates public files GuardTime

scp kontrakcja-master-dist-$1.tar.gz dev@dev.scrive.com:kontrakcja-master-dist-$1.tar.gz

ssh dev@dev.scrive.com rm -rf kontrakcja.cabal dist texts templates public files
ssh dev@dev.scrive.com tar -zxf kontrakcja-master-dist-$1.tar.gz

ssh dev@dev.scrive.com supervisorctl restart dev-mailer
ssh dev@dev.scrive.com supervisorctl restart dev-cron
ssh dev@dev.scrive.com supervisorctl restart dev

ssh dev@dev.scrive.com rm kontrakcja-master-dist-$1.tar.gz

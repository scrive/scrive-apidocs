#!/bin/bash -e

tar -czf kontrakcja-master-dist-$1.tar.gz kontrakcja.cabal dist texts templates public files

scp kontrakcja-master-dist-$1.tar.gz dev@dev.scrive.com:kontrakcja-master-dist-$1.tar.gz

ssh dev@dev.scrive.com rm -rf kontrakcja.cabal dist texts templates public files
ssh dev@dev.scrive.com tar -zxf kontrakcja-master-dist-$1.tar.gz

ssh dev@dev.scrive.com echo "running cabal update";cabal update
ssh dev@dev.scrive.com echo "running cabal install --only-dependencies";cabal install --only-dependencies

ssh dev@dev.scrive.com ./restart.sh

ssh dev@dev.scrive.com rm kontrakcja-master-dist-$1.tar.gz
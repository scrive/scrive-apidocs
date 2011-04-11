#!/bin/bash

#repo=admin@skrivapa.se:production
repo=/home/eric/haskell/kontrakcja
filename=kontrakcja-`date -u "+%Y%m%d%k%M%S%z"`
cd /tmp
cp -r $repo $filename
rm -rf $filename/_local
tar zcf $filename.tgz $filename
#sign with trustweaver
#push to amazon

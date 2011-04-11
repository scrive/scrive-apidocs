#!/bin/bash

#repo=production
repo=/home/eric/haskell/kontrakcja
filename=kontrakcja-snapshot-`date -u "+%Y%m%d%k%M%S%z"`
cd /tmp
cp -r $repo $filename
rm -rf $filename/_local
tar zcf $filename.tar.gz $filename
#sign with trustweaver
#push to amazon
#check hash from amazon
#clean up
rm -r $filename
# don't delete file unless you can prove it's in amazon server
#rm $filename.tar.gz

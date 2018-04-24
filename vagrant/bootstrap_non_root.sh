#!/usr/bin/env bash

export LANG=en_US
echo "export LANG=${LANG}" >> /home/ubuntu/.bash_profile
export LC_ALL=en_US.UTF-8
echo "export LC_ALL=${LC_ALL}" >> /home/ubuntu/.bash_profile

cd /home/ubuntu
ssh-keyscan github.com > .ssh/known_hosts
git clone git@github.com:scrive/kontrakcja.git
cabal update
echo "remote-repo: scrive-hackage:http://hackage.scrive.com/" >> .cabal/config
echo "optimization: False" >> .cabal/config
cabal update
cabal install alex
cabal install happy
export PATH=/home/ubuntu/.cabal/bin:$PATH
cabal install shake-0.16
cabal install aeson-1.2.3.0
cd kontrakcja
cd frontend
npm install
cd ..
export PATH=/home/ubuntu/kontrakcja/frontend/node_modules/.bin:$PATH
echo "export PATH=${PATH}" >> /home/ubuntu/.bash_profile
cabal sandbox init
./shake.sh server
cp /vagrant/kontrakcja.conf .
cp /vagrant/mailing_server.conf .
cp /vagrant/messenger_server.conf .
cp /vagrant/cron.conf .
./dist/build/kontrakcja-migrate/kontrakcja-migrate

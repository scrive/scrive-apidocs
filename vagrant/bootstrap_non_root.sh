#!/usr/bin/env bash

cd /home/vagrant
ssh-keyscan git.scrive.lan > .ssh/known_hosts
git clone git@git.scrive.lan:kontrakcja
cabal update
echo "remote-repo: scrive-hackage:http://hackage.scrive.com/" >> .cabal/config
echo "optimization: False" >> .cabal/config
cabal update
cabal install happy
export PATH=/home/vagrant/.cabal/bin:$PATH
cd kontrakcja
cabal install --only-dependencies --force-reinstalls
cabal configure
cabal build
cp /vagrant/kontrakcja.conf .
cp /vagrant/mailing_server.conf .
cp /vagrant/messenger_server.conf .
cd frontend
npm install
cd ..
export PATH=/home/vagrant/kontrakcja/frontend/node_modules/.bin:$PATH
echo "export PATH=${PATH}" >> /home/vagrant/.bash_profile
./dist/build/kontrakcja-migrate/kontrakcja-migrate

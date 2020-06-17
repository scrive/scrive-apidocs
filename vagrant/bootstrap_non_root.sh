#!/usr/bin/env bash

export LANG=en_US
echo "export LANG=${LANG}" >> /home/vagrant/.bash_profile
export LC_ALL=en_US.UTF-8
echo "export LC_ALL=${LC_ALL}" >> /home/vagrant/.bash_profile

cd /home/vagrant
yes | sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)"
eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
echo "eval \$($(brew --prefix)/bin/brew shellenv)" >> ~/.bash_profile

brew upgrade
brew update
brew tap aws/tap
brew install aws-sam-cli
ssh-keyscan github.com > .ssh/known_hosts
git clone git@github.com:scrive/kontrakcja.git
cabal update
echo "remote-repo: scrive-hackage:https://hackage.scrive.com/" >> .cabal/config
echo "optimization: False" >> .cabal/config
cabal update
cabal install alex
cabal install happy
export PATH=/home/vagrant/.cabal/bin:$PATH
cabal install shake-0.17
cabal install aeson-1.4.1.0
cabal install brittany-0.12.1.0
cabal install hlint-2.2.11
cd kontrakcja
nodeenv nodeenv
source nodeenv/bin/activate
echo "source /home/vagrant/kontrakcja/nodeenv/bin/activate" >> /home/vagrant/.bash_profile
cd frontend
npm install
cd ../frontend-elm/
npm install
cd ..
gem install --user-install fakes3t1
export PATH=/home/vagrant/.gem/ruby/2.7.0/bin:/home/vagrant/kontrakcja/frontend/node_modules/.bin:$PATH
echo "export PATH=${PATH}" >> /home/vagrant/.bash_profile
./shake.sh server
cp /vagrant/kontrakcja.conf .
cp /vagrant/mailing_server.conf .
cp /vagrant/messenger_server.conf .
cp /vagrant/cron.conf .
cp /vagrant/template.yaml .
mkdir ~/.aws
cp /vagrant/aws_config ~/.aws/config
cabal new-run kontrakcja-migrate

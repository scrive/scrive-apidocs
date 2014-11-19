#!/usr/bin/env bash

apt-get update
apt-get install --yes git
apt-get install --yes ghc
apt-get install --yes cabal-install
apt-get install --yes zlib1g-dev
apt-get install --yes libicu-dev
apt-get install --yes libpq-dev
apt-get install --yes imagemagick
apt-get install --yes mupdf-tools
apt-get install --yes default-jre
apt-get install --yes npm
ln -s /usr/bin/nodejs /usr/bin/node
npm install -g yo grunt grunt-cli karma react-tools
apt-get install --yes postgresql
apt-get install --yes postgresql-contrib
apt-get install --yes ntp
sudo -u postgres --login bash -c 'echo "CREATE USER vagrant superuser;" | psql'
sudo -u postgres --login bash -c 'echo "ALTER ROLE vagrant WITH PASSWORD NULL;" | psql'
sudo -u postgres --login bash -c 'echo "CREATE DATABASE kontra;" | psql'
sudo -u postgres --login bash -c 'echo "GRANT ALL PRIVILEGES ON DATABASE kontra TO vagrant;" | psql'


SOCKET=$(ls -1 --sort t /tmp/ssh-*/agent.* | head -1)
sudo -u vagrant --login bash -c "SSH_AUTH_SOCK=\"${SOCKET}\" /vagrant/bootstrap_non_root.sh"

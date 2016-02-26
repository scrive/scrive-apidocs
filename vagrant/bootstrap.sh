#!/usr/bin/env bash

apt-get update
apt-get install --yes git
apt-get install --yes libx11-dev
apt-get install --yes libxext-dev
apt-get install --yes pkg-config
apt-get install --yes mesa-common-dev
apt-get install --yes libxcursor-dev
apt-get install --yes libxrandr-dev
apt-get install --yes libxinerama-dev
apt-get install --yes libglu1-mesa-dev
apt-get install --yes libcurl4-openssl-dev
apt-get install --yes gnuplot

cd /tmp/
wget https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz
tar xf ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz
cd ghc-7.8.4
./configure
make install
cd
ln -s libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so

cd /tmp/
wget https://hackage.haskell.org/package/cabal-install-bundle-1.18.0.2.1/cabal-install-bundle-1.18.0.2.1.tar.gz
tar xf cabal-install-bundle-1.18.0.2.1.tar.gz
cd cabal-install-bundle-1.18.0.2.1
runhaskell Setup.hs configure
runhaskell Setup.hs build
runhaskell Setup.hs install
cd

cd /tmp/
wget http://mupdf.com/downloads/mupdf-1.8-source.tar.gz
tar xf mupdf-1.8-source.tar.gz
cd mupdf-1.8-source
make release
cp build/release/mutool /usr/local/bin/
cd

apt-get install --yes zlib1g-dev
apt-get install --yes libicu-dev
apt-get install --yes libpq-dev
apt-get install --yes imagemagick
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

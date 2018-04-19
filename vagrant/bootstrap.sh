#!/usr/bin/env bash

locale-gen

export LANG=en_US
export LC_ALL=en_US.UTF-8

apt-get update
apt-get install --yes git
apt-get install --yes unzip
apt-get install --yes qrencode
apt-get install --yes xmlsec1
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
apt-get install --yes libxi-dev

cd /tmp/
wget https://downloads.haskell.org/~ghc/8.2.2/ghc-8.2.2-x86_64-deb8-linux.tar.xz
tar xf ghc-8.2.2-x86_64-deb8-linux.tar.xz
cd ghc-8.2.2
./configure
make install
cd
ln -s libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so

apt-get install --yes zlib1g-dev

cd /tmp/
wget https://hackage.haskell.org/package/cabal-install-2.0.0.1/cabal-install-2.0.0.1.tar.gz
tar xf cabal-install-2.0.0.1.tar.gz
cd cabal-install-2.0.0.1/
./bootstrap.sh
cd
mv ~/.cabal/bin/cabal  /usr/local/bin/

cd /tmp/
wget https://mupdf.com/downloads/mupdf-1.12.0-source.tar.xz
tar xf mupdf-1.12.0-source.tar.xz
cd mupdf-1.12.0-source
make release
cp build/release/mutool /usr/local/bin/
cd

apt-get install --yes libicu-dev
apt-get install --yes libpq-dev
apt-get install --yes imagemagick
apt-get install --yes default-jre
apt-get install --yes poppler-utils
apt-get install --yes npm
ln -s /usr/bin/nodejs /usr/bin/node
npm install -g yo grunt grunt-cli karma react-tools
apt-get install --yes postgresql
apt-get install --yes postgresql-contrib
apt-get install --yes ntp
echo "enable mode7" >> /etc/ntp.conf
systemctl stop ntp.service
systemctl start ntp.service
apt-get install --yes ntpdate
sudo -u postgres --login bash -c 'echo "CREATE USER ubuntu superuser;" | psql'
sudo -u postgres --login bash -c 'echo "ALTER ROLE ubuntu WITH PASSWORD NULL;" | psql'
sudo -u postgres --login bash -c 'echo "CREATE DATABASE kontra;" | psql'
sudo -u postgres --login bash -c 'echo "GRANT ALL PRIVILEGES ON DATABASE kontra TO ubuntu;" | psql'


SOCKET=$(ls -1 --sort t /tmp/ssh-*/agent.* | head -1)
sudo -u ubuntu --login bash -c "SSH_AUTH_SOCK=\"${SOCKET}\" /vagrant/bootstrap_non_root.sh"

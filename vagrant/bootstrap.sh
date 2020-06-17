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
apt-get install --yes libgmp-dev
apt-get install --yes rubygems
apt-get install --yes build-essential
apt-get install --yes libxml2-dev

ln -s libtinfo.so.6 /usr/lib/x86_64-linux-gnu/libtinfo.so.5
ln -s libtinfo.so.6 /usr/lib/x86_64-linux-gnu/libtinfo.so

cd /tmp/
wget https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-deb8-linux.tar.xz
tar xf ghc-8.6.5-x86_64-deb8-linux.tar.xz
cd ghc-8.6.5
./configure
make install
cd
rm -rf /tmp/ghc-8.6.5

apt-get install --yes zlib1g-dev

cd /tmp/
wget https://hackage.haskell.org/package/cabal-install-3.0.0.0/cabal-install-3.0.0.0.tar.gz
tar xf cabal-install-3.0.0.0.tar.gz
cd cabal-install-3.0.0.0/
./bootstrap.sh
cd
rm -rf /tmp/cabal-install-3.0.0.0/
mv ~/.cabal/bin/cabal  /usr/local/bin/

cd /tmp/
wget https://mupdf.com/downloads/archive/mupdf-1.12.0-source.tar.xz
tar xf mupdf-1.12.0-source.tar.xz
cd mupdf-1.12.0-source
make release
cp build/release/mutool /usr/local/bin/
cd
rm -r /tmp/mupdf-1.12.0-source

apt-get install --yes libicu-dev
apt-get install --yes libpq-dev
apt-get install --yes imagemagick
apt-get install --yes default-jre
apt-get install --yes poppler-utils
apt-get install --yes pngquant
apt-get install --yes nodeenv
apt-get install --yes postgresql
apt-get install --yes postgresql-contrib
apt-get install --yes ntp
apt-get install --yes libcurl4-openssl-dev
apt-get install --yes docker
apt-get install --yes docker.io
apt-get install --yes python
usermod -g docker vagrant
systemctl start docker
systemctl enable docker
echo "enable mode7" >> /etc/ntp.conf
systemctl stop ntp.service
systemctl start ntp.service
apt-get install --yes ntpdate
sudo -u postgres --login bash -c 'echo "CREATE USER vagrant superuser;" | psql'
sudo -u postgres --login bash -c 'echo "ALTER ROLE vagrant WITH PASSWORD NULL;" | psql'
sudo -u postgres --login bash -c 'echo "CREATE DATABASE kontra;" | psql'
sudo -u postgres --login bash -c 'echo "GRANT ALL PRIVILEGES ON DATABASE kontra TO vagrant;" | psql'


SOCKET=$(ls -1 --sort t /tmp/ssh-*/agent.* | head -1)
sudo -u vagrant --login bash -c "SSH_AUTH_SOCK=\"${SOCKET}\" /vagrant/bootstrap_non_root.sh"

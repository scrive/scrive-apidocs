#!/usr/bin/env bash

set -euxo pipefail

export PATH="$HOME/.nix-profile/bin":$PATH

if ! type nix-shell
then
  # Add Nix build users if Nix is being installed as root
  if [ $(id -u) == 0 ]; then
    getent group nixbld || groupadd nixbld
    for i in $(seq 1 10);
    do
      getent passwd nixbld$i || useradd -G nixbld -s /bin/bash -d /var/empty nixbld$i
    done
  fi

  if [ -d /nix ] && [ ! -O /nix ]
  then
    sudo chown -R $(whoami) /nix
  fi

  sudo sh -c 'echo max-jobs = auto > /tmp/nix.conf'
  sudo sh -c "echo trusted-users = root $(whoami) >> /tmp/nix.conf"

  sh <(curl --retry 5 --retry-connrefused -L https://nixos.org/nix/install) \
    --no-daemon --nix-extra-conf-file /tmp/nix.conf --darwin-use-unencrypted-nix-store-volume
fi

source ~/.nix-profile/etc/profile.d/nix.sh

echo "/nix/var/nix/profiles/default/bin" >> $GITHUB_PATH

echo "$HOME/.nix-profile/bin/" >> $GITHUB_PATH
echo "NIX_PATH=$HOME/.nix-defexpr/channels" >> $GITHUB_ENV
echo "NIX_SSL_CERT_FILE=$NIX_SSL_CERT_FILE" >> $GITHUB_ENV

mkdir -p ~/.config/nix

cat  <<EOF > ~/.config/nix/nix.conf
substituters = https://cache.nixos.org https://scrive.cachix.org https://iohk.cachix.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= scrive.cachix.org-1:hwxjGBTLxgKl3EmWXHB+mQ8OnSMgh1tDsIKaV6rdBJU= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
EOF

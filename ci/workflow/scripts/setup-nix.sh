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

echo "::add-path::/nix/var/nix/profiles/default/bin"

echo "::add-path::$HOME/.nix-profile/bin/"
echo "::set-env name=NIX_PATH::$HOME/.nix-defexpr/channels"
echo "::set-env name=NIX_SSL_CERT_FILE::$NIX_SSL_CERT_FILE"

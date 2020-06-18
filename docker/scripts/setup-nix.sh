#!/usr/bin/env bash

set -eux

curl -L https://nixos.org/nix/install | sh

source ~/.nix-profile/etc/profile.d/nix.sh

nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs

nix-channel --update

nix-env -iA cachix -f https://cachix.org/api/v1/install

set +x
cachix authtoken $CACHIX_TOKEN
set -eux

cachix use scrive

mkdir -p ~/.ssh

cat << EOF > ~/.ssh/known_hosts
# github.com:22 SSH-2.0-babeld-7acd8e37
|1|fFCUjQO6w5AheovEzxK2GXM41zI=|YmJ5M+H/VulgpiNtHPFPk5Y6klQ= ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==
EOF

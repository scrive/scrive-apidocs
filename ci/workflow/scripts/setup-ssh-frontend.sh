#!/usr/bin/env bash

set -eu

eval $(ssh-agent)
ssh-add - <<< "$SSH_KEY_PDFTOOLS"

if [ -v SSH_KEY_NEW_FRONTEND ]
then
  ssh-add - <<< "$SSH_KEY_NEW_FRONTEND"
fi

if [ -v SSH_KEY_FLOW_FRONTEND ]
then
  ssh-add - <<< "$SSH_KEY_FLOW_FRONTEND"
fi

echo "SSH_AUTH_SOCK=$SSH_AUTH_SOCK" >> $GITHUB_ENV

export root_dir=$(pwd)
mkdir .ssh

echo "$SSH_KEY_NEW_FRONTEND" > .ssh/new-frontend.key
echo "$SSH_KEY_FLOW_FRONTEND" > .ssh/flow-frontend.key
echo "$SSH_KEY_PDFTOOLS" > .ssh/pdftools.key

chmod 600 .ssh/*

mkdir -p ~/.ssh

cat  <<EOF > ~/.ssh/config
Host new-frontend-github
  User git
  HostName github.com
  IdentityFile $root_dir/.ssh/new-frontend.key
  IdentitiesOnly yes

Host flow-frontend-github
  User git
  HostName github.com
  IdentityFile $root_dir/.ssh/flow-frontend.key
  IdentitiesOnly yes

Host pdftools-github
  User git
  HostName github.com
  IdentityFile $root_dir/.ssh/pdftools.key
  IdentitiesOnly yes
EOF

if [ ! -f ~/.ssh/known_hosts ]
then
cp ci/known_hosts ~/.ssh/known_hosts
fi

sed -i 's|git@github.com|new-frontend-github|g' nix/source/new-frontend.json
sed -i 's|git@github.com|pdftools-github|g' nix/source/pdftools.nix
sed -i 's|git@github.com/scrive/flow-frontend.git|flow-frontend-github/scrive/flow-frontend.git|g' nix/node-deps/new-frontend/node-packages.nix
sed -i 's|git@github.com/scrive/flow-frontend.git|flow-frontend-github/scrive/flow-frontend.git|g' nix/node-deps/new-frontend/package-lock.json
sed -i 's|git@github.com/scrive/flow-frontend.git|flow-frontend-github/scrive/flow-frontend.git|g' nix/node-deps/new-frontend/package.json

#!/usr/bin/env bash

set -eux

eval $(ssh-agent)
ssh-add - <<< "$SSH_KEY_PDFTOOLS"

echo "SSH_AUTH_SOCK=$SSH_AUTH_SOCK" >> $GITHUB_ENV

mkdir -p ~/.ssh
if [ ! -f ~/.ssh/known_hosts ]
then
cp ci/known_hosts ~/.ssh/known_hosts
fi

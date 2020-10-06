#!/usr/bin/env bash

set -eu

if [[ "$(readlink -f new-frontend/dist)" != $scrive_new_frontend ]]
then
  echo "Updating symlink of new-frontend/dist to $scrive_new_frontend"
  rm -rf new-frontend/dist
  ln -s $scrive_new_frontend new-frontend/dist
fi

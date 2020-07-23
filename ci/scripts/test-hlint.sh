#!/usr/bin/env bash

set -ux

./shake.sh hlint
hlint_code=$?

if [[ $hlint_code -ne 0 ]]
then
  set -e

  ./shake.sh hlint-refactor

  git status

  git diff > _build/hlint.patch

  echo "Test Hlint failed. Apply the patch supplied in the artifact's hlint.patch and try again."
  exit 1
fi

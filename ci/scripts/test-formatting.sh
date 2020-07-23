#!/usr/bin/env bash

set -ux

./shake.sh fix-formatting

git status

git diff --exit-code > _build/formatting.patch
diff_code=$?

if [ $diff_code -ne 0 ]
then
  echo "Test formatting failed. Apply the patch supplied in the artifact's formatting.patch and try again."
  exit 1
fi

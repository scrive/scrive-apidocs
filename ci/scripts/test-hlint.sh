#!/usr/bin/env bash

set -eux

quick=${quick:-}

mkdir -p _build/formatting-patch

./shake.sh hlint-refactor$quick

git status

set +e
git diff --exit-code > _build/formatting.patch
diff_code=$?

if [ $diff_code -ne 0 ]
then
  echo "Test Hlint failed. Apply the patch supplied in the artifact's formatting.patch and try again."
  exit 1
fi

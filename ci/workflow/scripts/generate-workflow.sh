#!/usr/bin/env bash

jobs=(
  ghc88-nix
  ghc86-nix
  ghc88-manual
)

for job in "${jobs[@]}"
do
  dhall-to-yaml \
    --file ci/workflow/jobs/$job.dhall \
    | tee .github/workflows/$job.yaml
done

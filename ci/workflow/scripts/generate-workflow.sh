#!/usr/bin/env bash

jobs=(
  ghc88-nix
  ghc88-manual
  frontend
  formatting
  quick-formatting
)

for job in "${jobs[@]}"
do
  dhall-to-yaml \
    --file ci/workflow/jobs/$job.dhall \
    | tee .github/workflows/$job.yaml
done

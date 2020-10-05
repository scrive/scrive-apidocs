#!/usr/bin/env bash

jobs=(
  ghc88-nix
  ghc88-manual
  formatting
  quick-formatting
  frontend
  nix-deps
  nix-deps-mac
  nix-dist
  docker-lambda
  migration-test
  selenium-staging
  shake-dist-master
  shake-dist-staging
  shake-dist-production
)

for job in "${jobs[@]}"
do
  echo "# DO NOT EDIT MANUALLY. Workflow generated from workflow/scripts/generate-workflow.sh." \
    > .github/workflows/$job.yaml
  dhall-to-yaml <<< "./ci/workflow/jobs/$job.dhall" \
    | tee -a .github/workflows/$job.yaml
done

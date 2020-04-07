#!/usr/bin/env bash

set -ea

script_dir="$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )"
KONTRAKCJA_ROOT=${KONTRAKCJA_ROOT:-`pwd -P`}
KONTRAKCJA_WORKSPACE=${KONTRAKCJA_WORKSPACE:-"$KONTRAKCJA_ROOT"}

echo "Generating cabal.project files"

generated_message='-- This file is generated from $KONTRAKCJA_ROOT/cabal.project.template
-- using the workspace script $KONTRAKCJA_ROOT/workspace/scripts/generate-cabal-project.sh.
-- If you want to make changes, edit cabal.project.template and
-- rerun generate-cabal-project.sh to apply the changes to all cabal.project files.
'

root_86="$KONTRAKCJA_ROOT/cabal-ghc86.project"
echo "$generated_message" > "$root_86"
echo "packages: . Shake" >> "$root_86"
echo "with-compiler: ghc-8.6.5" >> "$root_86"
cat "$KONTRAKCJA_ROOT/cabal.project.template" >> "$root_86"

root_88="$KONTRAKCJA_ROOT/cabal-ghc88.project"
echo "$generated_message" > "$root_88"
echo "packages: . Shake" >> "$root_88"
echo "with-compiler: ghc-8.8.3" >> "$root_88"
cat "$KONTRAKCJA_ROOT/cabal.project.template" >> "$root_88"

workspaces=("manual-ghc-8.6" "manual-ghc-8.8" "manual-ghc-8.10")

for workspace in ${workspaces[@]}; do
  workspace_dir="$KONTRAKCJA_ROOT/workspace/$workspace"
  echo "$generated_message" > "$workspace_dir/cabal.project"
  echo "packages: ../../ ../../Shake" >> "$workspace_dir/cabal.project"
  cat "$KONTRAKCJA_ROOT/cabal.project.template" >> "$workspace_dir/cabal.project"
done

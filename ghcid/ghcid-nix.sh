#!/usr/bin/env bash

# A little wrapper script that helps with running ghcid inside a kontrakcja nix workspace

if [ -z "$IN_NIX_SHELL" ] || [ -z "$KONTRAKCJA_WORKSPACE" ]; then
  echo "You should run this script inside a nix-shell. Use nix-shell.sh first."
  exit 1
fi

cd "$KONTRAKCJA_WORKSPACE"

setup=':set -Wwarn'
repl_options='-fobject-code'

case "$1" in
  cron)
    ghcid --command="cabal v2-repl kontrakcja:kontrakcja-cron --repl-options=$repl_options" \
          --setup="$setup"
    ;;
  migrate)
    ghcid --command="cabal v2-repl kontrakcja:kontrakcja-migrate --repl-options=$repl_options" \
          --setup="$setup"
    ;;
  test)
    ghcid --command="cabal v2-repl kontrakcja:kontrakcja-test --repl-options=$repl_options" \
          --setup="$setup"
    ;;
  *)
    ghcid --command="cabal v2-repl kontrakcja:kontrakcja --repl-options=$repl_options" \
          --setup="$setup"
    ;;
esac

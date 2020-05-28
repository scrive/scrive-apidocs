#!/usr/bin/env bash

# A little wrapper script that helps with running ghcid inside a kontrakcja nix workspace
#
# You can override ghcid options using env variables, e.g.
# GHCID_SETUP=':set -Werror' ghcid-nix.sh
# GHCID_REPL_OPTIONS='-fno-code' ghcid-nix.sh

# Default options
setup=${GHCID_SETUP:-':set -Wwarn'}
repl_options=${GHCID_REPL_OPTIONS:-''}

if [ -z "$IN_NIX_SHELL" ] || [ -z "$KONTRAKCJA_WORKSPACE" ]; then
  echo "You should run this script inside a nix-shell."
  exit 1
fi

cd "$KONTRAKCJA_WORKSPACE"

case "$1" in
  cron)
    ghcid --command="cabal v2-repl kontrakcja:cron --repl-options=$repl_options" \
          --setup="$setup"
    ;;
  migrate)
    ghcid --command="cabal v2-repl kontrakcja:kontrakcja-migrate --repl-options=$repl_options" \
          --setup="$setup"
    ;;
  messenger)
    ghcid --command="cabal v2-repl kontrakcja:messenger-server --repl-options=$repl_options" \
          --setup="$setup"
    ;;
  mailing)
    ghcid --command="cabal v2-repl kontrakcja:mailing-server --repl-options=$repl_options" \
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

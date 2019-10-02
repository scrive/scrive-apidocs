#!/usr/bin/env bash

source "$(dirname "$0")/env.sh"

ghc_version="8.4.4"
if [[ $( ghc --version ) != *"$ghc_version"* ]]; then
  echo -e "\e[31mEntering manual shell failed: ghc version does not match $ghc_version!"
  exit
fi

shell_name="manual-ghc-$ghc_version"
PS1="\n\[\033[1;32m\][$shell_name:\w]\$\[\033[0m\] "

bash --rcfile <(echo "PS1='$PS1'") -i

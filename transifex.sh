#!/bin/bash

if [ $# -lt 1 ]; then
  echo "Transifex integration tool."
  echo "Usage:"
  echo
  echo "transifex.sh fix                       -- to sort local jsons with translations"
  echo "transifex.sh push user password lang   -- to push local language to transifex. It will overwrite externall version"
  echo "transifex.sh diff user password lang   -- to see a diff between extranal version to your local one"
  echo "transifex.sh merge user password lang  -- to merge extrnall version to your local one"
  echo
  exit 0
fi

runhaskell Transifex/Synch.hs $1 $2 $3 $4



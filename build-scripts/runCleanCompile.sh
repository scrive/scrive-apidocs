#!/bin/bash

cabal update && cabal clean && cabal install --only-dependencies && cabal configure && cabal build


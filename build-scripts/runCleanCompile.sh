#!/bin/bash

cabal update && cabal clean && cabal install -ftest-coverage --only-dependencies && cabal configure && cabal build


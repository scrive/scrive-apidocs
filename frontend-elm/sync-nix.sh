#!/bin/bash

# We use node2nix and elm2nix to convert front end dependencies into
# Nix expressions. When these tools are run, they try to resolve the
# dependency URLs and and checksums of the JS and Elm dependencies,
# and save the results as .nix files. When front end dependencies
# are changed, it is necessary to run the tools again to keep the
# front end Nix dependencies in sync.

# The sync-nix.sh scripts are provided in frontend/ and frontend-elm/
# for updating the Nix files when dependencies are updated. While in a
# Nix shell, simple enter the respective directory and run `npm run sync-nix`.

# The generated Nix files include the *.nix files and registry.dat.

node2nix --nodejs-12 -l package-lock.json
elm2nix init > elm.nix
elm2nix convert > elm-srcs.nix
elm2nix snapshot

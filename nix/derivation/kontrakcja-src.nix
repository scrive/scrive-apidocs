let
  excluded = [
    ".git"

    "dist"
    "dist-newstyle"
    "node_modules"

    "nix"
    "shell.nix"
    "release.nix"

    "workspace"
    "cabal.project.freeze"
    "package-lock.json"

    "_local"
    "_shake"
    "_build"
    "test_data"

    "_nix_local"
    "s3files"
    "test_s3files"
  ];

  src = builtins.filterSource
    (path: type:
      ! builtins.elem (baseNameOf path) excluded

        # Don't copy .ghc.environemnt.* files
        && isNull (builtins.match "^\\.ghc\\.environment.+"
                    (baseNameOf path))
    )
    ../..
  ;
in
src

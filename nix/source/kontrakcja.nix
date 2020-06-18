let
  excluded = [
    ".git"

    "dist"
    "dist-newstyle"
    "node_modules"
    "docker"
    "supervisor"
    "elm-stuff"
    "registry.dat"
    "nix-support"

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
    "result"
  ];

  src = builtins.filterSource
    (path: type:
      ! builtins.elem (baseNameOf path) excluded

        && isNull (builtins.match ".+\.nix$"
                    (baseNameOf path))

        # Don't copy .ghc.environemnt.* files
        && isNull (builtins.match "^\\.ghc\\.environment.+"
                    (baseNameOf path))
    )
    ../..
  ;
in
src

let
  excluded = [
    ".git"

    "dist"
    "dist-newstyle"

    "nix"
    "shell.nix"
    "release.nix"
    "cabal.project.freeze"

    "_local"
    "workspace"
    "_nix_local"
    "test_data"
    "test_s3files"
  ];

  src = builtins.filterSource
    (path: type:
      ! builtins.elem (baseNameOf path) excluded

        # Don't copy .ghc.environemnt.* files
        && isNull (builtins.match "^\\.ghc\\.environment.+"
                    (baseNameOf path))

        # Don't copy frontend/ except for frontend/app/less which
        # is needed by the test suite.
        && (isNull (builtins.match ".+/frontend/.+" path)
            || ! isNull (builtins.match ".+/frontend/app" path)
            || ! isNull (builtins.match ".+/frontend/app/less.*" path))
    )
    ../..
  ;
in
src

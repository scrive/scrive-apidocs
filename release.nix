# This is the entry file to build the projects contained in this repository.

{ nixpkgs, compiler, scrivepdftools, ... }:
let 
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskell.packages.${compiler};

  # A derivation containing fakes3t1 and its dependencies.
  rubyEnv = pkgs.bundlerEnv {
    inherit (pkgs) ruby;
    name = "kontrakcja-ruby-env";
    gemset = ./nix/gemset.nix;
    gemdir = ./.;
  };

in rec {
  kontrakcja =
    let
      # Nix copies the source into the Nix store before building the project.
      # Some files in the repository are not useful and we don't want to copy
      # them.
      excluded = [
        "dist" "dist-newstyle" ".git" "shell.nix" "release.nix" "nix"
        "test_data" "test_s3files"
      ];

      src = builtins.filterSource
        (path: type: ! builtins.elem (baseNameOf path) excluded
                     # Don't copy .ghc.environemnt.* files
                     && isNull (builtins.match "^\\.ghc\\.environment.+"
                                 (baseNameOf path))
                     # Don't copy frontend/ except for frontend/app/less which
                     # is needed by the test suite.
                     && (isNull (builtins.match ".+/frontend/.+" path)
                         || ! isNull (builtins.match ".+/frontend/app" path)
                         || ! isNull (builtins.match ".+/frontend/app/less.*" path))
        ) ./.;

      # In order to save some time, we skip documentation and profiling.
      drv = pkgs.haskell.lib.overrideCabal
        (haskellPackages.callCabal2nix "kontrakcja" src {}) (super: {
          doHaddock = false;
          doHoogle = false;
          enableLibraryProfiling = false;
        });

    # FIXME: pass --show-details=streaming to cabal test to have the output show
    # up in Hydra as the tests are running.
    # This would need change in nixpkgs, we would need something like
    # testFlags, see buildFlags.
    in
    pkgs.lib.overrideDerivation drv (super: {
      buildInputs = with pkgs; super.buildInputs ++
        [ gnuplot imagemagick mupdf pngquant poppler_utils qrencode zbar zulu8
          aws-sam-cli postgresql rubyEnv curl lessc glibcLocales
        ];

      checkPhase = ''
        # Set up the test environment. See the script file.
        scrivepdftools=${scrivepdftools}
        ${builtins.readFile ./nix/kontrakcja-test-setup.sh}

        # Run the tests.
        ${super.checkPhase}
      '';
    });

  kontrakcja-frontend = import ./frontend { inherit kontrakcja; };
}

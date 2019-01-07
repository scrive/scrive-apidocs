# This is the entry file for nix-shell. It contains a Nix expression that
# evaluates to a derivation that builds a shell environment with all the
# tools a developer needs to compile and run the project.
#
# Some things are missing though:
# - If you want to run the Scrive PDF tools with SAM when running tests, you
#   have to install Docker and your user must be able to create containers.
#
# The following arguments can be passed when calling nix-shell like this:
# $ nix-shell --argstr compiler ghc843
# To pass a Nix expression,
# $ nix-shell --arg nixpkgs 'import <nixpkgs> ...'

{ compiler ? "ghc844"

, scrivepdftoolsSource ? builtins.fetchGit {
    url = "git@github.com:scrive/new-scrive-pdf-tools.git";
    rev = "0ea006685cfc90db5c9199580aad070e14d2810e";
  }

, nixpkgsRevision ? (builtins.replaceStrings ["\n"] [""]
    (builtins.readFile ./nix/nixpkgs-revision))

, nixpkgsSource ? builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = nixpkgsRevision;
  }
}:

let 
  # This configuration overloads some packages.
  config = import ./nix/nixpkgs-config.nix { inherit compiler; };
  nixpkgs = import nixpkgsSource { inherit config; };

  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;

  scrivepdftools = import (scrivepdftoolsSource + /release.nix) { inherit nixpkgs; };

  release = import ./release.nix {
    inherit compiler nixpkgs;
    inherit (scrivepdftools) scrivepdftools;
  };

  isHaskellLibrary = pkg: pkg ? isHaskellLibrary && pkg.isHaskellLibrary;

  # Build a derivation for GHC with (almost) all the packages on which the
  # Haskell depends. This way, cabal won't have to recompile them and they
  # can be fetched from a binary cache.
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (haskellPackages:
    let
      deps = builtins.filter isHaskellLibrary
        release.kontrakcja.propagatedBuildInputs;
    in
    deps ++ [
      haskellPackages.shake
    ]
  );

  sigshield = import ./nix/sigshield.nix { inherit nixpkgs; };

  # This contains additional tools we need in the development environment which
  # are not part of the packages already.
  devShell = pkgs.mkShell {
    nativeBuildInputs = [
      ghc
    ];

    buildInputs = with pkgs; with haskell.packages.${compiler}; [
      alex
      happy
      nodePackages.grunt-cli
      pkgs.cabal-install
      pkgs.jq
      postgresql
      sigshield
      which
    ];
  };

in
# Build a shell environment based on other derivations.
pkgs.mkShell {
  inputsFrom = with release; [
    kontrakcja
    kontrakcja-frontend
    # devShell needs to be last so that its GHC is before the generic one in PATH
    devShell
  ];

  shellHook = with pkgs; ''
    export LD_LIBRARY_PATH=${curl.out}/lib:${icu.dev}/lib:$LD_LIBRARY_PATH
    export PHANTOMJS_BIN=${phantomjs2}/bin/phantomjs

    scrivepdftools="${scrivepdftools.scrivepdftools}"
    ${builtins.readFile ./nix/kontrakcja-shell-setup.sh}
  '';
}

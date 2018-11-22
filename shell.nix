# This is the entry file for nix-shell. It contains a Nix expression that
# evaluates to a derivation that builds a shell environment with all the
# tools a developer needs to compile and run the project.
#
# Some things are missing though:
# - You need to have a PostgreSQL instance running to build and test the project
#   with shake.
# - If you want to run the Scrive PDF tools with SAM when running tests, you
#   have to install Docker and your user must be able to create containers.
#
# The following arguments can be passed when calling nix-shell like this:
# $ nix-shell --argstr compiler ghc843
# To pass a Nix expression,
# $ nix-shell --arg nixpkgs 'import <nixpkgs> ...'

{ compiler ? "ghc844"

, scrivepdftools ? builtins.fetchGit {
    url = "https://github.com/scrive/new-scrive-pdf-tools.git";
    rev = "cd1df73f12dfcdc61bdd7c021310ee04ef6fb012";
  }

, nixpkgs-revision ? (builtins.replaceStrings ["\n"] [""]
    (builtins.readFile ./nix/nixpkgs-revision))

, nixpkgs-source ? builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = nixpkgs-revision;
  }
}:

let 
  # This configuration overloads some packages.
  config = import ./nix/nixpkgs-config.nix { inherit compiler; };
  nixpkgs = import nixpkgs-source { inherit config; };

  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;

  scrivepdftools = import (scrivepdftools + /release.nix) { inherit nixpkgs; };

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
      phantomjs
      pkgs.cabal-install
      postgresql
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
    export LD_LIBRARY_PATH=${curl.out}/lib:${icu.dev}/lib:${glibc}/lib:$LD_LIBRARY_PATH
    export PHANTOMJS_BIN=${phantomjs}/bin/phantomjs
  '';
}

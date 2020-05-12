{ nixpkgs }:
let
  scrivepdftoolsSource = builtins.fetchGit {
    url = "ssh://git@github.com/scrive/new-scrive-pdf-tools.git";
    rev = "9cc08f31920ce89376e7ac75cf10509b1d44b850";
  };

  scrivepdftools = import (scrivepdftoolsSource + /release.nix) { inherit nixpkgs; };
in
scrivepdftools.scrivepdftools

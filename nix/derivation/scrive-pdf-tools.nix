{ nixpkgs }:
let
  scrivepdftoolsSource = builtins.fetchGit {
    url = "ssh://git@github.com/scrive/new-scrive-pdf-tools.git";
    rev = "133c0c2f740035d6fd3101cef7733586e5d4e784";
  };

  scrivepdftools = import (scrivepdftoolsSource + /release.nix) { inherit nixpkgs; };
in
scrivepdftools.scrivepdftools

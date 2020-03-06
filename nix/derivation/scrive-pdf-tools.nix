{ nixpkgs }:
let
  scrivepdftoolsSource = builtins.fetchGit {
    url = "ssh://git@github.com/scrive/new-scrive-pdf-tools.git";
    rev = "3b8f3dbd6f46bc97da295e9608e672d2fb4eb304";
  };

  scrivepdftools = import (scrivepdftoolsSource + /release.nix) { inherit nixpkgs; };
in
scrivepdftools.scrivepdftools

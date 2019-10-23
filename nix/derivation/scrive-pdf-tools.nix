{ nixpkgs }:
let
  scrivepdftoolsSource = builtins.fetchGit {
    url = "ssh://git@github.com/scrive/new-scrive-pdf-tools.git";
    rev = "40d520d1fc7d0a81338c945f24b7e881f70ce1ae";
  };

  scrivepdftools = import (scrivepdftoolsSource + /release.nix) { inherit nixpkgs; };
in
scrivepdftools.scrivepdftools

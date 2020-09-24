{ nixpkgs
, pdftools-src
}:
let
  scrivepdftools = import (pdftools-src + /release.nix) { inherit nixpkgs; };
in
scrivepdftools.scrivepdftools

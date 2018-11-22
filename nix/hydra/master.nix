# This file is used to build the latest version of all parts of the Scrive
# system, i.e. the back-end, front-end, PDF tools, etc.

let
  compiler = "ghc844";
  config = import ../nixpkgs-config.nix {
    inherit compiler;
  };
  nixpkgs = import <nixpkgs> { inherit config; };

  scrivepdftools = import <scrivepdftools/release.nix> { inherit nixpkgs; };
  kontrakcja = import <kontrakcja/release.nix> {
    inherit nixpkgs compiler;
    inherit (scrivepdftools) scrivepdftools;
  };

in {
  inherit (kontrakcja) kontrakcja kontrakcja-frontend;
  inherit (scrivepdftools) scrivepdftools;
}

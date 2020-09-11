{ nixpkgs }:
let
  inherit (nixpkgs) pkgs;

  rubyEnv = pkgs.bundlerEnv {
    inherit (pkgs) ruby;
    name = "fakes3";
    gemset = ./gemset.nix;
    gemdir = ./.;
  };
in
rubyEnv

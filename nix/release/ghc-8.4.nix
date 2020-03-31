{
  nixpkgs ? import ./nixpkgs.nix {}
, localeLang ? "en_US.UTF-8"
, workspaceRoot ? builtins.toPath(../..)
}:
let
  release-path = /release/ghc-8.4.nix;
in
import ./release.nix {
  inherit
    nixpkgs
    localeLang
    release-path
    workspaceRoot
  ;
}

{
  nixpkgs ? import ./nixpkgs.nix {}
, workspaceRoot ? builtins.toPath(../..)
, localeLang ? "C.UTF-8"
}:
let
  ghcVersion = "ghc881";
in
import ./release.nix {
  inherit nixpkgs ghcVersion workspaceRoot localeLang;
}

{
  nixpkgs ? import ./nixpkgs.nix {}
, workspaceRoot ? builtins.toPath(../..)
, localeLang ? "C.UTF-8"
}:
let
  ghcVersion = "ghc865";
in
import ./release.nix {
  inherit nixpkgs ghcVersion workspaceRoot localeLang;
}

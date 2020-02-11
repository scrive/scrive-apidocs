{
  nixpkgs ? import ./nixpkgs.nix {}
, workspaceRoot ? builtins.toPath(../..)
, localeLang ? "en_US.UTF-8"
}:
let
  ghcVersion = "ghc865";
in
import ./release.nix {
  inherit nixpkgs ghcVersion workspaceRoot localeLang;
}

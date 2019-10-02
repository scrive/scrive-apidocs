{
  nixpkgs ? import ./nixpkgs.nix {}
, workspaceRoot ? builtins.toPath(../..)
}:
let
  ghcVersion = "ghc881";
in
import ./release.nix {
  inherit nixpkgs ghcVersion workspaceRoot;
}

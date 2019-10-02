{
  nixpkgs ? import ./nixpkgs.nix
, workspaceRoot ? builtins.toPath(../..)
}:
let
  ghcVersion = "ghc844";
  inherit (nixpkgs) pkgs;
  haskellPackages-1 = pkgs.haskell.packages.${ghcVersion};
  haskellPackages-2 = haskellPackages-1.override (old: {
    # Use composeExtensions to prevent Nix from obscurely
    # drop any previous overrides
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        # Nix's GHC 8.4.4's base-noprelude somehow have version mismatch
        # with 8.4.4's base version, so we have to override with older version
        base-noprelude = super.callHackage "base-noprelude" "4.11.1.0" {};

        # Diff tests compilation failed when building on GHC 8.4.
        # Remove this when it is fixed.
        Diff = pkgs.haskell.lib.dontCheck
          (super.callHackage "Diff" "0.3.4" {});
      });
    });
in
import ./release.nix {
  inherit nixpkgs ghcVersion workspaceRoot;
  inHaskellPackages = haskellPackages-2;
}

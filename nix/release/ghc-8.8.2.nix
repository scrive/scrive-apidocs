{
  nixpkgs ? import ./nixpkgs.nix {}
, workspaceRoot ? builtins.toPath(../..)
, localeLang ? "en_US.UTF-8"
}:
let
  ghcVersion = "ghc882";
  inherit (nixpkgs) pkgs;
  haskellPackages-1 = pkgs.haskell.packages.${ghcVersion};
  haskellPackages-2 = haskellPackages-1.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        Cabal = super.callHackage
            "Cabal"
            "3.0.0.0"
            {}
        ;

        base-noprelude = super.callHackage
            "base-noprelude"
            "4.13.0.0"
            {}
        ;
      });
    });
in
import ./release.nix {
  inherit nixpkgs ghcVersion workspaceRoot localeLang;
  inHaskellPackages = haskellPackages-2;
}

{
  nixpkgs ? import ./nixpkgs.nix {}
, workspaceRoot ? builtins.toPath(../..)
, localeLang ? "en_US.UTF-8"
}:
let
  ghcVersion = "ghc865";
  inherit (nixpkgs) pkgs;


  callPackage = self: name: src:
    self.callCabal2nix
      name
      src
      {}
  ;

  callGitPackage = self: name: url: rev:
    callPackage self
        name
        (builtins.fetchGit {
          inherit url rev;
        })
      ;

  haskellPackages-1 = pkgs.haskell.packages.${ghcVersion};
  haskellPackages-2 = haskellPackages-1.override (old: {
    overrides = pkgs.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        base-noprelude = super.callHackage
          "base-noprelude"
          "4.12.0.0"
          {}
        ;
      });
    });
in
import ./release.nix {
  inherit nixpkgs ghcVersion workspaceRoot localeLang;
  inHaskellPackages = haskellPackages-2;
}

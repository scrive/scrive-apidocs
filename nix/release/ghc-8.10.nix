{
  nixpkgs ? import ./nixpkgs.nix {}
, workspaceRoot ? builtins.toPath(../..)
, localeLang ? "en_US.UTF-8"
}:
let
  ghcVersion = "ghc8101";
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
        base-noprelude = pkgs.haskell.lib.appendPatch
          ( super.callHackage
              "base-noprelude"
              "4.13.0.0"
              {}
          )
          ../patches/base-noprelude.patch
        ;

        cryptohash-sha256 = pkgs.haskell.lib.appendPatch
          ( callGitPackage super
              "cryptohash-sha256"
              "https://github.com/haskell-hvr/cryptohash-sha256.git"
              "48b0a9cda7405da2b3477d40ff4505b4842520b5"
          )
          ../patches/cryptohash-sha256.patch
        ;
      });
    });
in
import ./release.nix {
  inherit nixpkgs ghcVersion workspaceRoot localeLang;
  inHaskellPackages = haskellPackages-2;
}

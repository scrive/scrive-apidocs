{ haskell-nix }:
haskell-nix.pkgs.haskell-nix.haskellLib.cleanGit {
  name = "kontrakcja-src";
  src = ../../.;
}

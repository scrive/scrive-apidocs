{
  nixpkgs
, haskellPackages
}:
let
  src = import ./kontrakcja-src.nix;
in
haskellPackages.callCabal2nix
  "kontrakcja-shake"
  (src + "/Shake")
  {}

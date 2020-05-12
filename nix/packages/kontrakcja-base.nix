{
  nixpkgs
, haskellPackages
}:
let
  src = import ../source/kontrakcja.nix;
in
haskellPackages.callCabal2nix
  "kontrakcja"
  src
  {}

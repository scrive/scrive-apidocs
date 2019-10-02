{
  nixpkgs
, kontrakcja
}:
let
  kontrakcja-frontend = import ../../frontend/default.nix {
    inherit nixpkgs kontrakcja;
  };
in
kontrakcja-frontend

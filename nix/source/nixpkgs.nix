{ useLocal }:
let
  kontrakcja-nix-src = import ./kontrakcja-nix.nix
    { inherit useLocal; };

  nixpkgs-src = kontrakcja-nix-src + /source/nixpkgs.nix;
in
import nixpkgs-src

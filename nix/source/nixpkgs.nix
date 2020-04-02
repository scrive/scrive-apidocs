let
  kontrakcja-nix-src = import ./kontrakcja-nix.nix;
  nixpkgs-src = kontrakcja-nix-src + /source/nixpkgs.nix;
in
import nixpkgs-src

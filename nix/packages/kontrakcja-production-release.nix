{
  nixpkgs
, haskellPackages
, nixpkgs-src
}:
let
  inherit (nixpkgs) pkgs;

  sourceRoot = builtins.toPath(../..);

  kontrakcja-src = import ../source/kontrakcja.nix;

  kontrakcja-base = import ./kontrakcja-base.nix {
    inherit nixpkgs haskellPackages;
  };

  kontrakcja =
    # We haven't figure how to run kontrakcja tests in Nix sandbox yet.
    pkgs.haskell.lib.dontCheck
      ( pkgs.haskell.lib.dontHaddock
          kontrakcja-base
      );

  kontrakcja-frontend = import ./kontrakcja-frontend.nix {
    inherit nixpkgs kontrakcja kontrakcja-src nixpkgs-src;
  };

  scrivepdftools = import ./scrive-pdf-tools.nix { inherit nixpkgs; };
in
{
  inherit kontrakcja kontrakcja-frontend scrivepdftools;
}

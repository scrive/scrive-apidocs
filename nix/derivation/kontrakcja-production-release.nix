{
  nixpkgs
, haskellPackages
}:
let
  inherit (nixpkgs) pkgs;

  sourceRoot = builtins.toPath(../..);

  kontrakcja-src = import ./kontrakcja-src.nix;

  kontrakcja-base = import ./kontrakcja-base.nix {
    inherit nixpkgs haskellPackages;
  };

  kontrakcja = pkgs.haskell.lib.overrideCabal
    kontrakcja-base (super: {
      doCheck = false;
      doHaddock = true;
      doHoogle = true;
      enableLibraryProfiling = false;
    });

  kontrakcja-frontend = import ./kontrakcja-frontend.nix {
    inherit nixpkgs kontrakcja kontrakcja-src;
  };

  scrivepdftools = import ./scrive-pdf-tools.nix { inherit nixpkgs; };
in
{
  inherit kontrakcja kontrakcja-frontend scrivepdftools;
}

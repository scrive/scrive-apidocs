{
  nixpkgs
, haskellPackages
, workspaceRoot ? builtins.toPath(../..)
}:
let
  inherit (nixpkgs) pkgs;

  sourceRoot = builtins.toPath(../..);

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
    inherit nixpkgs kontrakcja;
  };

  scrivepdftools = import ./scrive-pdf-tools.nix { inherit nixpkgs; };
in
{
  inherit kontrakcja kontrakcja-frontend scrivepdftools;
}

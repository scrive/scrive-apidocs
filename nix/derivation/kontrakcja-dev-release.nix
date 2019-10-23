{
  nixpkgs
, haskellPackages
}:
let
  inherit (nixpkgs) pkgs;

  kontrakcja-src = import ./kontrakcja-src.nix;

  kontrakcja-1 = import ./kontrakcja-base.nix {
    inherit nixpkgs haskellPackages;
  };

  kontrakcja-shake = import ./kontrakcja-shake.nix {
    inherit nixpkgs haskellPackages;
  };

  kontrakcja = pkgs.haskell.lib.overrideCabal
    kontrakcja-1 (super: {
      # test requires other components to be running, so we won't
      # test while building from source code only
      doCheck = false;
      doHaddock = false;
      doHoogle = false;
      enableLibraryProfiling = false;
    });

  kontrakcja-frontend = import ./kontrakcja-frontend.nix {
    inherit nixpkgs kontrakcja kontrakcja-src;
  };
in
{
  inherit kontrakcja kontrakcja-frontend kontrakcja-shake;
}

{
  nixpkgs
, haskellPackages
}:
let
  inherit (nixpkgs) pkgs;

  kontrakcja-src = import ../source/kontrakcja.nix;

  kontrakcja-1 = import ./kontrakcja-base.nix {
    inherit nixpkgs haskellPackages;
  };

  kontrakcja-shake = import ./kontrakcja-shake.nix {
    inherit nixpkgs haskellPackages;
  };

  kontrakcja =
    pkgs.haskell.lib.disableOptimization (
      pkgs.haskell.lib.dontCheck
        ( pkgs.haskell.lib.dontHaddock
            kontrakcja-1
        ) );

  kontrakcja-frontend = import ./kontrakcja-frontend.nix {
    inherit nixpkgs kontrakcja kontrakcja-src;
  };

  kontrakcja-frontend-elm = import ./kontrakcja-frontend-elm.nix {
    inherit nixpkgs kontrakcja-src;
  };
in
{
  inherit
    kontrakcja
    kontrakcja-frontend
    kontrakcja-shake
    kontrakcja-frontend-elm
  ;
}

{ checkMaterialization ? false
, nixpkgs ? import ../modules/nixpkgs.nix
}:
let
  args = { inherit checkMaterialization; };

  ghc88 = import ./ghc88.nix args;
  ghc810 = import ./ghc810.nix args;

  new-frontend = import ./new-frontend.nix;


  haskell-nix = import ../modules/haskell-nix.nix;

  kontrakcja-src = import ../source/kontrakcja.nix {
    inherit haskell-nix;
  };

  api-docs = import ../modules/api-docs.nix {
    inherit nixpkgs kontrakcja-src;
  };

  haskell-plans = import ./plan.nix;
in
{ inherit
    ghc88
    ghc810
    api-docs
    new-frontend
    haskell-plans
    kontrakcja-src
  ;
}

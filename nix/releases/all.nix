{ checkMaterialization ? false
, nixpkgs ? import ../modules/nixpkgs.nix
}:
let
  args = { inherit checkMaterialization; };

  ghc88 = import ./ghc88.nix args;
  ghc810 = import ./ghc810.nix args;

  new-frontend = import ./new-frontend.nix;
  haskell-plans = import ./plan.nix;
in
{ inherit
    ghc88
    ghc810
    new-frontend
    haskell-plans
  ;
}

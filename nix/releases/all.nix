{ checkMaterialization ? false
, nixpkgs ? import ../modules/nixpkgs.nix
}:
let
  args = { inherit checkMaterialization; };

  ghc86 = import ./ghc86.nix args;
  ghc88 = import ./ghc88.nix args;

  new-frontend = import ./new-frontend.nix;
in
{ inherit ghc86 ghc88
    new-frontend
  ;
}

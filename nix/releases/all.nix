{ checkMaterialization ? false
}:
let
  args = { inherit checkMaterialization; };

  ghc86 = import ./ghc86.nix args;
  ghc88 = import ./ghc88.nix args;
in
{ inherit ghc86 ghc88;
}

{ checkMaterialization ? false
}:
let
  args = { inherit checkMaterialization; };
in
(import ./release.nix args).ghc88.dev-shell

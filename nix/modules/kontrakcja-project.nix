{ ghc-version
, nixpkgs
, haskell-nix
, kontrakcja-src
, materialized-plan
, hackage-index-state
, checkMaterialization
}:
let
  inherit (materialized-plan.kontrakcja) plan hash;
in
haskell-nix.pkgs.haskell-nix.cabalProject {
  inherit checkMaterialization;

  src = kontrakcja-src;

  compiler-nix-name = ghc-version;

  index-state = hackage-index-state;

  plan-sha256 = hash;
  materialized = plan;

  modules = [
    {
      reinstallableLibGhc = true;
    }
  ];
}

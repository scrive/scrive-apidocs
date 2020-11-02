{ nixpkgs
, ghc-version
, haskell-nix
, kontrakcja-src
, hackage-index-state
}:
let
  checkMaterialization = false;
  no-plan = { plan = null; hash = null; };

  materialized-plan =
    { kontrakcja = no-plan;
      hlint = no-plan;
      brittany = no-plan;
      apply-refact = no-plan;
      cabal-install = no-plan;
    };

  kontrakcja-project = import ./kontrakcja-project.nix
    { inherit nixpkgs
        ghc-version
        haskell-nix
        kontrakcja-src
        materialized-plan
        hackage-index-state
        checkMaterialization
      ;
    };

  haskell-deps = import ./haskell-deps.nix
    { inherit nixpkgs
        ghc-version
        haskell-nix
        materialized-plan
        hackage-index-state
        checkMaterialization
      ;
    };

  inherit (haskell-deps) projects;

  do-build-plan = import ./build-plan.nix;

  build-plan = name: project:
    do-build-plan
      { inherit nixpkgs name project; };

  kontrakcja = build-plan "kontrakcja" kontrakcja-project;

  hlint = build-plan "hlint" projects.hlint;

  brittany = build-plan "brittany" projects.brittany;

  apply-refact = build-plan "apply-refact" projects.apply-refact;

  cabal-install = build-plan "cabal-install" projects.cabal-install;
in
nixpkgs.stdenv.mkDerivation {
  name = "kontrakcja-all-plans";

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out

    cp -r ${kontrakcja} $out/kontrakcja
    cp -r ${hlint} $out/hlint
    cp -r ${brittany} $out/brittany
    cp -r ${apply-refact} $out/apply-refact
    cp -r ${cabal-install} $out/cabal-install
  '';

}

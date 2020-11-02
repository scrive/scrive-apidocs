{ nixpkgs
, ghc-version
, haskell-nix
, materialized-plan
, hackage-index-state
, checkMaterialization
}:
let
  haskell-nix-pkgs = haskell-nix.pkgs.haskell-nix;

  cabal-install = haskell-nix-pkgs.hackage-project {
    compiler-nix-name = ghc-version;
    index-state = hackage-index-state;

    name = "cabal-install";
    version = "3.2.0.0";

    plan-sha256 = materialized-plan.cabal-install.hash;
    materialized = materialized-plan.cabal-install.plan;
  };

  hlint = haskell-nix-pkgs.hackage-project {
    inherit checkMaterialization;

    compiler-nix-name = ghc-version;
    index-state = hackage-index-state;

    name = "hlint";
    version = "2.2.11";

    plan-sha256 = materialized-plan.hlint.hash;
    materialized = materialized-plan.hlint.plan;
  };

  brittany = haskell-nix-pkgs.hackage-project {
    inherit checkMaterialization;

    # Hardcode GHC version as it is only supported GHC 8.8
    compiler-nix-name = "ghc884";
    index-state = hackage-index-state;

    name = "brittany";
    version = "0.12.1.0";

    plan-sha256 = materialized-plan.brittany.hash;
    materialized = materialized-plan.brittany.plan;
  };

  apply-refact = haskell-nix-pkgs.hackage-project {
    inherit checkMaterialization;

    compiler-nix-name = ghc-version;
    index-state = hackage-index-state;

    name = "apply-refact";
    version = "0.8.1.0";

    plan-sha256 = materialized-plan.apply-refact.hash;
    materialized = materialized-plan.apply-refact.plan;
  };

  get-exe = project: project-name: exe-name:
    project.hsPkgs.${project-name}.components.exes.${exe-name};

  exes = {
    hlint = get-exe hlint "hlint" "hlint";

    brittany = get-exe brittany "brittany" "brittany";

    apply-refact = get-exe apply-refact "apply-refact" "refactor";

    cabal-install = get-exe cabal-install "cabal-install" "cabal";
  };

  projects = {
    inherit
      hlint
      brittany
      apply-refact
      cabal-install
    ;
  };

  plans = {
    hlint = hlint.plan-nix;

    brittany = brittany.plan-nix;

    apply-refact = apply-refact.plan-nix;

    cabal-install = cabal-install.plan-nix;
  };
in
{ inherit
    exes
    plans
    projects
  ;
}

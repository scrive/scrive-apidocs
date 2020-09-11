{ ghc-version }:
{ checkMaterialization ? false
}:
let
  haskell-nix = import ./haskell-nix.nix;

  nixpkgs-src = import ../source/nixpkgs.nix;

  hackage-index-state = import ../source/hackage.nix;

  haskell-nix-pkgs = haskell-nix.pkgs.haskell-nix;

  nixpkgs = import nixpkgs-src {};

  materialized-plan = import ./materialized-plan.nix
    { inherit nixpkgs ghc-version; };

  run-deps = import ./run-deps.nix
    { inherit nixpkgs haskell-nix; };

  haskell-deps = import ./haskell-deps.nix
    { inherit
        nixpkgs
        haskell-nix
        ghc-version
        materialized-plan
        hackage-index-state
        checkMaterialization
      ;
    };

  cabal-install = import ./wrap-cabal.nix
    { inherit nixpkgs;

      cabal-install = haskell-deps.exes.cabal-install;
      cabal-project = "cabal-nix.project";
    };

  kontrakcja-src = import ../source/kontrakcja.nix
    { inherit haskell-nix; };

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

  kontrakcja-frontend = import ./kontrakcja-frontend.nix
    { inherit nixpkgs nixpkgs-src
        kontrakcja-src kontrakcja-project;
    };

  pdftools-src = import ../source/pdftools.nix;

  scrivepdftools = import ./scrive-pdf-tools.nix
    { inherit nixpkgs pdftools-src; };

  dev-shell = import ./shells/dev.nix
    { inherit
        nixpkgs
        run-deps
        ghc-version
        haskell-deps
        cabal-install
        scrivepdftools
        kontrakcja-project
      ;
    };

  manual-shell = import ./shells/manual.nix
    { inherit
        nixpkgs
        run-deps
        ghc-version
        haskell-deps
      ;
    };

  backend-shell = import ./shells/backend.nix
    { inherit
        nixpkgs
        ghc-version
        haskell-nix
        haskell-deps
        cabal-install
        kontrakcja-project
      ;
    };

  manual-backend-shell = import ./shells/manual-backend.nix
    { inherit
        nixpkgs
        run-deps
        ghc-version
        haskell-nix
        haskell-deps
      ;
    };

  frontend-shell = import ./shells/frontend.nix
    { inherit nixpkgs
        haskell-deps
        cabal-install
        kontrakcja-project
      ;
    };

  selenium-shell = import ./shells/selenium.nix
    { inherit nixpkgs; };

  lint-shell = import ./shells/lint.nix
    { inherit nixpkgs
        haskell-nix
        haskell-deps
        cabal-install
        kontrakcja-project
      ;
    };

  detect-unused-shell = import ./shells/detect-unused.nix
    { inherit nixpkgs
        kontrakcja-project
      ;
    };

  dist-shell = import ./shells/dist.nix
    { inherit nixpkgs; };

  shell-deps = import ./shell-deps.nix
    { inherit nixpkgs;
      shells = [
        dev-shell
        lint-shell
        dist-shell
        manual-shell
        backend-shell
        frontend-shell
        selenium-shell
        detect-unused-shell
        manual-backend-shell
      ];
    };

  build-plan = import ./build-plans.nix
    { inherit
        nixpkgs
        ghc-version
        haskell-nix
        kontrakcja-src
        hackage-index-state
      ;
    };

  dist = import ./dist.nix
    { inherit
        nixpkgs
        scrivepdftools
        kontrakcja-src
        kontrakcja-frontend
        kontrakcja-project
      ;
    };
in
{ inherit
    dist
    build-plan
    shell-deps
    ghc-version
    haskell-nix
    haskell-deps
    kontrakcja-src
    scrivepdftools
    kontrakcja-project
    kontrakcja-frontend

    dev-shell
    lint-shell
    dist-shell
    manual-shell
    backend-shell
    frontend-shell
    selenium-shell
    detect-unused-shell
    manual-backend-shell
  ;
}

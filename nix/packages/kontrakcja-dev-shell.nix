{
  nixpkgs
, haskellPackages
, kontrakcja-nix-src
, pdftools-src
, nixpkgs-src
, extra-run-deps
, localeLang
, workspaceRoot
}:
let
  inherit (nixpkgs) pkgs;

  sourceRoot = builtins.toPath(../..);

  run-deps = import (kontrakcja-nix-src + /packages/run-deps.nix)
    { inherit nixpkgs haskellPackages extra-run-deps;
      wrapCabal = true;
    };

  release = import ./kontrakcja-dev-release.nix {
    inherit nixpkgs haskellPackages nixpkgs-src;
  };

  inherit (release) kontrakcja-shake kontrakcja-frontend;

  kontrakcja = pkgs.haskell.lib.doCheck release.kontrakcja;

  scrivepdftools = import ./scrive-pdf-tools.nix
    { inherit nixpkgs pdftools-src; };

  elm2nix = import ./elm2nix.nix { inherit nixpkgs; };
in
haskellPackages.shellFor {
  name = "kontrakcja-dev-shell";

  inherit scrivepdftools;

  KONTRAKCJA_ROOT = sourceRoot;
  KONTRAKCJA_WORKSPACE = workspaceRoot;

  packages = ps: [ kontrakcja kontrakcja-shake ];

  buildInputs =
    run-deps ++
    kontrakcja-frontend.buildInputs
  ;

  LANG = localeLang;

  SKIP_CABAL_UPDATE = 1;
}

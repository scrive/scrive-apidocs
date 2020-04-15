{
  nixpkgs
, haskellPackages
, kontrakcja-nix-src
, nixpkgs-src
, localeLang ? "en_US.UTF-8"
, workspaceRoot ? builtins.toPath(../..)
}:
let
  inherit (nixpkgs) pkgs;

  sourceRoot = builtins.toPath(../..);

  run-deps = import (kontrakcja-nix-src + /packages/run-deps.nix)
    { inherit nixpkgs haskellPackages; };

  release = import ./kontrakcja-dev-release.nix {
    inherit nixpkgs haskellPackages nixpkgs-src;
  };

  inherit (release) kontrakcja-shake kontrakcja-frontend;

  kontrakcja = pkgs.haskell.lib.doCheck release.kontrakcja;

  scrivepdftools = import ./scrive-pdf-tools.nix { inherit nixpkgs; };

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

  shellHook = ''
    export LANG=${localeLang}

    cat << EOF
    ***************************************************************************
    You are now in Nix shell for kontrakcja. Your Nix workspace directory is:
    $KONTRAKCJA_WORKSPACE

    To build kontrakcja, run:
    cd "\$KONTRAKCJA_WORKSPACE" && ./shake.sh all

    To start dev server, run:
    cd "\$KONTRAKCJA_WORKSPACE" && ./run-dev.sh

    For more info please refer to README.md
    ***************************************************************************

    EOF
  '';
}

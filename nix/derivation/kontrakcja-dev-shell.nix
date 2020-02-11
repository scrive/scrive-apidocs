{
  nixpkgs
, haskellPackages
, localeLang ? "en_US.UTF-8"
, workspaceRoot ? builtins.toPath(../..)
}:
let
  inherit (nixpkgs) pkgs;

  sourceRoot = builtins.toPath(../..);

  run-deps = import ./kontrakcja-run-deps.nix { inherit nixpkgs; };

  release = import ./kontrakcja-dev-release.nix {
    inherit nixpkgs haskellPackages;
  };

  inherit (release) kontrakcja-shake kontrakcja-frontend;

  # Enable checking to build test dependencies in Nix
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
    kontrakcja-frontend.buildInputs ++
    [
      elm2nix
      scrivepdftools
      haskellPackages.alex
      haskellPackages.happy
      haskellPackages.brittany
      haskellPackages.cabal-install
      pkgs.nodePackages.less
      pkgs.nodePackages.yarn
      pkgs.nodePackages.grunt-cli
      pkgs.nodePackages.node2nix
      pkgs.elmPackages.elm
      pkgs.elmPackages.elm-format
      pkgs.libxml2
    ];

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

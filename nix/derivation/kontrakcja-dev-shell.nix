{
  nixpkgs
, haskellPackages
, localeLang ? "C.UTF-8"
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
      scrivepdftools
      haskellPackages.alex
      haskellPackages.happy
      haskellPackages.brittany
      haskellPackages.cabal-install
      pkgs.nodePackages.less
      pkgs.nodePackages.grunt-cli
    ];

  shellHook = ''
    export LANG=${localeLang}
  '';
}

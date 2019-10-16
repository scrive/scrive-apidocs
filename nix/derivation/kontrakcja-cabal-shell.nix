{
  nixpkgs
, haskellPackages
, localeLang ? "C.UTF-8"
, workspaceRoot ? builtins.toPath(../..)
}:
let
  inherit (nixpkgs) pkgs;

  sourceRoot = builtins.toPath(../..);

  release = import ./kontrakcja-dev-release.nix {
    inherit nixpkgs haskellPackages;
  };

  inherit (release) kontrakcja-shake;

  kontrakcja = pkgs.haskell.lib.doCheck release.kontrakcja;
in
haskellPackages.shellFor {
  name = "kontrakcja-shell";

  KONTRAKCJA_ROOT = sourceRoot;
  KONTRAKCJA_WORKSPACE = workspaceRoot;
  packages = ps: [ kontrakcja kontrakcja-shake ];

  buildInputs = [
    pkgs.glibcLocales
    pkgs.nodejs
    pkgs.nodePackages.less
    pkgs.nodePackages.grunt-cli
    haskellPackages.alex
    haskellPackages.happy
    haskellPackages.brittany
    haskellPackages.cabal-install
  ];

  shellHook = ''
    export LANG=${localeLang}
  '';
}

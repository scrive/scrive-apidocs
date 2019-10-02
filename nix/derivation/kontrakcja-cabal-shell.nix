{
  nixpkgs
, haskellPackages
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

  LANG = "en_US.UTF-8";

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
}

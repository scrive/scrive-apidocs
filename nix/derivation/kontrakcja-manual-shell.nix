{
  nixpkgs
, haskellPackages
, workspaceRoot ? builtins.toPath(../..)
}:
let
  inherit (nixpkgs) pkgs;

  sourceRoot = builtins.toPath(../..);

  ghc = haskellPackages.ghcWithPackages
    (pkg: []);
in
pkgs.mkShell {
  name = "kontrakcja-manual-shell";

  LANG = "en_US.UTF-8";
  LD_LIBRARY_PATH = "${pkgs.zlib}/lib:${pkgs.icu}/lib:${pkgs.curl.out}/lib";

  KONTRAKCJA_ROOT = sourceRoot;
  KONTRAKCJA_WORKSPACE = workspaceRoot;

  buildInputs = [
    ghc
    haskellPackages.alex
    haskellPackages.happy
    haskellPackages.brittany
    haskellPackages.cabal-install
    pkgs.nodejs
    pkgs.nodePackages.less
    pkgs.nodePackages.grunt-cli
    pkgs.jq
    pkgs.icu
    pkgs.curl
    pkgs.postgresql
    pkgs.glibcLocales
  ];
}

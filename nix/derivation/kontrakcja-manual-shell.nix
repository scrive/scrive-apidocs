{
  nixpkgs
, haskellPackages
, localeLang ? "en_US.UTF-8"
, workspaceRoot ? builtins.toPath(../..)
}:
let
  inherit (nixpkgs) pkgs;

  run-deps = import ./kontrakcja-run-deps.nix { inherit nixpkgs; };

  sourceRoot = builtins.toPath(../..);

  ghc = haskellPackages.ghcWithPackages
    (pkg: []);

  elm2nix = import ./elm2nix.nix { inherit nixpkgs; };
in
pkgs.mkShell {
  name = "kontrakcja-manual-shell";

  LD_LIBRARY_PATH = "${pkgs.zlib}/lib:${pkgs.icu}/lib:${pkgs.curl.out}/lib";

  KONTRAKCJA_ROOT = sourceRoot;
  KONTRAKCJA_WORKSPACE = workspaceRoot;

  buildInputs = [
    ghc
    elm2nix
    haskellPackages.alex
    haskellPackages.happy
    haskellPackages.brittany
    haskellPackages.cabal-install
    pkgs.pkgconfig
    pkgs.nodejs
    pkgs.nodePackages.less
    pkgs.nodePackages.yarn
    pkgs.nodePackages.grunt-cli
    pkgs.nodePackages.node2nix
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.jq
    pkgs.icu
    pkgs.curl
    pkgs.postgresql
    pkgs.glibcLocales
    pkgs.libxml2
  ] ++ run-deps;

  shellHook = ''
    export LANG=${localeLang}
  '';
}

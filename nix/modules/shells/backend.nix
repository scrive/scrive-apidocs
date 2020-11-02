# This is used for running back end tests on GitHub Actions

{ nixpkgs
, ghc-version
, cabal-install
, haskell-nix
, haskell-deps
, kontrakcja-project
}:
let
  inherit (nixpkgs) pkgs nodePackages elmPackages python3Packages;

  inherit (haskell-deps.exes)
    hlint brittany apply-refact;

  vendor = import ../vendor.nix { inherit nixpkgs; };

  run-deps = [
    cabal-install
    vendor.fakes3
    vendor.sam
    pkgs.sass

    elmPackages.elm
    nodePackages.less
    nodePackages.grunt-cli
    python3Packages.supervisor

    pkgs.jq
    pkgs.curl
    pkgs.zbar
    pkgs.mupdf
    pkgs.nodejs
    pkgs.xmlsec
    pkgs.gnuplot
    pkgs.killall
    pkgs.openjdk
    pkgs.pngquant
    pkgs.qrencode
    pkgs.postgresql
    pkgs.imagemagick
    pkgs.glibcLocales
    haskell-nix.pkgs.poppler_utils
  ];
in
kontrakcja-project.shellFor {
  packages = hsPkgs: [
    hsPkgs.kontrakcja
    hsPkgs.kontrakcja-shake
  ];

  buildInputs = run-deps;

  exactDeps = true;

  withHoogle = false;

  SKIP_CABAL_UPDATE = 1;

  shellHook = ''
    export LANG=''${LANG:-en_US.UTF-8}
    '';
}

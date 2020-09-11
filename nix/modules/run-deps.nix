{ nixpkgs
, haskell-nix
, ghc-version
}:
let
  inherit (nixpkgs) pkgs nodePackages elmPackages python3Packages;

  haskellPackages = nixpkgs.haskell.packages.${ghc-version};

  vendor = import ./vendor.nix { inherit nixpkgs; };

  vendorDeps = [
    vendor.elm2nix
    vendor.fakes3
  ];

  pythonDeps = [
    python3Packages.supervisor
  ];

  elmDeps = [
    elmPackages.elm
    elmPackages.elm-format
  ];

  nodeDeps = [
    nodePackages.less
    nodePackages.yarn
    nodePackages.grunt-cli
    nodePackages.node2nix
  ];

  haskellDeps = [
    haskellPackages.ghcid
  ];

  mainDeps = [
    pkgs.jq
    pkgs.git
    pkgs.icu
    pkgs.curl
    pkgs.zbar
    pkgs.zlib
    pkgs.dhall
    pkgs.mupdf
    pkgs.nginx
    pkgs.nodejs
    pkgs.xmlsec
    pkgs.libxml2
    pkgs.gnuplot
    pkgs.openjdk
    pkgs.pngquant
    pkgs.qrencode
    pkgs.cabal2nix
    pkgs.pkgconfig
    pkgs.dhall-json
    pkgs.postgresql
    pkgs.imagemagick
    pkgs.glibcLocales
    haskell-nix.pkgs.pkgconfig
    haskell-nix.pkgs.aws-sam-cli
    haskell-nix.pkgs.poppler_utils
  ];
in
     vendorDeps
  ++ pythonDeps
  ++ elmDeps
  ++ nodeDeps
  ++ haskellDeps
  ++ mainDeps

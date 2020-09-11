{ nixpkgs
, ghc-version
, run-deps
, haskell-nix
, haskell-deps
}:
let
  inherit (nixpkgs)
    pkgs zlib icu curl libxml2
    nodePackages python3Packages
  ;

  inherit (haskell-deps.exes) cabal-install;

  ghc = nixpkgs.haskell.packages.${ghc-version}.ghc;

  vendor = import ../vendor.nix { inherit nixpkgs; };

  run-deps = [
    ghc
    cabal-install
    vendor.fakes3
    nodePackages.less
    python3Packages.supervisor
    pkgs.jq
    pkgs.icu
    pkgs.curl
    pkgs.zbar
    pkgs.mupdf
    pkgs.nodejs
    pkgs.xmlsec
    pkgs.libxml2
    pkgs.gnuplot
    pkgs.openjdk
    pkgs.pngquant
    pkgs.qrencode
    pkgs.postgresql
    pkgs.imagemagick
    pkgs.glibcLocales
    haskell-nix.pkgs.pkgconfig
    haskell-nix.pkgs.poppler_utils
  ];
in
nixpkgs.mkShell {
  name = "kontrakcja-manual-backend-shell";

  LD_LIBRARY_PATH = "${zlib}/lib:${icu}/lib:${curl.out}/lib:${libxml2.out}/lib";

  buildInputs = run-deps;

  shellHook = ''
    export LANG=''${LANG:-en_US.UTF-8}
    '';
}

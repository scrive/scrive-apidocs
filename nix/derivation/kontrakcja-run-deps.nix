{ nixpkgs }:
let
  inherit (nixpkgs) pkgs;

  rubyEnv = pkgs.bundlerEnv {
    inherit (pkgs) ruby;
    name = "kontrakcja-ruby-env";
    gemset = ./gemset.nix;
    gemdir = ../../.;
  };

  run-deps = [ rubyEnv ] ++
    (with pkgs; [
      jq
      curl
      zbar
      mupdf
      lessc
      xmlsec
      gnuplot
      openjdk
      pngquant
      qrencode
      postgresql
      aws-sam-cli
      imagemagick
      glibcLocales
      poppler_utils
    ]);
in
run-deps

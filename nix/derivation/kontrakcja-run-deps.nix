{ nixpkgs }:
let
  inherit (nixpkgs) pkgs;

  rubyEnv = pkgs.bundlerEnv {
    inherit (pkgs) ruby;
    name = "kontrakcja-ruby-env";
    gemset = ./gemset.nix;
    gemdir = ../../.;
  };

  aws-sam-cli = import ./aws-sam-cli { inherit nixpkgs; };

  run-deps = [ rubyEnv aws-sam-cli ] ++
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
      imagemagick
      glibcLocales
      poppler_utils
    ]);
in
run-deps

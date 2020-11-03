let
  nixpkgs = import <nixpkgs> {};

  inherit (nixpkgs) pkgs stdenv nodePackages;

  src = ../.;

  gems = pkgs.bundlerEnv {
    name = "scrive-api-docs";
    gemdir = src;
    gemset = ./gemset.nix;
  };

  openapi2slate = (import ./openapi2slate {
    inherit pkgs;
  }).openapi2slate;
in
stdenv.mkDerivation {
  name = "scrive-api-docs";

  inherit src;

  buildInputs = [
    gems
    pkgs.nodejs
    openapi2slate
  ];

  buildPhase = ''
    openapi2slate documentation/scrive_api.yaml > source/index.html.md
    middleman build --clean
  '';

  installPhase = ''
    mv build $out
  '';
}

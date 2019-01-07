{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;

in
stdenv.mkDerivation {
  name = "sigshield";
  src = builtins.filterSource
    (path: type: baseNameOf path == "sigshield.c") ./.;

  buildInputs = [pkgs.gcc];

  buildPhase = "gcc sigshield.c -o sigshield";

  installPhase = ''
    mkdir -p $out/bin
    cp sigshield $out/bin/sigshield
  '';
}

{
  nixpkgs
, haskellPackages
, nixpkgs-src
, kontrakcja-nix-src
, localeLang ? "en_US.UTF-8"
, workspaceRoot ? builtins.toPath(../..)
}:
let
  inherit (nixpkgs) pkgs;

  sourceRoot = builtins.toPath(../..);

  release = import ./kontrakcja-production-release.nix {
    inherit nixpkgs haskellPackages nixpkgs-src;
  };

  run-deps = import (kontrakcja-nix-src + /packages/run-deps.nix)
    { inherit nixpkgs haskellPackages; };

  inherit (release) kontrakcja kontrakcja-frontend;

  packages = run-deps ++ [
    kontrakcja kontrakcja-frontend
  ];
in
pkgs.stdenv.mkDerivation {
  name = "kontrakcja-production-shell";

  KONTRAKCJA_ROOT = sourceRoot;
  KONTRAKCJA_WORKSPACE = workspaceRoot;

  buildInputs = packages;

  shellHook = ''
    export LANG=${localeLang}
  '';
}

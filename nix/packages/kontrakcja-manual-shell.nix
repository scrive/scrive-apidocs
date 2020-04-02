{
  nixpkgs
, haskellPackages
, kontrakcja-nix-src
, localeLang ? "en_US.UTF-8"
, workspaceRoot ? builtins.toPath(../..)
}:
let
  inherit (nixpkgs) pkgs;

  sourceRoot = builtins.toPath(../..);

  run-deps = import (kontrakcja-nix-src + /packages/run-deps.nix)
    { inherit nixpkgs haskellPackages; };

  ghc = haskellPackages.ghcWithPackages
    (pkg: []);
in
pkgs.mkShell {
  name = "kontrakcja-manual-shell";

  LD_LIBRARY_PATH = "${pkgs.zlib}/lib:${pkgs.icu}/lib:${pkgs.curl.out}/lib:${pkgs.libxml2.out}/lib";
  C_INCLUDE_PATH = "${pkgs.libxml2.dev}/include/libxml2";

  KONTRAKCJA_ROOT = sourceRoot;
  KONTRAKCJA_WORKSPACE = workspaceRoot;

  buildInputs = [
    ghc
  ] ++ run-deps;

  shellHook = ''
    export LANG=${localeLang}
  '';
}

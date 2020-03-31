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

  elm2nix = import ./elm2nix.nix { inherit nixpkgs; };
in
pkgs.mkShell {
  name = "kontrakcja-manual-shell";

  LD_LIBRARY_PATH = "${pkgs.zlib}/lib:${pkgs.icu}/lib:${pkgs.curl.out}/lib";

  KONTRAKCJA_ROOT = sourceRoot;
  KONTRAKCJA_WORKSPACE = workspaceRoot;

  buildInputs = [
    ghc
  ] ++ run-deps;

  shellHook = ''
    export LANG=${localeLang}
  '';
}

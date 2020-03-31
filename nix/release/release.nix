{
  release-path
, nixpkgs ? import ./nixpkgs.nix
, localeLang ? "en_US.UTF-8"
, workspaceRoot ? builtins.toPath(../..)
}:
let
  kontrakcja-nix-src = import ../source/kontrakcja-nix.nix;

  kontrakcja-nix = import (kontrakcja-nix-src + release-path)
    { inherit nixpkgs; };

  inherit (kontrakcja-nix) haskellPackages prodHaskellPackages;

  kontrakcja-src = import ../packages/kontrakcja-src.nix;

  kontrakcja-base = import ../packages/kontrakcja-base.nix {
    inherit nixpkgs haskellPackages;
  };

  manual-shell = import ../packages/kontrakcja-manual-shell.nix {
    inherit nixpkgs kontrakcja-nix-src
      workspaceRoot localeLang haskellPackages;
  };

  dev-shell = import ../packages/kontrakcja-dev-shell.nix {
    inherit nixpkgs kontrakcja-nix-src workspaceRoot localeLang;
    haskellPackages = haskellPackages;
  };

  dev-shell-optimized = import ../packages/kontrakcja-dev-shell.nix {
    inherit nixpkgs kontrakcja-nix-src workspaceRoot localeLang;
    haskellPackages = prodHaskellPackages;
  };

  dev-release = import ../packages/kontrakcja-dev-release.nix {
    inherit nixpkgs haskellPackages;
  };

  production-shell = import ../packages/kontrakcja-production-shell.nix {
    inherit nixpkgs kontrakcja-nix-src
      workspaceRoot localeLang;

    haskellPackages = prodHaskellPackages;
  };

  production-release = import ../packages/kontrakcja-production-release.nix {
    inherit nixpkgs;
    haskellPackages = prodHaskellPackages;
  };

  dev-deps = nixpkgs.stdenv.mkDerivation {
    name = "kontrakcja-dev-deps";
    src=kontrakcja-src;
    propagatedBuildInputs = dev-shell.buildInputs;
    configurePhase = "echo configure phase..; env";

    buildPhase = ''
      mkdir -p $out
      echo copying source: ${kontrakcja-src}
      cp -r "${kontrakcja-src}"/* $out/
    '';
    testPhase = "";
    installPhase = "echo install phase";
  };
in
{
  inherit
    haskellPackages
    kontrakcja-base
    kontrakcja-src
    manual-shell
    dev-shell
    dev-shell-optimized
    dev-release
    dev-deps
    production-shell
    production-release;
}

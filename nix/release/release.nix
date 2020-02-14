{
  nixpkgs ? import ./nixpkgs.nix
, ghcVersion
, inHaskellPackages ? nixpkgs.pkgs.haskell.packages.${ghcVersion}
, workspaceRoot ? builtins.toPath(../..)
, localeLang ? "en_US.UTF-8"
}:
let
  haskellPackages = import ../derivation/kontrakcja-build-deps.nix {
    inherit nixpkgs inHaskellPackages;
  };

  prodHaskellPackages = import ../derivation/kontrakcja-build-deps.nix {
    inherit nixpkgs inHaskellPackages;
    quickBuild = false;
  };

  kontrakcja-src = import ../derivation/kontrakcja-src.nix;

  kontrakcja-base = import ../derivation/kontrakcja-base.nix {
    inherit nixpkgs haskellPackages;
  };

  manual-shell = import ../derivation/kontrakcja-manual-shell.nix {
    inherit nixpkgs workspaceRoot localeLang;
    haskellPackages = inHaskellPackages;
  };

  cabal-shell = import ../derivation/kontrakcja-cabal-shell.nix {
    inherit nixpkgs haskellPackages workspaceRoot localeLang;
  };

  dev-shell = import ../derivation/kontrakcja-dev-shell.nix {
    inherit nixpkgs haskellPackages workspaceRoot localeLang;
  };

  dev-release = import ../derivation/kontrakcja-dev-release.nix {
    inherit nixpkgs haskellPackages;
  };

  production-shell = import ../derivation/kontrakcja-production-shell.nix {
    inherit nixpkgs workspaceRoot localeLang;

    haskellPackages = prodHaskellPackages;
  };

  production-release = import ../derivation/kontrakcja-production-release.nix {
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
    cabal-shell
    dev-shell
    dev-release
    dev-deps
    production-shell
    production-release;
}

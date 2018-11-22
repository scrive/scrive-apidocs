{ nixpkgs ? import <nixpkgs> {}
, node-version ? "10_x" 
, kontrakcja
}:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;

  # Make NPM work with HOME=/homeless-shelter.
  npmOptions = "--cache ./.npm --userconfig ./.npmrc";

  # Some files to exclude when the repository is copied to the Nix store.
  excluded = [
    "backend" "dist" "dist-newstyle" "scrivepdftools" ".git" "node_modules"
    "shell.nix" "release.nix" "nix" "test_data" "test_s3files"
  ];

in
stdenv.mkDerivation {
  name = "kontrakcja-frontend";
  src = builtins.filterSource
    (path: type: ! builtins.elem (baseNameOf path) excluded)
    ./..; # It relies on ../texts as well

  buildInputs = [
    pkgs."nodejs-${node-version}"
    pkgs.glibcLocales
  ];

  configurePhase = ''
    base="$PWD"
    export LOCALIZATION_BIN="${kontrakcja}/bin/localization"
    export LANG=sv_SE.UTF-8
    cd "$base/frontend"
    npm install ${npmOptions}
  '';

  buildPhase = ''
    cd "$base/frontend"
    npm run build:nix ${npmOptions}
  '';

  installPhase = ''
    cp -r "$base/frontend/dist" $out
  '';
}

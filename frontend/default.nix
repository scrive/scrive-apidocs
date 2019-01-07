{ nixpkgs ? import <nixpkgs> {}
, nodeVersion ? "10_x"
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
    "shell.nix" "release.nix" "nix" "test_data" "test_s3files" "_nix_local"
  ];

in
stdenv.mkDerivation {
  name = "kontrakcja-frontend";
  src = builtins.filterSource
    (path: type: ! builtins.elem (baseNameOf path) excluded)
    ./..; # It relies on ../texts as well

  buildInputs = [
    pkgs."nodejs-${nodeVersion}"
    pkgs.glibcLocales
  ];

  configurePhase = ''
    base="$PWD"
    export LOCALIZATION_BIN="${kontrakcja}/bin/localization"
    export LANG=sv_SE.UTF-8
    export PHANTOMJS_BIN="${pkgs.phantomjs}/bin/phantomjs"
    cd "$base/frontend"
    npm install ${npmOptions}
  '';

  buildPhase = ''
    cd "$base/frontend"
    npm run build:nix ${npmOptions}
  '';

  testPhase = ''
    cd "$base/frontend"
    npm run test ${npmOptions}
  '';

  installPhase = ''
    cp -r "$base/frontend/dist" $out
  '';
}

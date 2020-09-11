let
  nixpkgs = import ../modules/nixpkgs.nix;

  haskell-nix = import ../modules/haskell-nix.nix;

  kontrakcja-src = import ../source/kontrakcja.nix
    { inherit haskell-nix; };

  pdftools-src = import ../source/pdftools.nix;

  dummy-main = builtins.toFile "Main.hs" ''
    main :: IO ()
    main = return ()
    '';
in
nixpkgs.stdenv.mkDerivation {
  name = "kontrakcja-nix-deps";

  unpackPhase = "true";

  buildPhase = ''
    mkdir -p ./Shake ./frontend ./frontend-elm

    cp -r \
      ${kontrakcja-src}/nix \
      ${kontrakcja-src}/shell.nix \
      ${kontrakcja-src}/release.nix \
      ${kontrakcja-src}/cabal.project \
      ${kontrakcja-src}/cabal.project.freeze \
      ./

    cp -r \
      ${kontrakcja-src}/frontend/package.json \
      ${kontrakcja-src}/frontend/package-lock.json \
      ${kontrakcja-src}/frontend/default.nix \
      ${kontrakcja-src}/frontend/node-env.nix \
      ${kontrakcja-src}/frontend/node-packages.nix \
      ./frontend/

    cp -r \
      ${kontrakcja-src}/frontend-elm/package.json \
      ${kontrakcja-src}/frontend-elm/package-lock.json \
      ${kontrakcja-src}/frontend-elm/default.nix \
      ${kontrakcja-src}/frontend-elm/node-env.nix \
      ${kontrakcja-src}/frontend-elm/node-packages.nix \
      ${kontrakcja-src}/frontend-elm/elm.nix \
      ${kontrakcja-src}/frontend-elm/elm-srcs.nix \
      ${kontrakcja-src}/frontend-elm/registry.dat \
      ./frontend-elm/

    mkdir -p \
      ./localization/src \
      ./scripts/sort_imports \
      ./scripts/detect_old_templates \
      ./scripts/detect_old_localizations

    cp ${dummy-main} ./Shake/Shake.hs
    cp ${dummy-main} ./localization/src/LocalizationMain.hs
    cp ${dummy-main} ./scripts/sort_imports/SortImportsMain.hs
    cp ${dummy-main} ./scripts/detect_old_templates/DetectOldTemplatesMain.hs
    cp ${dummy-main} ./scripts/detect_old_localizations/DetectOldLocalizationsMain.hs

    find . -type d -exec chmod 755 {} \;
    find . -type f -exec chmod 644 {} \;

    mkdir -p ./nix/source/pdftools
    echo "./pdftools/." > ./nix/source/pdftools.nix

    cat << EOF > ./nix/source/kontrakcja.nix
    { haskell-nix }:
    builtins.path {
      path = ../..;
      name = "kontrakcja-nix-src";
    }
    EOF

    cp \
      ${pdftools-src}/pom.xml \
      ${pdftools-src}/release.nix \
      ${pdftools-src}/project-info.json \
      ./nix/source/pdftools/

    pattern="s|Exposed-Modules|_Exposed-Modules|Ig ; s|Other-Modules|_Other-Modules|Ig ; s|base-noprelude|base, base-noprelude|Ig"

    sed -e "$pattern" ${kontrakcja-src}/kontrakcja.cabal > ./kontrakcja.cabal
    sed -e "$pattern" ${kontrakcja-src}/Shake/kontrakcja-shake.cabal > ./Shake/kontrakcja-shake.cabal

    find . -type d -exec chmod 755 {} \;
    find . -type f -exec chmod 644 {} \;
  '';

  # Build as an archive, because there are some weird race condition bugs when running
  # Nix expressions stored in /nix/store.
  installPhase = ''
    mkdir -p $out
    tar czf "$out/kontrakcja-nix.tar.gz" .
  '';
}

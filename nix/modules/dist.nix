{ nixpkgs
, kontrakcja-src
, scrivepdftools
, kontrakcja-project
, kontrakcja-frontend
, scrive-new-frontend
}:
let
  inherit (nixpkgs) pkgs lib stdenv;

  components =
    kontrakcja-project.kontrakcja.components.exes //
    kontrakcja-project.kontrakcja.components.tests;

  runDeps = [
    pkgs.zbar
    pkgs.curl
    pkgs.mupdf
    pkgs.lessc
    pkgs.xmlsec
    pkgs.openjdk
    pkgs.gnuplot
    pkgs.qrencode
    pkgs.pngquant
    pkgs.postgresql
    pkgs.imagemagick
    pkgs.glibcLocales
    pkgs.poppler_utils
  ];

  runDepsBin = lib.strings.makeBinPath runDeps;

  bins = [
    "cron"
    "config-checker"
    "detect_old_localizations"
    "detect_old_templates"
    "kontrakcja-migrate"
    "kontrakcja-server"
    "localization"
    "mailing-server"
    "messenger-server"
    "routinglist"
    "sort_imports"
    "transifex"
    "kontrakcja-test"
  ];

  wrapBin = bin : ''
    cat <<EOF > $out/bin/${bin}
    #!/usr/bin/env bash

    export KONTRAKCJA_ROOT=$out
    export KONTRAKCJA_WORKSPACE=''\\''${KONTRAKCJA_WORKSPACE:-''\\$(pwd)}
    export PATH=${runDepsBin}:''\\$PATH
    export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive

    ${components.${bin}}/bin/${bin} "''\\$@"
    EOF
    '';

  wrapBins = lib.strings.concatMapStrings wrapBin bins;

  dirsToCopy =
    [
      "build-scripts"
      "certs"
      "templates"
      "configuration-templates"
      "evidence-package"
      "files"
      "GuardTime"
      "pure_sql"
      "reporting"
      "scrivepdftools"
      "texts"
      "vagrant"
      "backend/test/csv"
      "backend/test/e-legitimation"
      "backend/test/json"
      "backend/test/pdfs"
      "backend/test/sso"
      "backend/test/screenshots"
      "backend/test/xml"
    ];

  copyDirs = lib.strings.concatMapStrings
    (dir: "cp -r \"${kontrakcja-src}/${dir}\" \"$out/${dir}\" \n")
    dirsToCopy
  ;
in
stdenv.mkDerivation {
  name = "kontrakcja-dist";

  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out/bin $out/backend/test $out/new-frontend

    ${copyDirs}
    cp -r ${kontrakcja-frontend} $out/frontend
    cp -r ${scrive-new-frontend} $out/new-frontend/dist

    ls -la $out/scrivepdftools
    chmod 755 $out/scrivepdftools
    chmod 644 $out/scrivepdftools/scrivepdftools.jar
    rm -f $out/scrivepdftools/newscrivepdftools.jar
    cp ${scrivepdftools}/scrivepdftools.jar $out/scrivepdftools/newscrivepdftools.jar

    ${wrapBins}

    chmod +x $out/bin/*
  '';
}

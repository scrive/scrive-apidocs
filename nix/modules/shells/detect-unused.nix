{ nixpkgs
, kontrakcja-project
}:
let
  detect-old-templates = kontrakcja-project.kontrakcja.components.exes.detect_old_templates;
  detect-old-localizations = kontrakcja-project.kontrakcja.components.exes.detect_old_localizations;
in
nixpkgs.mkShell {
  buildInputs = [
    nixpkgs.glibcLocales
    detect-old-templates
    detect-old-localizations
  ];

  exactDeps = true;

  withHoogle = false;

  SKIP_CABAL_UPDATE = 1;

  shellHook = ''
    export LANG=''${LANG:-en_US.UTF-8}
    '';
}

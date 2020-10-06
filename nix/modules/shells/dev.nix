{ nixpkgs
, run-deps
, ghc-version
, cabal-install
, scrivepdftools
, haskell-deps
, kontrakcja-project
, scrive-new-frontend
}:
let
  inherit (haskell-deps.exes)
    hlint brittany apply-refact
  ;

  KONTRAKCJA_ROOT = builtins.toPath ../../..;

  deps = [
    hlint
    brittany
    apply-refact
    cabal-install
  ] ++ run-deps;

  deps-path = nixpkgs.lib.strings.concatMapStrings
    (pkg: if pkg == null then "" else "${pkg}/bin:")
    deps
  ;

  link-frontend = "( cd \$KONTRAKCJA_ROOT && ./new-frontend/link-new-frontend.sh )";
in
kontrakcja-project.shellFor {
  inherit KONTRAKCJA_ROOT scrivepdftools;

  scrive_new_frontend = scrive-new-frontend;

  packages = hsPkgs: [
    hsPkgs.kontrakcja
    hsPkgs.kontrakcja-shake
  ];

  exactDeps = true;

  withHoogle = false;

  SKIP_CABAL_UPDATE = 1;

  shellHook = ''
    export LANG=''${LANG:-en_US.UTF-8}
    export PATH=${deps-path}$PATH
    ${link-frontend}
    '';
}

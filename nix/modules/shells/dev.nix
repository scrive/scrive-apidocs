{ nixpkgs
, run-deps
, ghc-version
, cabal-install
, scrivepdftools
, haskell-deps
, kontrakcja-project
}:
let
  inherit (haskell-deps.exes)
    hlint brittany apply-refact
  ;

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
in
kontrakcja-project.shellFor {
  inherit scrivepdftools;

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
    '';
}

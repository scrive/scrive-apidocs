{ nixpkgs
, haskell-nix
, haskell-deps
, cabal-install
, kontrakcja-project
}:
let
  inherit (nixpkgs) git nodejs nodePackages glibcLocales;

  inherit (haskell-deps.exes)
    hlint brittany apply-refact;

  sort-imports = kontrakcja-project.kontrakcja.components.exes.sort_imports;
  kontrakcja-shake = kontrakcja-project.kontrakcja-shake.components.exes.kontrakcja-shake;
in
kontrakcja-project.shellFor {
  packages = hsPkgs: [ ];

  buildInputs = [
    git
    hlint
    nodejs
    brittany
    apply-refact
    sort-imports
    glibcLocales
    cabal-install
    kontrakcja-shake
    nodePackages.less
    nodePackages.grunt-cli
  ];

  withHoogle = false;

  shellHook = ''
    export LANG=''${LANG:-en_US.UTF-8}
    '';
}

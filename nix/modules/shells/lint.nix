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

  ghc = haskell-nix.pkgs.haskell-nix.haskellPackages.ghcWithPackages (ps: []);

  sort-imports = kontrakcja-project.kontrakcja.components.exes.sort_imports;
  kontrakcja-shake = kontrakcja-project.kontrakcja-shake.components.exes.kontrakcja-shake;
in
nixpkgs.mkShell {
  buildInputs = [
    git
    ghc
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

  shellHook = ''
    export LANG=''${LANG:-en_US.UTF-8}
    '';
}

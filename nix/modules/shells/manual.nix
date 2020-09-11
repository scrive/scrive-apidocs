{ nixpkgs
, ghc-version
, run-deps
, haskell-deps
}:
let
  inherit (nixpkgs) zlib icu curl libxml2;

  inherit (haskell-deps.exes) cabal-install
    hlint brittany apply-refact;

  ghc = nixpkgs.haskell.packages.${ghc-version}.ghc;
in
nixpkgs.mkShell {
  name = "kontrakcja-manual-shell";

  LD_LIBRARY_PATH = "${zlib}/lib:${icu}/lib:${curl.out}/lib:${libxml2.out}/lib";

  buildInputs = [
    ghc
    hlint
    brittany
    apply-refact
    cabal-install
  ] ++ run-deps;

  shellHook = ''
    export LANG=''${LANG:-en_US.UTF-8}
    '';
}

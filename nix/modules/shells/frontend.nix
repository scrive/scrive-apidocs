{ nixpkgs
, haskell-deps
, cabal-install
, kontrakcja-project
}:
let
  inherit (haskell-deps.exes) cabal-install;
  inherit (nixpkgs) pkgs nodePackages elmPackages;

  localization = kontrakcja-project.kontrakcja.components.exes.localization;

  # On Mac browsers have to be installed outside of Nix
  browserDeps =
    if builtins.currentSystem == "x86_64-darwin"
    then []
    else [ pkgs.chromium ]
  ;

  run-deps = [
    pkgs.sass
    pkgs.nodejs
    pkgs.glibcLocales
    elmPackages.elm
    nodePackages.less
    nodePackages.grunt-cli
  ] ++ browserDeps;
in
nixpkgs.mkShell {
  CHROME_BIN="${pkgs.chromium}/bin/chromium";
  LOCALIZATION_BIN = "${localization}/bin/localization";

  buildInputs = [
    localization
    cabal-install
  ] ++ run-deps
  ;

  shellHook = ''
    export LANG=''${LANG:-en_US.UTF-8}
    '';
}

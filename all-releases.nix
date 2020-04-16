{ useLocal ? false
, extra-run-deps ? pkgs: hsPkgs: []
}:
let
  localeLang =
    if builtins.pathExists ./.locale
    then builtins.readFile ./.locale
    else "en_US.UTF-8"
    ;

  workspaceRoot = builtins.toPath(./.);

  loadRelease = version:
    import (./nix/release + "/ghc-${version}.nix") {
      inherit useLocal localeLang extra-run-deps workspaceRoot;
    };

  ghc84 = loadRelease "8.4";
  ghc86 = loadRelease "8.6";
  ghc88 = loadRelease "8.8";
  ghc810 = loadRelease "8.10";
in
{ inherit ghc84 ghc86 ghc88 ghc810;
}

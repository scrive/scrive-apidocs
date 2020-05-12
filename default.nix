{ useLocal ? false
, extra-run-deps ? pkgs: hsPkgs: []
}:
let
  localeLang =
    if builtins.pathExists ./.locale
    then builtins.readFile ./.locale
    else "en_US.UTF-8"
    ;
in
import ./nix/release/ghc-8.8.nix {
  inherit useLocal localeLang extra-run-deps;
  workspaceRoot = builtins.toPath(./.);
}

let
  localeLang =
    if builtins.pathExists ./.locale
    then builtins.readFile ./.locale
    else "en_US.UTF-8"
    ;
in
import ../../nix/release/ghc-8.10.nix {
  inherit localeLang;
  workspaceRoot = builtins.toPath(./.);
}

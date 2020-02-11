let
  localeLang =
    if builtins.pathExists ./.locale
    then builtins.readFile ./.locale
    else "en_US.UTF-8"
    ;
in
import ../../nix/release/ghc-8.4.4.nix {
  inherit localeLang;
  workspaceRoot = builtins.toPath(./.);
}

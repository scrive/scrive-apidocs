let
  release = import ./release.nix;
in
release.dev-shell

{ localeLang ? "C.UTF-8" }:
let
  release = import ./release.nix {
    inherit localeLang;
  };
in
release.dev-shell
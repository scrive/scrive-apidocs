{ useLocal ? false
, extra-run-deps ? pkgs: hsPkgs: []
}:
let
  release = import ./release.nix
    { inherit useLocal extra-run-deps; };
in
release.dev-shell-optimized

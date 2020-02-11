{ nixpkgs }:
let
  src = builtins.fetchGit {
    url = "https://github.com/cachix/elm2nix.git";
    rev = "47617ce9131831221e6cfb47cf6488554f7f2f3f";
  };
in
import src {
  pkgs = nixpkgs;
}

{ nixpkgs }:
let
  version = "5d40c8779952f6112efca4b56e66b6f69d3d3468";
  sha256 = "0pmj7ldgjvsn6ykk0539yp6zc6appsb8bb5vc1r038m1fr3b8vgj";

  src = builtins.fetchTarball
    { inherit sha256;
      url = "https://github.com/cachix/elm2nix/archive/${version}.tar.gz";
    };
in
import src {
  pkgs = nixpkgs;
}

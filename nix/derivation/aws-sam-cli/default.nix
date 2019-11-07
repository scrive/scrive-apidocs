{ nixpkgs }:
let
  python = nixpkgs.python37Packages;
  pythonDeps = import ./requirements.nix { pkgs = nixpkgs; };
in
pythonDeps.packages.aws-sam-cli

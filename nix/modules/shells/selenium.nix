{ nixpkgs }:
let
  inherit (nixpkgs) pkgs;

  pyPkgs = pkgs.python3Packages;

  pythonEnv = pkgs.python3.buildEnv.override {
    extraLibs = [
      pyPkgs.nose
      pyPkgs.pillow
      pyPkgs.enum34
      pyPkgs.selenium
      pyPkgs.requests
      pyPkgs.pyquery
      pyPkgs.setuptools
      pyPkgs.python-dateutil
    ];

    ignoreCollisions = true;
  };

  # On Mac browsers have to be installed outside of Nix
  browserDeps =
    if builtins.currentSystem == "x86_64-darwin"
    then []
    else [ pkgs.firefox ]
  ;
in
nixpkgs.mkShell {
  buildInputs = [
    pythonEnv
    pkgs.xvfb_run
    pkgs.geckodriver
    pkgs.poppler_utils
    pkgs.python2Packages.supervisor
    pkgs.python2Packages.pip
    pkgs.selenium-server-standalone
  ] ++ browserDeps;
}

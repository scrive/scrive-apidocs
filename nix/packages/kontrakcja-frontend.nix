{ nixpkgs
, nodeVersion ? "12_x"
, kontrakcja
, kontrakcja-src
, nixpkgs-src
}:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;

  nodejs = pkgs."nodejs-${nodeVersion}";

  elmPkgs = import ../../frontend-elm/elm.nix {
    nixpkgs = nixpkgs-src;
  };

  nodePackages = import ../../frontend/default.nix {
    pkgs = nixpkgs;
    inherit nodejs;
  };

  elmNodePackages = import ../../frontend-elm/default.nix {
    pkgs = nixpkgs;
    inherit nodejs;
  };

  inherit (nodePackages.shell) nodeDependencies;

  elmNodeDependencies = elmNodePackages.shell.nodeDependencies ;
in
stdenv.mkDerivation {
  name = "kontrakcja-frontend";
  buildInputs = [
    nodejs
    pkgs.glibcLocales
    pkgs.nodePackages.less
    pkgs.nodePackages.grunt-cli
  ] ++ elmPkgs.buildInputs;

  src = kontrakcja-src;

  inherit nodeDependencies;

  configurePhase = ''
    export LOCALIZATION_BIN="${kontrakcja}/bin/localization"
    export LANG=en_US.UTF-8

    ln -s ${nodeDependencies}/lib/node_modules frontend/node_modules
    ln -s ${elmNodeDependencies}/lib/node_modules frontend-elm/node_modules

    export KONTRAKCJA_ROOT=$PWD
    export KONTRAKCJA_WORKSPACE=$PWD

    echo "building kontrakcja frontend"
    echo "$PWD"
    ls -la $PWD
    ls -la $PWD/frontend
    ls -la $PWD/frontend-elm

    cd frontend
  '';

  buildPhase = elmPkgs.buildPhase;

  testPhase = ''
  '';

  installPhase = ''
    npm run build:nix
    echo "copying dist:"
    ls -la dist
    cp -r dist $out
  '';
}

{ nixpkgs
, nodeVersion ? "10_x"
, kontrakcja
, kontrakcja-src
}:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;

  nodejs = pkgs."nodejs-${nodeVersion}";
  nodePackages = import ../../frontend/default.nix {
    pkgs = nixpkgs;
    inherit nodejs;
  };
  inherit (nodePackages.shell) nodeDependencies;
in
stdenv.mkDerivation {
  name = "kontrakcja-frontend";
  buildInputs = [
    nodejs
    pkgs.glibcLocales
    pkgs.nodePackages.less
    pkgs.nodePackages.grunt-cli
  ];

  src = kontrakcja-src;

  inherit nodeDependencies kontrakcja;

  configurePhase = ''
    export LOCALIZATION_BIN="${kontrakcja}/bin/localization"
    export LANG=en_US.UTF-8

    export NODE_PATH=${nodeDependencies}/lib/node_modules
    export PATH="${nodeDependencies}/bin:$PATH"

    ln -s ${nodeDependencies}/lib/node_modules frontend/node_modules

    export KONTRAKCJA_ROOT=$PWD
    export KONTRAKCJA_WORKSPACE=$PWD

    echo "building kontrakcja frontend"
    env
    ls -la $PWD
    ls -la $PWD/frontend

    cd frontend
  '';

  buildPhase = ''
    npm run build:nix
  '';

  testPhase = ''
    npm run test
  '';

  installPhase = ''
    echo "copying dist:"
    ls -la dist
    cp -r dist $out
  '';
}

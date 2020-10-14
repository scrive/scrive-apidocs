{ nixpkgs
, nodeVersion ? "12_x"
, nixpkgs-src
, kontrakcja-src
, kontrakcja-project
}:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;

  localization = kontrakcja-project.kontrakcja.components.exes.localization;

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

  frontend-src = stdenv.mkDerivation {
    name = "kontrakcja-frontend-src";
    src = kontrakcja-src;

    configurePhase = "true";
    buildPhase = "true";
    testPhase = "true";

    installPhase = ''
      mkdir -p $out
      cp -r frontend frontend-elm texts templates $out/
      mkdir -p $out/backend/flow
      cp -r backend/flow/docs $out/backend/flow/
    '';
  };
in
stdenv.mkDerivation {
  name = "kontrakcja-frontend";
  buildInputs = [
    nodejs
    pkgs.sass
    pkgs.glibcLocales
    pkgs.nodePackages.less
    pkgs.nodePackages.grunt-cli
  ] ++ elmPkgs.buildInputs;

  src = frontend-src;

  inherit nodeDependencies;

  configurePhase = ''
    export LOCALIZATION_BIN="${localization}/bin/localization"
    export LANG=en_US.UTF-8

    ln -s ${nodeDependencies}/lib/node_modules frontend/node_modules
    ln -s ${elmNodeDependencies}/lib/node_modules frontend-elm/node_modules

    export KONTRAKCJA_ROOT=$PWD
    export KONTRAKCJA_WORKSPACE=$PWD

    echo "building kontrakcja frontend"
    echo "$PWD"
    ls -la
    ls -la frontend
    ls -la frontend-elm

    cd frontend
  '';

  buildPhase = elmPkgs.buildPhase;
  testPhase = "true";

  installPhase = ''
    npm run build:nix
    echo "copying dist:"
    ls -la dist

    mkdir -p $out
    cp -r dist $out/
    cp -r app $out/
  '';
}

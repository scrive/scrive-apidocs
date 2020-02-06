{ nixpkgs
, nodeVersion ? "13_x"
, kontrakcja-src
}:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;

  nodejs = pkgs."nodejs-${nodeVersion}";

  elmPkgs = import ../../frontend-elm/elm.nix {
    inherit nixpkgs;
  };

  elmNodePackages = import ../../frontend-elm/default.nix {
    pkgs = nixpkgs;
    inherit nodejs;
  };

  elmNodeDependencies = elmNodePackages.shell.nodeDependencies ;
in
stdenv.mkDerivation {
  name = "kontrakcja-frontend-elm";
  buildInputs = [
    nodejs
    pkgs.glibcLocales
  ] ++ elmPkgs.buildInputs;

  src = kontrakcja-src;

  configurePhase = ''
    export LANG=C.UTF-8

    ln -s ${elmNodeDependencies}/lib/node_modules frontend-elm/node_modules

    echo "building kontrakcja frontend-elm"
    echo "$PWD"
    ls -la $PWD
    ls -la $PWD/frontend-elm

    cd frontend-elm
  '';

  buildPhase = elmPkgs.buildPhase;

  testPhase = ''
  '';

  installPhase = ''
    npm run build
    echo "copying dist:"
    ls -la dist
    cp -r dist $out
  '';
}

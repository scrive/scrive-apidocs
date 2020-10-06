{ nixpkgs }:
let
  inherit (nixpkgs) pkgs stdenv nodejs;

  frontend-src = import ../source/new-frontend.nix;
  flow-frontend-src = import ../source/flow-frontend.nix;

  nodePackages = import ../node-deps/new-frontend/default.nix {
    inherit nodejs;
    pkgs = nixpkgs;
  };

  inherit (nodePackages.shell) nodeDependencies;
in
stdenv.mkDerivation {
  name = "scrive-new-frontend";
  src = frontend-src;
  buildInputs = [ nodejs ];
  buildPhase = ''
    cp -r ${nodeDependencies}/lib/node_modules ./
    export PATH="${nodeDependencies}/bin:$PATH"

    ls -la node_modules/

    npm run prod-build
  '';

  installPhase = ''
    cp -r dist $out/
  '';
}

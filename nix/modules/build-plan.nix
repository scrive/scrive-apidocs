{ nixpkgs
, name
, project
}:
let
  build-plan = project.plan-nix;
in
nixpkgs.stdenv.mkDerivation {
  inherit name;

  unpackPhase = "true";

  buildInputs = [ nixpkgs.nix ];

  installPhase = ''
    mkdir -p $out

    cp -r ${build-plan} $out/plan

    nix-hash --base32 --type sha256 $out/plan/ > $out/plan-hash.txt
  '';
}

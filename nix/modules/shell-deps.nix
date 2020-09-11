{ nixpkgs
, shells
}:
let
  buildInputs = nixpkgs.lib.concatMap
    (shell:
      shell.buildInputs ++
      shell.nativeBuildInputs ++
      shell.propagatedBuildInputs ++
      shell.propagatedNativeBuildInputs
    )
    shells;

  shell-deps = nixpkgs.stdenv.mkDerivation {
    name = "kontrakcja-shell-deps";
    unpackPhase = "true";

    propagatedBuildInputs = buildInputs;

    configurePhase = "true";
    buildPhase = ''
      mkdir -p $out
    '';

    testPhase = "true";
    installPhase = "true";
  };
in
shell-deps

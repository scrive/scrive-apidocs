{ nixpkgs
, cabal-install
, cabal-project
}:
let
  inherit (nixpkgs) pkgs;
in
pkgs.stdenv.mkDerivation {
  name = "wrap-cabal-install";

  unpackPhase = "true";

  propagatedBuildInputs = [ cabal-install ];

  buildPhase = ''
    mkdir -p $out/bin

    cat > $out/bin/cabal << 'EOF'
    #!/usr/bin/env bash

    set -ea

    if ( [ "$1" == "update" ] || [ "$1" == "v2-update" ] )
    then
      if [ "$SKIP_CABAL_UPDATE" == 1 ]
      then
        echo "Skipping cabal update. Unset \$SKIP_CABAL_UPDATE to force update"
      else
        set -x
        ${cabal-install}/bin/cabal "$@"
      fi
    else
      set -x
      ${cabal-install}/bin/cabal --project-file=${cabal-project} "$@"
    fi

    EOF

    chmod +x $out/bin/cabal

    ln -s ${cabal-install}/bin/cabal $out/bin/cabal-original
    '';

  installPhase = "true";
}

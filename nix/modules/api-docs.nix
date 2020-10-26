{ nixpkgs
, kontrakcja-src
}:
let

  api-docs-src = kontrakcja-src + "/api-docs";
in
import ../../api-docs {
  inherit nixpkgs;
  src = api-docs-src;
}

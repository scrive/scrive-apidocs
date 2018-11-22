{ mkDerivation, base, cond, exceptions, extra, fclabels, hpqtypes
, stdenv
}:
mkDerivation {
  pname = "kontrakcja-prelude";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [
    base cond exceptions extra fclabels hpqtypes
  ];
  description = "Alternative Prelude for kontrakcja";
  license = "unknown";
}

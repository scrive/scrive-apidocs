{ nixpkgs
, ghc-version
}:
let
  inherit (nixpkgs.lib) removeSuffix;

  plan-dir = ../plans + "/${ghc-version}";

  make-entry = name:
    # { plan = null;
    #   hash = null;
    # };
    { plan = plan-dir + "/${name}/plan";
      hash = removeSuffix "\n"
        (builtins.readFile (plan-dir + "/${name}/plan-hash.txt"));
    };
in
{
  kontrakcja = make-entry "kontrakcja";

  hlint = make-entry "hlint";

  brittany = make-entry "brittany";

  apply-refact = make-entry "apply-refact";

  cabal-install = make-entry "cabal-install";
}

{
  pkgs = hackage:
    {
      packages = {
        "ghc-lib-parser-ex".revision = (((hackage."ghc-lib-parser-ex")."8.8.5.8").revisions).default;
        "ghc-lib-parser-ex".flags.ghc-lib = false;
        "ghc".revision = (((hackage."ghc")."8.8.4").revisions).default;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "exceptions".flags.transformers-0-4 = true;
        "refact".revision = (((hackage."refact")."0.3.0.2").revisions).default;
        "binary".revision = (((hackage."binary")."0.8.7.0").revisions).default;
        "ghc-boot".revision = (((hackage."ghc-boot")."8.8.4").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "utf8-string".revision = (((hackage."utf8-string")."1.0.1.1").revisions).default;
        "polyparse".revision = (((hackage."polyparse")."1.13").revisions).default;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.7").revisions).default;
        "bifunctors".flags.semigroups = true;
        "bifunctors".flags.tagged = true;
        "extra".revision = (((hackage."extra")."1.7.7").revisions).default;
        "split".revision = (((hackage."split")."0.2.3.4").revisions).default;
        "data-fix".revision = (((hackage."data-fix")."0.3.0").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "base-compat-batteries".revision = (((hackage."base-compat-batteries")."0.11.1").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "ghc-heap".revision = (((hackage."ghc-heap")."8.8.4").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "cmdargs".revision = (((hackage."cmdargs")."0.10.20").revisions).default;
        "cmdargs".flags.testprog = false;
        "cmdargs".flags.quotation = true;
        "ghci".revision = (((hackage."ghci")."8.8.4").revisions).default;
        "clock".revision = (((hackage."clock")."0.8").revisions).default;
        "clock".flags.llvm = false;
        "syb".revision = (((hackage."syb")."0.7.1").revisions).default;
        "distributive".revision = (((hackage."distributive")."0.6.2").revisions).default;
        "distributive".flags.semigroups = true;
        "distributive".flags.tagged = true;
        "hscolour".revision = (((hackage."hscolour")."1.24.4").revisions).default;
        "scientific".revision = (((hackage."scientific")."0.3.6.2").revisions).default;
        "scientific".flags.integer-simple = false;
        "scientific".flags.bytestring-builder = false;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "random".revision = (((hackage."random")."1.2.0").revisions).default;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.3").revisions).default;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.1").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "dlist".revision = (((hackage."dlist")."1.0").revisions).default;
        "dlist".flags.werror = false;
        "conduit".revision = (((hackage."conduit")."1.3.2.1").revisions).default;
        "semigroups".revision = (((hackage."semigroups")."0.19.1").revisions).default;
        "semigroups".flags.bytestring = true;
        "semigroups".flags.unordered-containers = true;
        "semigroups".flags.text = true;
        "semigroups".flags.tagged = true;
        "semigroups".flags.containers = true;
        "semigroups".flags.binary = true;
        "semigroups".flags.hashable = true;
        "semigroups".flags.transformers = true;
        "semigroups".flags.deepseq = true;
        "semigroups".flags.bytestring-builder = false;
        "semigroups".flags.template-haskell = true;
        "data-default".revision = (((hackage."data-default")."0.7.1.1").revisions).default;
        "data-default-instances-old-locale".revision = (((hackage."data-default-instances-old-locale")."0.0.1").revisions).default;
        "uniplate".revision = (((hackage."uniplate")."1.6.12").revisions).default;
        "uniplate".flags.separate_syb = true;
        "uniplate".flags.typeable_fingerprint = true;
        "parsec".revision = (((hackage."parsec")."3.1.14.0").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "yaml".revision = (((hackage."yaml")."0.11.5.0").revisions).default;
        "yaml".flags.no-exe = true;
        "yaml".flags.no-examples = true;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.5").revisions).default;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.three = false;
        "template-haskell".revision = (((hackage."template-haskell")."2.15.0.0").revisions).default;
        "mono-traversable".revision = (((hackage."mono-traversable")."1.0.15.1").revisions).default;
        "vector".revision = (((hackage."vector")."0.12.1.2").revisions).default;
        "vector".flags.unsafechecks = false;
        "vector".flags.internalchecks = false;
        "vector".flags.wall = false;
        "vector".flags.boundschecks = true;
        "primitive".revision = (((hackage."primitive")."0.7.1.0").revisions).default;
        "base-compat".revision = (((hackage."base-compat")."0.11.1").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.3").revisions).default;
        "time-compat".flags.old-locale = false;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.11").revisions).default;
        "ansi-terminal".flags.example = false;
        "tagged".revision = (((hackage."tagged")."0.8.6").revisions).default;
        "tagged".flags.transformers = true;
        "tagged".flags.deepseq = true;
        "haskell-src-exts".revision = (((hackage."haskell-src-exts")."1.23.1").revisions).default;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.2.0.1").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.2.1").revisions).default;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "these".revision = (((hackage."these")."1.1.1.1").revisions).default;
        "these".flags.assoc = true;
        "bytestring".revision = (((hackage."bytestring")."0.10.10.1").revisions).default;
        "old-locale".revision = (((hackage."old-locale")."1.0.0.7").revisions).default;
        "data-default-instances-dlist".revision = (((hackage."data-default-instances-dlist")."0.0.1").revisions).default;
        "text".revision = (((hackage."text")."1.2.4.0").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."3.0.1.0").revisions).default;
        "assoc".revision = (((hackage."assoc")."1.0.2").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.12.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "base".revision = (((hackage."base")."4.13.0.0").revisions).default;
        "haskell-src-exts-util".revision = (((hackage."haskell-src-exts-util")."0.2.5").revisions).default;
        "comonad".revision = (((hackage."comonad")."5.0.6").revisions).default;
        "comonad".flags.distributive = true;
        "comonad".flags.test-doctests = true;
        "comonad".flags.containers = true;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "data-default-class".revision = (((hackage."data-default-class")."0.1.2.0").revisions).default;
        "terminfo".revision = (((hackage."terminfo")."0.4.1.4").revisions).default;
        "vector-algorithms".revision = (((hackage."vector-algorithms")."0.8.0.3").revisions).default;
        "vector-algorithms".flags.unsafechecks = false;
        "vector-algorithms".flags.internalchecks = false;
        "vector-algorithms".flags.llvm = false;
        "vector-algorithms".flags.boundschecks = true;
        "vector-algorithms".flags.bench = true;
        "vector-algorithms".flags.properties = true;
        "cpphs".revision = (((hackage."cpphs")."1.20.9.1").revisions).default;
        "cpphs".flags.old-locale = false;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.0.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.examples = false;
        "data-default-instances-containers".revision = (((hackage."data-default-instances-containers")."0.0.1").revisions).default;
        "attoparsec".revision = (((hackage."attoparsec")."0.13.2.4").revisions).default;
        "attoparsec".flags.developer = false;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "happy".revision = (((hackage."happy")."1.20.0").revisions).default;
        "file-embed".revision = (((hackage."file-embed")."0.0.13.0").revisions).default;
        "strict".revision = (((hackage."strict")."0.4").revisions).default;
        "strict".flags.assoc = true;
        "hpc".revision = (((hackage."hpc")."0.6.0.3").revisions).default;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "process".revision = (((hackage."process")."1.6.9.0").revisions).default;
        "filepattern".revision = (((hackage."filepattern")."0.1.2").revisions).default;
        "libyaml".revision = (((hackage."libyaml")."0.1.2").revisions).default;
        "libyaml".flags.system-libyaml = false;
        "libyaml".flags.no-unicode = false;
        "resourcet".revision = (((hackage."resourcet")."1.2.4.2").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "cabal-doctest".revision = (((hackage."cabal-doctest")."1.0.8").revisions).default;
        "aeson".revision = (((hackage."aeson")."1.5.4.0").revisions).default;
        "aeson".flags.cffi = false;
        "aeson".flags.fast = false;
        "aeson".flags.bytestring-builder = false;
        "aeson".flags.developer = false;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.8.4").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.2").revisions).default;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.3.2.0").revisions).default;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.8.4";
        nix-name = "ghc884";
        packages = {
          "ghc" = "8.8.4";
          "binary" = "0.8.7.0";
          "ghc-boot" = "8.8.4";
          "ghc-prim" = "0.5.3";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "ghc-heap" = "8.8.4";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "ghci" = "8.8.4";
          "deepseq" = "1.4.4.0";
          "parsec" = "3.1.14.0";
          "directory" = "1.3.6.0";
          "template-haskell" = "2.15.0.0";
          "containers" = "0.6.2.1";
          "bytestring" = "0.10.10.1";
          "text" = "1.2.4.0";
          "Cabal" = "3.0.1.0";
          "base" = "4.13.0.0";
          "time" = "1.9.3";
          "terminfo" = "0.4.1.4";
          "transformers" = "0.5.6.2";
          "hpc" = "0.6.0.3";
          "filepath" = "1.4.2.1";
          "process" = "1.6.9.0";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.8.4";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    { packages = { hlint = ./.plan.nix/hlint.nix; }; };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "hlint" = {
            flags = {
              "gpl" = lib.mkOverride 900 true;
              "threaded" = lib.mkOverride 900 true;
              "ghc-lib" = lib.mkOverride 900 false;
              };
            };
          };
        })
    ];
  }
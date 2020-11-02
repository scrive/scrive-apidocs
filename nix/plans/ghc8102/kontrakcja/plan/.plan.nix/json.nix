{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {
      split-base = true;
      parsec = true;
      pretty = true;
      generic = true;
      mapdict = false;
      };
    package = {
      specVersion = "1.6";
      identifier = { name = "json"; version = "0.10.0.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2007-2018 Galois Inc.";
      maintainer = "Iavor S. Diatchki (iavor.diatchki@gmail.com)";
      author = "Galois Inc.";
      homepage = "";
      url = "";
      synopsis = "Support for serialising Haskell to and from JSON";
      description = "JSON (JavaScript Object Notation) is a lightweight data-interchange\nformat. It is easy for humans to read and write. It is easy for\nmachines to parse and generate.  It is based on a subset of the\nJavaScript Programming Language, Standard ECMA-262 3rd Edition -\nDecember 1999.\n\nThis library provides a parser and pretty printer for converting\nbetween Haskell values and JSON.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "CHANGES"
        "tests/GenericTest.hs"
        "tests/HUnit.hs"
        "tests/Makefile"
        "tests/Parallel.hs"
        "tests/QC.hs"
        "tests/QuickCheckUtils.hs"
        "tests/Unit.hs"
        "tests/unit/fail1.json"
        "tests/unit/fail10.json"
        "tests/unit/fail11.json"
        "tests/unit/fail12.json"
        "tests/unit/fail13.json"
        "tests/unit/fail14.json"
        "tests/unit/fail15.json"
        "tests/unit/fail16.json"
        "tests/unit/fail17.json"
        "tests/unit/fail18.json"
        "tests/unit/fail19.json"
        "tests/unit/fail2.json"
        "tests/unit/fail20.json"
        "tests/unit/fail21.json"
        "tests/unit/fail22.json"
        "tests/unit/fail23.json"
        "tests/unit/fail24.json"
        "tests/unit/fail25.json"
        "tests/unit/fail26.json"
        "tests/unit/fail27.json"
        "tests/unit/fail28.json"
        "tests/unit/fail29.json"
        "tests/unit/fail3.json"
        "tests/unit/fail30.json"
        "tests/unit/fail31.json"
        "tests/unit/fail32.json"
        "tests/unit/fail33.json"
        "tests/unit/fail4.json"
        "tests/unit/fail5.json"
        "tests/unit/fail6.json"
        "tests/unit/fail7.json"
        "tests/unit/fail8.json"
        "tests/unit/fail9.json"
        "tests/unit/pass1.json"
        "tests/unit/pass2.json"
        "tests/unit/pass3.json"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = if flags.split-base
          then (([
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (if flags.generic
            then [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
              ]
            else [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              ])) ++ (pkgs.lib).optional (flags.parsec) (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))) ++ (pkgs.lib).optional (flags.pretty) (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          else [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        modules = [
          "Text/JSON"
          "Text/JSON/Types"
          "Text/JSON/String"
          "Text/JSON/ReadP"
          ] ++ (pkgs.lib).optionals (flags.split-base) (((pkgs.lib).optional (flags.generic) "Text/JSON/Generic" ++ (pkgs.lib).optional (flags.parsec) "Text/JSON/Parsec") ++ (pkgs.lib).optional (flags.pretty) "Text/JSON/Pretty");
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/3; }
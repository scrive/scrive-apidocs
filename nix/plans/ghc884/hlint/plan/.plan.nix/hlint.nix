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
    flags = { threaded = true; gpl = true; ghc-lib = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "hlint"; version = "2.2.11"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2006-2020";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/ndmitchell/hlint#readme";
      url = "";
      synopsis = "Source code suggestions";
      description = "HLint gives suggestions on how to improve your source code.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "data";
      dataFiles = [
        "hlint.yaml"
        "default.yaml"
        "Test.hs"
        "report_template.html"
        "hs-lint.el"
        "hlint.1"
        "hlint.ghci"
        "HLint_QuickCheck.hs"
        "HLint_TypeCheck.hs"
        ];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" "CHANGES.txt" ];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."cpphs" or (errorHandler.buildDepError "cpphs"))
          (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."haskell-src-exts" or (errorHandler.buildDepError "haskell-src-exts"))
          (hsPkgs."haskell-src-exts-util" or (errorHandler.buildDepError "haskell-src-exts-util"))
          (hsPkgs."uniplate" or (errorHandler.buildDepError "uniplate"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."refact" or (errorHandler.buildDepError "refact"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."filepattern" or (errorHandler.buildDepError "filepattern"))
          (hsPkgs."ghc-lib-parser-ex" or (errorHandler.buildDepError "ghc-lib-parser-ex"))
          ] ++ (if !flags.ghc-lib && (compiler.isGhc && (compiler.version).ge "8.8.0") && (compiler.isGhc && (compiler.version).lt "8.9.0")
          then [
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
            (hsPkgs."ghc-boot" or (errorHandler.buildDepError "ghc-boot"))
            ]
          else [
            (hsPkgs."ghc-lib-parser" or (errorHandler.buildDepError "ghc-lib-parser"))
            ])) ++ (pkgs.lib).optional (flags.gpl) (hsPkgs."hscolour" or (errorHandler.buildDepError "hscolour"));
        buildable = true;
        modules = [
          "Paths_hlint"
          "Apply"
          "CmdLine"
          "Grep"
          "HLint"
          "HsColour"
          "Idea"
          "Report"
          "Util"
          "Parallel"
          "Refact"
          "Timing"
          "CC"
          "EmbedData"
          "Config/Compute"
          "Config/Haskell"
          "Config/Read"
          "Config/Type"
          "Config/Yaml"
          "GHC/Util"
          "GHC/Util/ApiAnnotation"
          "GHC/Util/View"
          "GHC/Util/Brackets"
          "GHC/Util/FreeVars"
          "GHC/Util/HsDecl"
          "GHC/Util/HsExpr"
          "GHC/Util/HsType"
          "GHC/Util/Pat"
          "GHC/Util/LanguageExtensions/Type"
          "GHC/Util/Module"
          "GHC/Util/Outputable"
          "GHC/Util/SrcLoc"
          "GHC/Util/DynFlags"
          "GHC/Util/RdrName"
          "GHC/Util/Scope"
          "GHC/Util/Unify"
          "HSE/All"
          "HSE/Match"
          "HSE/Scope"
          "HSE/Type"
          "HSE/Util"
          "Hint/All"
          "Hint/Bracket"
          "Hint/Comment"
          "Hint/Duplicate"
          "Hint/Export"
          "Hint/Extensions"
          "Hint/Import"
          "Hint/Lambda"
          "Hint/List"
          "Hint/ListRec"
          "Hint/Match"
          "Hint/Monad"
          "Hint/Naming"
          "Hint/NewType"
          "Hint/Pattern"
          "Hint/Pragma"
          "Hint/Restrict"
          "Hint/Smell"
          "Hint/Type"
          "Hint/Unsafe"
          "Hint/Util"
          "Test/All"
          "Test/Annotations"
          "Test/InputOutput"
          "Test/Proof"
          "Test/Translate"
          "Test/Util"
          "Language/Haskell/HLint"
          "Language/Haskell/HLint3"
          "Language/Haskell/HLint4"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "hlint" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hlint" or (errorHandler.buildDepError "hlint"))
            ];
          buildable = true;
          mainPath = [
            "src/Main.hs"
            ] ++ (pkgs.lib).optional (flags.threaded) "";
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }
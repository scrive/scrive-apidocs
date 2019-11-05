{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (IOException, SomeException, catch, evaluate)
import Control.Monad
import Control.Monad.Writer (execWriter, tell)
import Data.Char
import Data.List (intersect)
import Data.List.Extra (intersperse, trim)
import Data.Maybe
import Data.Version.Extra (makeVersion, readVersion, showVersion)
import Development.Shake
import Development.Shake.FilePath
import Distribution.Text (display)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess, readProcess)

import Shake.Cabal
import Shake.DBSchema (buildDBDocs)
import Shake.Flags
import Shake.NewBuild
import Shake.Oracles
import Shake.TeamCity
import Shake.Utils

opts :: String -> ShakeOptions
opts v = shakeOptions { shakeVersion = v
                      , shakeFiles = "_build"
                      , shakeVerbosity = Loud
                      -- When running on multiple threads, a build
                      -- failure on one thread does not stop other
                      -- threads from continuing.
                      --
                      -- Maybe this is default Shake behaviour, but it is
                      -- annoying as the build error gets lost in
                      -- output.
                      --
                      -- Run on single thread until we figure this out
                      , shakeThreads = 1
                      }
usageMsg :: String
usageMsg = unlines
  [ ""
  , "# Shake build system for Scrive"
  , "  Automatically runs dependencies, just specify your target"
  , "  Run with one or more targets"
  , ""
  , "  $> ./shake server"
  , ""
  , "  To see Environment Variables used run ./shake help-env"
  , ""
  , "# Build targets"
  , ""
  , "   all             : Build both server and frontend"
  , "   server          : Build all server-side executables"
  , "   frontend        : Build all frontend resources"
  , ""
  , "   haddock         : Build Haddock Documentation"
  , "   db-docs         : Build database schema docs"
  , ""
  , "                     SchemaCrawler must be installed, schemacrawler.sh"
  , "                     must be in PATH. GraphViz must be in PATH. Also the"
  , "                     DB must be alredy created and running."
  , ""
  , "# Test targets"
  , ""
  , "   test                      : Run all tests"
  , "   test-server               : Run all server-side tests"
  , "   test-frontend             : Run all frontend tests"
  , "   test-frontend-tests       : Run frontend Grunt Tests"
  , "   test-frontend-lint        : Run frontend Grunt Style Checkers"
  , ""
  , "                               Use the --pattern PATTERN (or -p) option to"
  , "                               only run test cases whose names match"
  , "                               a given pattern."
  , "# Distribution targets"
  , ""
  , "   dist                      : Build all and create .tar.gz archive"
  , ""
  , "# Formatting and linting targets"
  , "   hindent                   : Format code with hindent"
  , "   stylish-haskell           : Format code with stylish-haskell"
  , "   hlint                     : Run hlint, printing output to stdout"
  , "   hlint-report              : Run hlint, generating an HTML report"
  , "   hlint-refactor            : Run hlint, applying all hints automatically"
  , "   test-formatting           : Test whether code base has run through fix-formatting"
  , "   test-hs-import-order      : Run Haskell Import Order checking script"
  , "   fix-formatting            : Format code base using brittany and sort_import"
  , "   fix-formatting-quck       : Format code base using brittany and sort_import, only on changed files against master"
  , "   fix-hs-import-order       : Sort Haskell imports"
  , "   test-hs-outdated-deps     : Check for outdated Haskell dependencies"
  , ""
  , "                               Use the --src-subdir=DIR option to limit"
  , "                               the above commands to a part of the tree."
  , ""
  , "# Utility scripts"
  , ""
  , "   transifex-fix              : Sort local Transifex .json translation"
  , "                                files"
  , "   transifex-push             : Push local translation to Transifex"
  , "   transifex-diff             : Diff local translation against the remote"
  , "   transifex-merge            : Merge local translation with the remote"
  , "   transifex-help             : Help on using Transifex"
  , "   detect-old-localizations   : Detect old localizations"
  , "   detect-old-templates       : Detect old templates"
  , "   take-reference-screenshots : Take reference screenshots"
  , "   run-localization           : Update localization files"
  , "   scripts-help               : Help on using utility scripts"
  , ""
  , "# Clean"
  , ""
  , "   clean          : Clean all except Shake directory"
  , "   fresh          : Clean all (including Shake build data)"
  , "   clean-server   : Clean with 'cabal clean'"
  , "   clean-frontend : Clean with 'grunt clean'"
  , ""
  , "# Help"
  , ""
  , "   help           : This help message"
  , "   help-env       : Print information about environment variables used"
  , ""
  ]

checkPrerequisites :: IO ()
checkPrerequisites = do
  requireVersion "node"  ["--version"] (makeVersion [4,0,0]) tail
  requireVersion "grunt" ["--version"] (makeVersion [0,1,0])
    (takeWhile dotOrNum . drop 11)
  requireVersion "lessc" ["--version"] (makeVersion [2,5,0])
    (takeWhile dotOrNum . drop 6)
  requireVersion "cabal" ["--numeric-version"] (makeVersion [2,4,0]) id
  requireVersion "alex"  ["--version"] (makeVersion [3,1,0])
    (takeWhile dotOrNum . drop 13)
  requireVersion "happy" ["--version"] (makeVersion [1,18,0])
    (takeWhile dotOrNum . drop 14)
  requireVersion "brittany" ["--version"] (makeVersion [0,12,0,0])
    (takeWhile dotOrNum . drop 17)

    where
      dotOrNum c = c == '.' || isNumber c

      requireVersion  prog args ver pre =
        requireVersion' prog args ver pre
        `catch` (\(_ :: IOException) ->
                   hPutStrLn stderr ("Required program '"
                                     <> prog <> "' couldn't be found.") >>
                   exitFailure)

      requireVersion' prog args ver pre = do
        out  <- pre . trim <$> readProcess prog args ""
        ver' <- (evaluate . readVersion $ out) `catch`
                (\(_ :: SomeException) ->
                   hPutStrLn stderr ("Parse error: " <> prog <> " " <> show args
                                     <> ": " <> out)
                   >> exitFailure)
        when (ver' < ver) $ do
          hPutStrLn stderr
            ("Required program '" <> prog <> "' has wrong version: "
             <> showVersion ver'  <> ", at least "
             <> showVersion ver   <> " required.")
          exitFailure

main :: IO ()
main = do
  -- Check that the prerequisites are installed and their versions are
  -- correct.
  checkPrerequisites

  sourceRoot <- fromMaybe "." <$> (lookupEnv "KONTRAKCJA_ROOT")

  -- Used to check if Shake.hs rules changed, triggering a full rebuild.
  hsDeps       <- getHsDeps $ sourceRoot </> "Shake"
  ver          <- getHashedShakeVersion $ ["shake.sh"] ++ hsDeps
  -- Dependency information needed by our rules.
  cabalFile    <- parseCabalFile $ sourceRoot </> "kontrakcja.cabal"
  shakeArgsWith (opts ver) shakeFlags $ \flags targets -> return . Just $ do
    newBuild <- liftIO $ mkUseNewBuild flags cabalFile
    let opt        = getOptimisationLevel flags
        exeDynamic = DisableExecutableDynamic `notElem` flags

    if null targets then want ["help"] else want targets

    -- * First add Oracles
    addOracles

    -- * The "help" phony task that is the default target
    "help" ~> putNormal usageMsg
    -- * Main targets
    "all"                  ~> need ["server", "frontend"]
    "server"               ~> need ["_build/cabal-build"]
    "frontend"             ~> need ["_build/grunt-build"]

    "haddock"              ~> need ["_build/cabal-haddock.tar.gz"]
    "db-docs"              ~> need ["_build" </> "db-docs" </> "kontra.html"]
    "test"                 ~> need ["test-server","test-frontend"
                                   ,"test-hs-import-order"]
    "test-server"          ~> need ["run-server-tests"]
    "test-frontend"        ~> need ["test-frontend-tests","test-frontend-lint"]
    "test-hs-import-order" ~> need ["_build/hs-import-order"]

    "test-frontend-tests"      ~> need ["grunt-test"]
    "test-frontend-lint"       ~> need ["grunt-eslint"]

    "dist" ~> need ["_build/kontrakcja.tar.gz"]

    "transifex-fix"              ~> runTransifexFixScript   newBuild opt
    "transifex-push"             ~> runTransifexPushScript  newBuild opt flags
    "transifex-diff"             ~> runTransifexDiffScript  newBuild opt flags
    "transifex-merge"            ~> runTransifexMergeScript newBuild opt flags
    "transifex-help"             ~> do putNormal transifexUsageMsg
                                       putNormal "-----------------------------"
                                       putNormal "Output of 'transifex --help':"
                                       putNormal "-----------------------------"
                                       runTransifexUsageScript newBuild opt
    "detect-old-localizations"   ~> runDetectOldLocalizationsScript newBuild opt
    "detect-old-templates"       ~> runDetectOldTemplatesScript newBuild opt
    "take-reference-screenshots" ~> runTakeReferenceScreenshotsScript
    "run-localization"           ~> runLocalization newBuild opt
    "scripts-help"               ~> putNormal scriptsUsageMsg

    "clean"          ~> need ["clean-server","clean-frontend"]
    "clean-server"   ~> need ["cabal-clean"]
    "clean-frontend" ~> need ["grunt-clean"]
    "fresh"          ~> do
      need ["clean"]
      removeFilesAfter "_build/" ["//*"]
      removeFilesAfter "_shake/" ["//*"]

    -- * Rules
    componentBuildRules   sourceRoot newBuild opt cabalFile
    serverBuildRules      newBuild opt exeDynamic cabalFile
    serverTestRules       newBuild opt cabalFile
                          (mkCreateDBWithTestConf flags)
                          [pat | TestPattern pat <- flags]
    serverFormatLintRules sourceRoot newBuild opt cabalFile flags
    frontendBuildRules    newBuild
    frontendTestRules     newBuild
    distributionRules     newBuild opt
    oracleHelpRule

-- * Server

getOptimisationLevel :: [ShakeFlag] -> OptimisationLevel
getOptimisationLevel flags =
  last (NoOptimisation : [ optlevel | OptimisationLevel optlevel <- flags ])

-- | Server build rules
serverBuildRules :: UseNewBuild -> OptimisationLevel -> EnableExecutableDynamic
                 -> CabalFile
                 -> Rules ()
serverBuildRules newBuild opt exeDynamic cabalFile = do
  ifNewBuild newBuild (serverNewBuildRules opt exeDynamic cabalFile)
                      (serverOldBuildRules opt exeDynamic cabalFile)
  "_build" </> "db-docs" </> "kontra.html" %> buildDBDocs

getCabalConfigureFlags :: OptimisationLevel -> EnableExecutableDynamic
                       -> Action [String]
getCabalConfigureFlags opt exeDynamic = do
  testCoverage <- askOracle (BuildTestCoverage ())
  flags0       <- askOracle (BuildCabalConfigureOptions ())
  return . execWriter $ do
    tell [flags0]
    when testCoverage $ tell ["--enable-coverage"]
    case opt of
      MaxOptimisation     -> tell ["-O2"]
      DefaultOptimisation -> return ()
      NoOptimisation      -> tell ["-O0"]
    when exeDynamic $ tell ["--enable-executable-dynamic"]

serverNewBuildRules :: OptimisationLevel -> EnableExecutableDynamic
                    -> CabalFile -> FilePath
                    -> Rules ()
serverNewBuildRules opt exeDynamic cabalFile buildDir = do
  "_build/cabal-update" %>>> do
    sourceRoot <- askOracle (SourceRoot ())
    need
      [ sourceRoot </> "kontrakcja.cabal"
      ]

    cmd (EchoStdout True) "cabal v2-update"

  "cabal.project.local" %> \_ -> do
    sourceRoot <- askOracle (SourceRoot ())
    need
      [ "_build/cabal-update"
      , sourceRoot </> "kontrakcja.cabal"
      ]

    flags <- getCabalConfigureFlags opt exeDynamic
    command [Shell] "cabal" ("v2-configure":flags)

  "_build/cabal-build" %>>> do
    sourceRoot <- askOracle (SourceRoot ())
    need ["cabal.project.local"]
    tc <- askOracle (TeamCity ())
    when tc $ do
      -- Need to rebuild on TeamCity because version code for
      -- resources that is generated in src/Version.hs is used for stuff
      -- that is rebuilt with new version code by `grunt build`.
      alwaysRerun
      -- Force GHC to rebuild the TH-containing module.
      cmd "touch" (sourceRoot </> "backend" </> "lib" </> "Version.hs")
    needServerHaskellFiles cabalFile
    cmd (EchoStdout True) "cabal v2-build all"

  "_build/cabal-haddock.tar.gz" %> \_ -> do
    need ["_build/cabal-build"]
    needServerHaskellFiles cabalFile
    -- Limit to library from package, due to Cabal bug:
    -- https://github.com/haskell/cabal/issues/1919
    command_ [] "cabal" [ "v2-haddock"
                        , "all"
                        ]
    command_ [Shell] "tar" ["-czf","_build/cabal-haddock.tar.gz"
                           , buildDir </> "noopt/doc"]

  "cabal-clean" ~> cmd "cabal v2-clean"

serverOldBuildRules :: OptimisationLevel -> EnableExecutableDynamic -> CabalFile
                    -> Rules ()
serverOldBuildRules opt exeDynamic cabalFile = do
  "cabal.sandbox.config" %> \_ -> do
    sandbox <- askOracle (BuildSandbox ())
    if null sandbox
       then cmd "cabal sandbox init"
       else cmd $ "cabal sandbox init --sandbox=" <> sandbox

  "_build/cabal-update" %>>> do
    sourceRoot <- askOracle (SourceRoot ())
    need
      [ "cabal.config"
      , sourceRoot </> "kontrakcja.cabal"
      ]
    cmd (EchoStdout True) "cabal update"

  "_build/cabal-install-deps" %>>> do
    sourceRoot <- askOracle (SourceRoot ())
    void $ askOracleWith (GhcVersion ()) ""
    need
      [ "cabal.config"
      , sourceRoot </> "kontrakcja.cabal"
      , "cabal.sandbox.config"
      , "_build/cabal-update"
      ]

    cmd (EchoStdout True) "cabal" [ "install", "--enable-tests"
                                  , "--only-dependencies"
                                  , "--reorder-goals"
                                  , "--force-reinstalls" ]

  -- If this rule was run, then cabal was configured with flags
  -- Using a separate production rule lets us avoid having to run cabal clean
  -- every time, because then we know that it was run with up to date flags
  "_build/cabal-configure-with-flags" %>>> do
    sourceRoot <- askOracle (SourceRoot ())
    need
      [ "cabal.config"
      , sourceRoot </> "kontrakcja.cabal"
      , "_build/cabal-install-deps"
      ]
    tc <- askOracle (TeamCity ())
    -- Need to rebuild on TeamCity because version code for resources
    -- that is generated in src/Version.hs is used for stuff that is
    -- rebuilt with new version code by `grunt build`.
    when tc $ alwaysRerun
    -- Need to clean for flags to be effective.
    command_ [] "cabal" ["clean"]

    flags <- getCabalConfigureFlags opt exeDynamic
    command [Shell] "cabal" ("configure":"--enable-tests":flags)

  "dist/setup-config" %> \_ -> do
    sourceRoot <- askOracle (SourceRoot ())
    need
      [ "cabal.config"
      , sourceRoot </> "kontrakcja.cabal"
      , "_build/cabal-install-deps"
      ]
    tc <- askOracle (TeamCity ())
    testCoverage <- askOracle (BuildTestCoverage ())
    cabalFlags <- askOracleWith (BuildCabalConfigureOptions ()) ""
    if tc || testCoverage || (not . null) cabalFlags
      then need ["_build/cabal-configure-with-flags"]
      else cmd (Shell) $ "cabal configure --enable-tests"

  "_build/cabal-build" %>>> do
    need ["dist/setup-config"]
    needServerHaskellFiles cabalFile
    cmd (EchoStdout True) "cabal build"

  "_build/cabal-haddock.tar.gz" %> \_ -> do
    need ["_build/cabal-build"]
    needServerHaskellFiles cabalFile
    -- Limit to library from package, due to Cabal bug:
    -- https://github.com/haskell/cabal/issues/1919
    command_ [] "cabal" ["haddock","--internal"]
    command_ [Shell] "tar" ["-czf","_build/cabal-haddock.tar.gz","dist/doc"]

  "cabal-clean" ~> cmd "cabal clean"

-- | Should a new test configuration be created with new DB
-- configuration for this test run?
data CreateTestDBWithNewConf = CreateTestDBWithNewConf| DontCreateTestConf

mkCreateDBWithTestConf :: [ShakeFlag] -> CreateTestDBWithNewConf
mkCreateDBWithTestConf flags =
  if CreateDB `elem` flags then CreateTestDBWithNewConf else DontCreateTestConf

-- | Server test rules
serverTestRules :: UseNewBuild -> OptimisationLevel -> CabalFile
                -> CreateTestDBWithNewConf
                -> [Pattern]
                -> Rules ()
serverTestRules newBuild opt cabalFile createDBWithConf testPatterns = do
  "kontrakcja_test.conf" %> \_ ->
    case createDBWithConf of
      CreateTestDBWithNewConf -> do
        (dbName, initialConnString, lConf, s3Conf, cronMonthlyInvoiceConf) <-
          askOracle $ CreateTestDBWithConfData ()
        liftIO $ writeFile "kontrakcja_test.conf"
          ("{ "
            <> "\"database\":\"" <> initialConnString
                                 <> " dbname='" <> dbName <> "'" <> "\" , "
            <> "\"pdftools_lambda\":" <> lConf <> ","
            <> "\"amazon\":" <> s3Conf <> ","
            <> "\"cron_monthly_invoice\":" <> cronMonthlyInvoiceConf
            <> "}")
      DontCreateTestConf -> do
        tc <- askOracle (TeamCity ())
        when tc $ do
          testConfFile <- askOracle (BuildTestConfPath ())
          copyFile' testConfFile "kontrakcja_test.conf"

  "run-server-tests" ~> do
    let testSuiteNames    = testComponentNames cabalFile
        testSuiteExePaths = map (componentTargetPath newBuild opt) testSuiteNames
    need $ "kontrakcja_test.conf" : map unComponentName testSuiteNames
    -- removeFilesAfter is only performed on a successfull build, this file
    -- needs to be cleaned regardless otherwise successive builds will fail
    liftIO $ removeFiles "." ["kontrakcja-test.tix"]
    sourceRoot <- askOracle (SourceRoot ())
    withDB createDBWithConf $ forM_ testSuiteExePaths $ \testSuiteExe -> do
      tc <- askOracle (TeamCity ())
      let testPatterns'
            | null testPatterns = []
            | otherwise         = "-t" : intersperse "-t" testPatterns
      if tc
        then do
          target <- askOracle (BuildTarget ())
          let cmdopt = [Shell] <> langEnv
              flags = ["--plain"
                      ,"--output-dir _build/kontrakcja-test-artefacts"]
                      <> testPatterns'
                      <> case target of
                           "staging" -> ["--staging-tests"]
                           _         -> []
          (Exit c, Stdouterr out) <- command cmdopt testSuiteExe flags
          liftIO $ hUnitForTeamCity c out
        else
          cmd testSuiteExe testPatterns'
    testCoverage <- askOracle (BuildTestCoverage ())
    when testCoverage $ do
      putNormal "Checking if kontrakcja-test.tix was generated by tests..."
      need ["kontrakcja-test.tix"]
      command_ [Shell]
        ("ls "
         <> (sourceRoot </> "backend/test/src")
         <> " | awk -F \".\" '{print \"--exclude=\"$1}' "
         <> "| hpc markup `xargs echo` "
         <> "--destdir=coverage-reports kontrakcja-test.tix "
         <> concat [ "--hpcdir=" <> hpcPath <> " "
                   | hpcPath <- hpcPaths cabalFile newBuild opt ]
        ) []
      command_ [Shell]
        ("zip -r _build/coverage-reports.zip "
         <> "coverage-reports kontrakcja-test.tix") []
      removeFilesAfter "coverage-reports" ["//*"]

        where
          withDB DontCreateTestConf      act = act
          withDB CreateTestDBWithNewConf act = do
            (dbName, connString, (_::String), (_::String), (_::String)) <-
              askOracle $ CreateTestDBWithConfData ()
            (mkDB connString dbName >> act)
              `actionFinally` (rmDB connString dbName)
            where
              mkDB connString dbName = do
                unit $ cmd "psql" [connString <> " dbname=postgres", "-c"
                                  ,"CREATE DATABASE " <> dbName <> ";"]
                unit $ cmd "psql" [connString <> " dbname=" <> dbName, "-c"
                                  ,"CREATE EXTENSION IF NOT EXISTS pgcrypto;"]
              rmDB connString dbName =
                callProcess "psql" [connString <> " dbname=postgres", "-c"
                                   ,"DROP DATABASE IF EXISTS " <> dbName <> ";"]

needHaskellFilesInDirectories :: [FilePath] -> Action ()
needHaskellFilesInDirectories paths = do
  sourceRoot <- askOracle (SourceRoot ())
  needPatternsInDirectories sourceRoot ["//*.hs"] paths

needAllHaskellFiles :: CabalFile -> Action ()
needAllHaskellFiles = needHaskellFilesInDirectories . allHsSourceDirs

needServerHaskellFiles :: CabalFile -> Action ()
needServerHaskellFiles = needHaskellFilesInDirectories . allLibExeHsSourceDirs

serverFormatLintRules :: FilePath -> UseNewBuild -> OptimisationLevel
                      -> CabalFile -> [ShakeFlag] -> Rules ()
serverFormatLintRules sourceRoot newBuild opt cabalFile flags = do
  "_build/hs-import-order" %>>> do
    needAllHaskellFiles cabalFile
    need [componentTargetPath newBuild opt (mkExeName "sort_imports")]
    hsImportOrderAction True srcSubdirs

  "fix-formatting" ~> do
    need ["fix-hs-import-order", "brittany"]

  "fix-formatting-quick" ~> do
    need ["fix-hs-import-order", "brittany-quick"]

  "test-formatting" ~> do
    need ["test-hs-import-order", "brittany-check"]

  "fix-hs-import-order" ~> do
    need [componentTargetPath newBuild opt (mkExeName "sort_imports")]
    hsImportOrderAction False srcSubdirs

  "test-hs-outdated-deps" ~> do
    cmd "cabal" ["outdated", "--exit-code",
                 if useNewBuild newBuild
                 then "--v2-freeze-file"
                 else "--freeze-file"]

  "hindent" ~> do
    forM_ srcSubdirs $ \subdir ->
      onEachHsFile subdir ["hindent", "-XNoPatternSynonyms"]

  "list-extensions" ~> do
    forM_ (allExtensions cabalFile) $ \ext ->
      putNormal . display $ ext

  "stylish-haskell" ~> do
    forM_ srcSubdirs $ \subdir ->
      onEachHsFile subdir ["stylish-haskell", "-i"]

  "hlint" ~> do
    cmd "hlint" (commonHLintOpts <> srcSubdirs)

  "hlint-report" ~> do
    unit $ cmd "hlint" (commonHLintOpts <> ("-q":"--report":srcSubdirs))
    putNormal "Output written to report.html"

  "hlint-refactor" ~> do
    forM_ srcSubdirs $ \subdir ->
      onEachHsFile subdir (["hlint"] <> commonHLintOpts
                            <> [ "--refactor", "--refactor-options=-i"])

  "brittany" ~> do
    hsFiles <- listHsFiles
    forM_ hsFiles $
      \srcPath -> do
        unit $ command [] "brittany"
          $ [ "--config-file"
            , sourceRoot </> "brittany.yaml"
            , "--write-mode"
            , "inplace"
            , srcPath
            ]

  "brittany-quick" ~> do
    hsFiles <- listHsFiles
    (Exit code, Stdout res, Stderr err) <-
      command [Cwd sourceRoot] "git" ["diff", "master", "--name-only"]
    case code of
      ExitSuccess -> do
        let changedFiles = fmap (\file -> sourceRoot </> file) $ lines res
        let changedHsFiles = intersect hsFiles changedFiles
        forM_ changedHsFiles $
          \srcPath -> do
            unit $ command [] "brittany"
              $ [ "--config-file"
                , sourceRoot </> "brittany.yaml"
                , "--write-mode"
                , "inplace"
                , srcPath
                ]

        return ()
      ExitFailure _ ->
        fail $ "Failed to run git diff: " <> err

  "brittany-check" ~> do
    hsFiles <- listHsFiles
    unformattedFiles <- fmap join $
      forM hsFiles $ \srcPath -> do
        formatted <- checkFormatted $ srcPath
        if formatted
        then return []
        else do
          putNormal $ "file is not formatted: " <> srcPath
          return [srcPath]
    case unformattedFiles of
      [] -> return ()
      _ -> fail $
        "formatting check failed. the following files are unformatted:\n" <>
        (concat $ intersperse "\n" unformattedFiles)

    where
      srcSubdirs :: [FilePath]
      srcSubdirs =
        case [subdir | SrcSubdir subdir <- flags] of
          [] -> fmap (sourceRoot </>) $
            "Shake" : allHsSourceDirs cabalFile
          ds -> ds

      commonHLintOpts = ["-XNoPatternSynonyms" , "--no-exit-code", "--cross"]

      listHsFiles :: Action [String]
      listHsFiles = fmap join $
        forM srcSubdirs $ \subdir -> do
          files <- getDirectoryFiles subdir ["//*.hs"]
          return $ fmap (\file -> subdir </> file) files

      onEachHsFile :: String -> [String] -> Action ()
      onEachHsFile subdir act = unit $
        cmd "find" $ [subdir, "-type", "f", "-name", "*.hs", "-exec"]
                     <> act <> ["{}", ";"]

      checkFormatted srcPath = do
        Exit code <- command [] "brittany"
          $ [ "--config-file"
            , sourceRoot </> "brittany.yaml"
            , "--check-mode"
            , srcPath
            ]
        case code of
          ExitSuccess -> return True
          ExitFailure _ -> return False

      hsImportOrderAction checkOnly dirs = do
        let sortImportsFlags = if checkOnly then ("--check":dirs) else dirs
        command ([Shell] <> langEnv)
          (componentTargetPath newBuild opt . mkExeName $ "sort_imports")
          sortImportsFlags

-- * Frontend

gruntNewBuildArg :: UseNewBuild -> String
gruntNewBuildArg unb | useNewBuild unb = "--new-build"
                     | otherwise       = "--no-new-build"

-- | Frontend build rules
frontendBuildRules :: UseNewBuild -> Rules ()
frontendBuildRules newBuild = do
  "_build/npm-install" %>>> do
    sourceRoot <- askOracle (SourceRoot ())
    need [ sourceRoot </> "frontend/package.json" ]
    command
      [ EchoStdout True
      , Cwd $ sourceRoot </> "frontend"
      ]
      "npm"
      ["install"]

  "_build/grunt-build" %>>> do
    sourceRoot <- askOracle (SourceRoot ())
    need [ "_build/npm-install"
         , "localization"
         ]
    alwaysRerun
    command
      ([ EchoStdout True
       , Cwd $ sourceRoot </> "frontend"
       ] <> langEnv)
      "grunt"
      ["build", gruntNewBuildArg newBuild]

  "grunt-clean" ~> do
    sourceRoot <- askOracle (SourceRoot ())
    need ["_build/npm-install"]
    cmd
      [Cwd $ sourceRoot </> "frontend"]
      "grunt"
      ["clean", gruntNewBuildArg newBuild]

-- | Frontend test rules
frontendTestRules :: UseNewBuild -> Rules ()
frontendTestRules newBuild = do
  "grunt-eslint" ~> do
    sourceRoot <- askOracle (SourceRoot ())
    need ["_build/npm-install"]
    cmd
      [Cwd $ sourceRoot </> "frontend"]
      "grunt"
      ["eslint", gruntNewBuildArg newBuild]

  "grunt-coverage" ~> do
    sourceRoot <- askOracle (SourceRoot ())
    need ["_build/grunt-build"]
    command_
      [Cwd $ sourceRoot </> "frontend"]
      "grunt"
      ["test", gruntNewBuildArg newBuild]

  "grunt-test" ~> do
    sourceRoot <- askOracle (SourceRoot ())
    need ["_build/grunt-build"]
    testCoverage <- askOracle (BuildTestCoverage ())
    case testCoverage of
      False -> cmd
        [Cwd $ sourceRoot </> "frontend"]
        "grunt"
        ["test:fast", gruntNewBuildArg newBuild]
      True -> do
        need ["grunt-coverage"]
        command_ [Shell] "zip -r _build/JS_coverage.zip frontend/coverage/" []
        removeFilesAfter "frontend/coverage" ["//*"]

-- * Create distribution
distributionRules :: UseNewBuild -> OptimisationLevel -> Rules ()
distributionRules newBuild opt = do
  "urls.txt" %> \_ -> do
    tc <- askOracle (TeamCity ())
    nginxconfrulespath <- askOracle (NginxConfRulesPath ())
    nginxconfdefaultrule <- askOracle (NginxConfDefaultRule ())
    nginxconfrulespathalternative <- askOracle (NginxConfRulesPathAlternative ())
    unless tc $ do
      fail $ "ERROR: routinglist executable is only built "
        <> "with Shake when running from TeamCity"
    when (null nginxconfrulespath) $ do
      fail "ERROR: NGINX_CONF_RULES_PATH is empty"
    let routingListPath = componentTargetPath newBuild opt . mkExeName
                          $ "routinglist"
        nginxconfrulespathalternative' = if null nginxconfrulespathalternative
          then nginxconfrulespath
          else nginxconfrulespathalternative
    need [routingListPath]
    command_ [FileStdin nginxconfrulespath] routingListPath ["urls.txt", nginxconfdefaultrule]
    command_ [FileStdin nginxconfrulespathalternative'] routingListPath ["urls_list.txt", nginxconfdefaultrule]
    removeFilesAfter "." ["urls.txt", "urls_list.txt"]

  let binaryNames = map mkExeName $
                    [ "kontrakcja-server"
                    , "cron"
                    , "kontrakcja-migrate"
                    , "mailing-server"
                    , "messenger-server"
                    , "config-checker" ]

  copyBinariesRules binaryNames

  "_build/kontrakcja.tar.gz" %> \_ -> do
    let binaries = map binaryPath binaryNames
    need $ ["all", "urls.txt"] <> binaries
    let distFiles = binaries <>
                    [ "evidence-package/samples.p"
                    , "frontend/app/img"
                    , "frontend/app/less"
                    , "frontend/dist"
                    , "scrivepdftools"
                    , "GuardTime"
                    , "templates"
                    , "pure_sql"
                    , "files"
                    , "texts"
                    , "urls.txt"
                    , "urls_list.txt"
                    , "build-scripts/deployDevNginxRules.sh"
                    , "build-scripts/deployFeaturesTestbedNginxRules.sh"
                    , "certs"
                    ]
    command_ [Shell] "tar" $ ["-czf","_build/kontrakcja.tar.gz"] <> distFiles

    where
      binaryPath = componentTargetPath DontUseNewBuild DefaultOptimisation

      -- | For each given exe component, generate a rule for copying
      -- it to 'dist/build/compname', if needed.
      copyBinariesRules :: [CabalComponentName] -> Rules ()
      copyBinariesRules binaryNames =
        whenNewBuild newBuild $ \_ ->
          forM_ binaryNames $ \binaryName -> do
            let sourcePath = componentTargetPath newBuild opt binaryName
                targetPath = binaryPath binaryName
            targetPath %> \_ -> do
              liftIO $ createDirectoryIfMissing True (takeDirectory targetPath)
              copyFileChanged sourcePath targetPath

-- * Utility scripts.

scriptsUsageMsg :: String
scriptsUsageMsg = unlines $
  [ "Using utility scripts"
  , "====================="
  , ""
  , "transifex-fix"
  , "-------------"
  , "Sort local Transifex .json translation files."
  , "To use, run 'shake.sh transifex-fix ARGS'."
  , ""
  , "transifex-push"
  , "-------------"
  , "Push local translation to the Transifex server."
  , "To use, run 'shake.sh transifex-push ARGS'."
  , "Required arguments: --user, --password."
  , "Optional argument:  --lang."
  , ""
  , "transifex-diff"
  , "-------------"
  , "Diff local translation against the version on the server."
  , "To use, run 'shake.sh transifex-diff ARGS'."
  , "Required arguments: --user, --password."
  , "Optional argument:  --lang."
  , ""
  , "transifex-merge"
  , "-------------"
  , "Merge local translation with the remote one."
  , "To use, run 'shake.sh transifex-merge ARGS'."
  , "Required arguments: --user, --password."
  , "Optional argument:  --lang."
  , ""
  , "transifex-help"
  , "--------------"
  , "Help on using Transifex."
  , ""
  , "detect-old-localizations"
  , "------------------------"
  , "Detect old localizations. To use, run 'shake.sh detect-old-localizations'."
  , ""
  , "detect-old-templates"
  , "--------------------"
  , "Detect old templates. To use, run 'shake.sh detect-old-templates'."
  , ""
  , "take-reference-screenshots"
  , "--------------------------"
  , "Update reference screenshots."
  , "After running 'shake.sh take-reference-screenshots',"
  , "do the following steps:"
  , ""
  -- TODO: Explain the process better.
  , "x = concatLines base64 /tmp/author.png"
  , "authorJson = {\"time\":str(now()), in format 2015-09-22T16:00:00Z"
  , "              \"image\":\"data:image/jpeg;base64,\" + x}"
  , "writeFile files/reference_screenshots/author.json (dumps(authorJson))"
  , "same thing for /tmp/desktop2.png into "
    <> "files/reference_screenshots/standard.json"
  , "same thing for /tmp/mobile2.png into"
  , "files/reference_screenshots/mobile.json"
  , ""
  , "run-localization"
  , "--------------------------"
  , "Update the pre-generated localization files in"
  , "'frontend/app/localization/ for all languages."
  , "To use, run 'shake.sh run-localization'."
  ]

transifexUsageMsg :: String
transifexUsageMsg = unlines $
  [ "------------------------------------"
  , "How to do Transifex synchronisation:"
  , "------------------------------------"
  , ""
  , "First make sure that you are on good branch - the one that is"
  , "synched with TX. Right now it's staging."
  , ""
  , "WARNING: If you're on a different branch, you can destroy texts."
  , ""
  , "First, do:"
  , ""
  , " ./shake.sh transifex-diff --user USER --password PASS --lang en"
  , ""
  , "And check if response looks reasonable. If there are 500 texts"
  , "changed, it's bad, but 50 are ok. Now push your local source texts"
  , "to transifex. This will make all English texts available to"
  , "translators."
  , ""
  , "./shake.sh transifex-push --user USER --password PASS --lang en"
  , ""
  , "You should never do this with any other language, since it will"
  , "overwrite translations. And translations should be done in TX, not"
  , "in sources."
  , ""
  , "Now it's time to fetch stuff from TX. You do that with"
  , ""
  , "./shake.sh transifex-merge --user USER --password PASS"
  , ""
  , "This will fetch all source files for all languages. It will include"
  , "English, and it is possible that TX will drop some spaces, etc. It"
  , "still should be fine. For every change it will overwrite local file."
  , ""
  , "After you merged it is always good to run tests with -t"
  , "Localization. Errors should be fixed in TX, and then you just do"
  , "synchronization again. But at this point you will not need to push"
  , "English texts."
  ]

runTX :: UseNewBuild -> OptimisationLevel -> String -> [String] -> Action ()
runTX newBuild opt a args = do
  let scriptPath = componentTargetPath newBuild opt . mkExeName $ "transifex"
  need [scriptPath]
  command_ [] scriptPath ([a] <> args)

runTransifexUsageScript :: UseNewBuild -> OptimisationLevel -> Action ()
runTransifexUsageScript newBuild opt = do
  let scriptPath = componentTargetPath newBuild opt . mkExeName $ "transifex"
  need [scriptPath]
  -- 'transifex' prints out usage info if no command provided.
  cmd scriptPath

runTransifexFixScript :: UseNewBuild -> OptimisationLevel -> Action ()
runTransifexFixScript newBuild opt = runTX newBuild opt "fix" []

-- Transifex synch commands take USER PASSWORD [LANG] as parameters.
-- USER and PASSWORD are obligatory and ShakeFlags can have different
-- order. This function helps with conversion.
transifexServerParams :: [ShakeFlag] -> Action (String, String, Maybe String)
transifexServerParams flags = do
  case (findUser flags, findPassword flags) of
    (Just u, Just p) -> return (u, p, findLang flags)
    _ -> do
      fail "ERROR: user or password missing for comunication with TX"
  where
    findUser     xs = listToMaybe [ u | TransifexUser u     <- xs]
    findPassword xs = listToMaybe [ p | TransifexPassword p <- xs]
    findLang     xs = listToMaybe [ l | TransifexLang l     <- xs ]

runTransifexDiffScript :: UseNewBuild -> OptimisationLevel -> [ShakeFlag]
                       -> Action ()
runTransifexDiffScript newBuild opt flags = do
  (u,p,ml) <- transifexServerParams flags
  case ml of
    Just l -> runTX newBuild opt "diff-lang" [u, p, l]
    _      -> runTX newBuild opt "diff-all" [u, p]

runTransifexPushScript :: UseNewBuild -> OptimisationLevel -> [ShakeFlag]
                       -> Action ()
runTransifexPushScript newBuild opt flags = do
  (u,p,ml) <- transifexServerParams flags
  case ml of
    Nothing   -> runTX newBuild opt "push-lang" [u, p, "en"]
    Just "en" -> runTX newBuild opt "push-lang" [u, p, "en"]
    Just _    -> do
      fail "TX push command has to be run ONLY with language 'en'"

runTransifexMergeScript :: UseNewBuild -> OptimisationLevel -> [ShakeFlag]
                        -> Action ()
runTransifexMergeScript newBuild opt flags = do
  (u,p,ml) <- transifexServerParams flags
  case ml of
    Just l -> runTX newBuild opt "merge-lang" [u, p, l]
    _      -> runTX newBuild opt "merge-all" [u, p]

runDetectOldLocalizationsScript :: UseNewBuild -> OptimisationLevel -> Action ()
runDetectOldLocalizationsScript newBuild opt = do
  let scriptPath = componentTargetPath newBuild opt .
                   mkExeName $ "detect_old_localizations"
  need [scriptPath]
  cmd scriptPath

runDetectOldTemplatesScript :: UseNewBuild -> OptimisationLevel -> Action ()
runDetectOldTemplatesScript newBuild opt = do
  let scriptPath = componentTargetPath newBuild opt .
                   mkExeName $ "detect_old_templates"
  need [scriptPath]
  cmd scriptPath

runTakeReferenceScreenshotsScript :: Action ()
runTakeReferenceScreenshotsScript = do
  need ["scripts/take_reference_screenshots.py"]
  cmd "python scripts/take_reference_screenshots.py"

runLocalization :: UseNewBuild -> OptimisationLevel -> Action ()
runLocalization newBuild opt = do
  let exePath = componentTargetPath newBuild opt .
                mkExeName $ "localization"
  need [exePath]
  cmd  exePath

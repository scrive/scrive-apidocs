{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Development.Shake

import Shake.GitHub
import Shake.Oracles
import Shake.Utils

opts :: String -> ShakeOptions
opts v = shakeOptions { shakeVersion = v
                      , shakeFiles = "_build"
                      , shakeThreads = 0
                      }

main :: IO ()
main = do
  -- Used to check if Shake.hs rules changed, triggering a full rebuild
  ver <- getHashedShakeVersion ["shake.sh"
                               , "Shake/Shake.hs"
                               , "Shake/GitHub.hs"
                               , "Shake/Oracles.hs"
                               , "Shake/Utils.hs"
                               ]
  shakeArgs (opts ver) $ do
    want ["help"]

    -- * First add Oracles
    addOracles

    -- * The "help" phony task that is the default target
    "help" ~> do
      putNormal ""
      putNormal "# Shake build system for Scrive"
      putNormal "  Automatically runs dependancies, just specify your target"
      putNormal "  Run with one or more targets"
      putNormal ""
      putNormal "  $> ./shake build-server"
      putNormal ""
      putNormal "  To see Environment Variables used run ./shake help-env"
      putNormal ""
      putNormal "# Build targets"
      putNormal ""
      putNormal "   all             : Build both server and frontend"
      putNormal "   server          : Build all server-side executables"
      putNormal "   frontend        : Build all frontend resources"
      putNormal ""
      putNormal "   haddock         : Build Haddock Documentation"
      putNormal ""
      putNormal "# Test targets"
      putNormal ""
      putNormal "   test                      : Run all tests"
      putNormal "   test-server               : Run all server-side tests"
      putNormal "   test-frontend             : Run all frontend tests"
      putNormal "   test-hs-import-order      : Run Haskell Import Order checking script"
      putNormal "   fix-hs-import-order       : Sort Haskell imports"
      putNormal ""
      putNormal "   test-frontend-tests       : Run frontend Grunt Tests"
      putNormal "   test-frontend-jscs        : Run frontend Grunt JSCS Style Checker"
      putNormal ""
      putNormal "# Clean"
      putNormal ""
      putNormal "   clean          : Clean all except Shake directory"
      putNormal "   fresh          : Clean all (including Shake build data)"
      putNormal "   clean-server   : Clean with 'cabal clean'"
      putNormal "   clean-frontend : Clean with 'grunt clean'"
      putNormal ""

    -- * Main targets
    "all"       ~> need ["server", "frontend"]
    "server"    ~> need ["_build/cabal-build"]
    "frontend"  ~> need ["_build/grunt-build"]

    "haddock"   ~> need ["_build/cabal-haddock"]

    "test" ~> need ["test-server","test-frontend","test-hs-import-order"]
    "test-server" ~> need ["kontrakcja-test"]
    "test-frontend" ~> need ["test-frontend-tests","test-frontend-jscs"]
    "test-hs-import-order" ~> need ["_build/hs-import-order"]

    "test-frontend-tests"      ~> need ["grunt-test"]
    "test-frontend-jscs"       ~> need ["grunt-jscs"]

    "clean" ~> need ["clean-server","clean-frontend"]
    "clean-server" ~> need ["cabal-clean"]
    "clean-frontend" ~> need ["grunt-clean"]
    "fresh" ~> do
      need ["clean"]
      removeFilesAfter "_build/" ["//*"]
      removeFilesAfter "_shake/" ["//*"]

    -- * Rules
    serverBuildRules
    serverTestRules
    frontendBuildRules
    frontendTestRules
    oracleHelpRule

-- * Server

-- | Server build rules
serverBuildRules :: Rules ()
serverBuildRules = do
  let cabalFiles = ["cabal.config", "kontrakcja.cabal"]

  "cabal.sandbox.config" %> \_ -> do
    sandbox <- askOracle (BuildSandbox ())
    if null sandbox
       then cmd "cabal sandbox init"
       else cmd $ "cabal sandbox init --sandbox=" ++ sandbox

  "_build/cabal-update" %>>> do
    need cabalFiles
    cmd (EchoStdout True) "cabal update"

  "_build/cabal-install-deps" %>>> do
    need cabalFiles
    need [ "cabal.sandbox.config"
         , "_build/cabal-update"
         ]
    cmd (EchoStdout True) "cabal install --only-dependencies --force-reinstalls"

  "dist/setup-config" %> \_ -> do
    need cabalFiles
    need ["_build/cabal-install-deps"]
    testCoverage <- askOracle (BuildTestCoverage ())
    if testCoverage
      then do
        -- We need to clean binaries to force rebuild if we want to enable coverage reports
        need ["cabal-clean"]
        cmd "cabal configure -ftestcoverage"
      else
        cmd "cabal configure"

  "_build/cabal-build" %>>> do
    need ["dist/setup-config"]
    needServerHaskellFiles
    cmd (EchoStdout True) "cabal build"

  "_build/cabal-haddock" %>>> do
    need ["_build/cabal-build"]
    cmd "cabal haddock --all"

  "cabal-clean" ~> cmd "cabal clean"

-- | Server test rules
serverTestRules :: Rules ()
serverTestRules = do
  let hsImportOrderAction checkOnly = do
        let flags = if checkOnly then ("--check":hsSourceDirs) else hsSourceDirs
        command [Shell, AddEnv "LANG" "en_GB.UTF-8"] "runhaskell scripts/sort_imports.hs" flags

  "_build/hs-import-order" %>>> do
    needServerHaskellFiles
    withGitHub "Haskell import order" $ hsImportOrderAction True

  "fix-hs-import-order" ~> hsImportOrderAction False

  "kontrakcja_test.conf" %> \_ -> do
    tc <- askOracle (TeamCity ())
    when tc $ do
      testConfFile <- askOracle (BuildTestConfPath ())
      copyFile' testConfFile "kontrakcja_test.conf"

  "kontrakcja-test" ~> do
    need ["_build/cabal-build", "kontrakcja_test.conf"]
    tc <- askOracle (TeamCity ())
    if tc
      then do
        target <- askOracle (BuildTarget ())
        let cmdopt = [Shell, AddEnv "LANG" "en_US.UTF-8", AddEnv "LC_CTYPE" "en_US.UTF-8"]
            flags = ["--plain","--output-dir _build/kontrakcja-test-artefacts"]
              ++ case target of
                      "staging" -> ["--staging-tests"]
                      _ -> []
        command_ cmdopt "./build/dist/kontrakcja-test/kontrakcja-test" flags
      else cmd "./build/dist/kontrakcja-test/kontrakcja-test"
    testCoverage <- askOracle (BuildTestCoverage ())
    when testCoverage $ do
      putNormal "Checking if kontrakcja-test.tix was generated by tests..."
      need ["kontrakcja-test.tix"]
      command_ [Shell]
        ("ls test/src | awk -F \".\" '{print \"--exclude=\"$1}'"
         ++ " | hpc markup `xargs echo` --destdir=coverage-reports kontrakcja-test.tix"
        ) []
      command_ [Shell] "zip -r _build/coverage-reports.zip coverage-reports kontrakcja-test.tix" []
      removeFilesAfter "." ["kontrakcja-test.tix"]
      removeFilesAfter "coverage-reports" ["//*"]

needServerHaskellFiles :: Action ()
needServerHaskellFiles = needPatternsInDirectories ["//*.hs"] hsSourceDirs

-- | All HS source directories from kontrakcja.cabal
-- For `needServerHaskellFiles` we could do with top-level directories, but our
-- sort imports procedure needs the directories as defined in .cabal file as it
-- uses this information to decide if an import is foreign or not
-- FIXME This can be automated this using grep, ack, sort, uniq on kontrakcja.cabal:
-- grep 'hs-source-dirs' -i kontrakcja.cabal | sed -e 's/Hs-source-dirs://g' -e 's/^ *//g' | tr ' ' '\n' | sort -u
hsSourceDirs :: [FilePath]
hsSourceDirs =
  [ "cron"
  , "cron/inc"
  , "cron/schema"
  , "cron/src"
  , "docconv/src"
  , "docconv/test/src"
  , "inc"
  , "localization/src"
  , "mailer/inc"
  , "mailer/schema"
  , "mailer/src"
  , "messenger/inc"
  , "messenger/schema"
  , "messenger/src"
  , "misc"
  , "pdftools"
  , "schema/inc"
  , "schema/src"
  , "src"
  , "test/src"
  ]

-- * Frontend

-- | Frontend build rules
frontendBuildRules :: Rules ()
frontendBuildRules = do
  "_build/npm-install" %>>> do
    need ["frontend/package.json"]
    command [EchoStdout True, Cwd "frontend"] "npm" ["install"]

  "_build/grunt-build" %>>> do
    need [ "_build/npm-install"
         , "dist/setup-config" -- Need `cabal configure` as Grunt uses localization
         ]
    alwaysRerun
    withGitHub "Grunt Build" $
      command [EchoStdout True, Cwd "frontend", AddEnv "LC_ALL" "en_US.UTF-8"] "grunt" ["build"]

  "grunt-clean" ~> cmd (Cwd "frontend") "grunt clean"

-- | Frontend test rules
frontendTestRules :: Rules ()
frontendTestRules = do
  "grunt-jscs" ~> do
    need ["_build/npm-install"]
    withGitHub "JSCS" $
      cmd (Cwd "frontend") "grunt jscs"

  "grunt-test" ~> do
    need ["_build/grunt-build"]
    withGitHub "Grunt Test" $ do
      testCoverage <- askOracle (BuildTestCoverage ())
      case testCoverage of
        False -> cmd (Cwd "frontend") "grunt test:fast"
        True -> do
          command_ [Cwd "frontend"] "grunt" ["test"]
          command_ [Shell] "zip -r _build/JS_coverage.zip frontend/coverage/" []
          removeFilesAfter "frontend/coverage" ["//*"]

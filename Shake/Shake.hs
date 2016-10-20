{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.List
import Development.Shake
import Development.Shake.FilePath
import System.Console.GetOpt
import System.Exit (exitFailure)

import Shake.GitHub
import Shake.Oracles
import Shake.TeamCity
import Shake.Utils

opts :: String -> ShakeOptions
opts v = shakeOptions { shakeVersion = v
                      , shakeFiles = "_build"
                      , shakeVerbosity = Loud
                      -- When running on multiple threads, a build failure on one
                      -- thread did not stop other threads from continuing
                      -- Maybe this is default Shake behaviour but is annoying as
                      -- the build error gets lost in output
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
  , ""
  , "# Test targets"
  , ""
  , "   test                      : Run all tests"
  , "   test-server               : Run all server-side tests"
  , "   test-frontend             : Run all frontend tests"
  , "   test-hs-import-order      : Run Haskell Import Order checking script"
  , "   fix-hs-import-order       : Sort Haskell imports"
  , ""
  , "   test-frontend-tests       : Run frontend Grunt Tests"
  , "   test-frontend-lint        : Run frontend Grunt Style Checkers"
  , ""
  , "# Distribution targets"
  , ""
  , "   dist                      : Build all and create .tar.gz archive"
  , ""
  , "# Utility scripts"
  , ""
  , "   transifex-fix              : Sort local Transifex .json translation files"
  , "   transifex-push             : Push local translation to Transifex"
  , "   transifex-diff             : Diff local translation against the remote"
  , "   transifex-merge            : Merge local translation with the remote"
  , "   transifex-help             : Help on using Transifex"
  , "   detect-old-localizations   : Detect old localizations"
  , "   detect-old-templates       : Detect old templates"
  , "   take-reference-screenshots : Take reference screenshots"
  , "   scripts-help               : Help on using utility scripts"
  , ""
  , "# Clean"
  , ""
  , "   clean          : Clean all except Shake directory"
  , "   fresh          : Clean all (including Shake build data)"
  , "   clean-server   : Clean with 'cabal clean'"
  , "   clean-frontend : Clean with 'grunt clean'"
  , ""
  ]

data TransifexFlag = User String | Password String | Lang String | Resource String
  deriving (Eq, Ord)

transifexFlags :: [OptDescr (Either String TransifexFlag)]
transifexFlags =
  [ Option "" ["user"]     (reqArg User     "USER") "User name"
  , Option "" ["password"] (reqArg Password "PASS") "Password"
  , Option "" ["lang"]     (reqArg Lang     "LANG") "Language"
  , Option "" ["resource"] (reqArg Resource "NAME") "Resource"
  ]
  where
    reqArg toFlag name = ReqArg (Right . toFlag) name

main :: IO ()
main = do
  -- Used to check if Shake.hs rules changed, triggering a full rebuild
  ver <- getHashedShakeVersion ["shake.sh"
                               , "Shake/Shake.hs"
                               , "Shake/GitHub.hs"
                               , "Shake/Oracles.hs"
                               , "Shake/TeamCity.hs"
                               , "Shake/Utils.hs"
                               ]
  shakeArgsWith (opts ver) transifexFlags $ \flags targets -> return . Just $ do
    if null targets then want ["help"] else want targets

    -- * First add Oracles
    addOracles

    -- * The "help" phony task that is the default target
    "help" ~> putNormal usageMsg
    -- * Main targets
    "all"       ~> need ["server", "frontend"]
    "server"    ~> need ["_build/cabal-build"]
    "frontend"  ~> need ["_build/grunt-build"]

    "haddock"   ~> need ["_build/cabal-haddock.tar.gz"]

    "test" ~> need ["test-server","test-frontend","test-hs-import-order"]
    "test-server" ~> need ["kontrakcja-test"]
    "test-frontend" ~> need ["test-frontend-tests","test-frontend-lint"]
    "test-hs-import-order" ~> need ["_build/hs-import-order"]

    "test-frontend-tests"      ~> need ["grunt-test"]
    "test-frontend-lint"       ~> need ["grunt-eslint"]

    "dist" ~> need ["_build/kontrakcja.tar.gz"]

    "transifex-fix"              ~> runTransifexScript "fix"   []
    "transifex-push"             ~> runTransifexScript "push"  flags
    "transifex-diff"             ~> runTransifexScript "diff"  flags
    "transifex-merge"            ~> runTransifexScript "merge" flags
    "transifex-help"             ~> do putNormal transifexUsageMsg
                                       putNormal "-----------------------------"
                                       putNormal "Output of 'transifex --help':"
                                       putNormal "-----------------------------"
                                       runTransifexScript "" []
    "detect-old-localizations"   ~> runDetectOldLocalizationsScript
    "detect-old-templates"       ~> runDetectOldTemplatesScript
    "take-reference-screenshots" ~> runTakeReferenceScreenshotsScript

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
    utilityScriptRules
    distributionRules
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
    _ <- askOracleWith (GhcVersion ()) ""
    need cabalFiles
    need [ "cabal.sandbox.config"
         , "_build/cabal-update"
         ]
    cmd (EchoStdout True) "cabal install --only-dependencies --force-reinstalls"

  -- If this rule was run, then cabal was configured with flags
  -- Using a separate production rule lets us avoid having to run cabal clean
  -- every time, because then we know that it was run with up to date flags
  "_build/cabal-configure-with-flags" %>>> do
    need cabalFiles
    need ["_build/cabal-install-deps"]
    tc <- askOracle (TeamCity ())
    testCoverage <- askOracle (BuildTestCoverage ())
    cabalFlags <- askOracle (BuildCabalConfigureOptions ())
    let flags = if tc then ["-fenable-routinglist",cabalFlags]
                      else [cabalFlags]
    -- Need to rebuild on TeamCity because versioncode for resources that is
    -- generated in src/Version.hs is used for stuff that is rebuilt with new
    -- versioncode by `grunt build`
    when tc $ alwaysRerun
    -- Need to clean for flags to be effective
    command_ [] "cabal" ["clean"]
    case testCoverage of
      True -> command [Shell] "cabal" $ "configure":"-ftest-coverage":flags
      False -> command [Shell] "cabal" $ "configure":flags

  "dist/setup-config" %> \_ -> do
    need cabalFiles
    need ["_build/cabal-install-deps"]
    tc <- askOracle (TeamCity ())
    testCoverage <- askOracle (BuildTestCoverage ())
    cabalFlags <- askOracleWith (BuildCabalConfigureOptions ()) ""
    if tc || testCoverage || (not . null) cabalFlags
      then need ["_build/cabal-configure-with-flags"]
      else cmd (Shell) $ "cabal configure"

  "_build/cabal-build" %>>> do
    need ["dist/setup-config"]
    needServerHaskellFiles
    cmd (EchoStdout True) "cabal build"

  "_build/cabal-haddock.tar.gz" %> \_ -> do
    need ["_build/cabal-build"]
    needServerHaskellFiles
    -- Limit to library from package, due to Cabal bug:
    -- https://github.com/haskell/cabal/issues/1919
    command_ [] "cabal" ["haddock","--internal"]
    command_ [Shell] "tar" ["-czf","_build/cabal-haddock.tar.gz","dist/doc"]

  "cabal-clean" ~> cmd "cabal clean"

-- | Server test rules
serverTestRules :: Rules ()
serverTestRules = do
  let hsImportOrderAction checkOnly = do
        let flags = if checkOnly then ("--check":sourceDirsFromCabal)
                    else sourceDirsFromCabal
        command ([Shell] ++ langEnv) "runhaskell scripts/sort_imports.hs" flags

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
    -- removeFilesAfter is only performed on a successfull build, this file
    -- needs to be cleaned regardless otherwise successive builds will fail
    liftIO $ removeFiles "." ["kontrakcja-test.tix"]
    tc <- askOracle (TeamCity ())
    if tc
      then do
        target <- askOracle (BuildTarget ())
        let cmdopt = [Shell] ++ langEnv
            flags = ["--plain","--output-dir _build/kontrakcja-test-artefacts"]
              ++ case target of
                      "staging" -> ["--staging-tests"]
                      _ -> []
        (Exit c, Stdouterr out) <- command cmdopt
                                   "./dist/build/kontrakcja-test/kontrakcja-test"
                                   flags
        liftIO $ hUnitForTeamCity c out
      else do
        cmd "./dist/build/kontrakcja-test/kontrakcja-test"
    testCoverage <- askOracle (BuildTestCoverage ())
    when testCoverage $ do
      putNormal "Checking if kontrakcja-test.tix was generated by tests..."
      need ["kontrakcja-test.tix"]
      command_ [Shell]
        ("ls test/src | awk -F \".\" '{print \"--exclude=\"$1}' "
         ++ "| hpc markup `xargs echo` "
         ++ "--destdir=coverage-reports kontrakcja-test.tix"
        ) []
      command_ [Shell]
        ("zip -r _build/coverage-reports.zip "
         ++ "coverage-reports kontrakcja-test.tix") []
      removeFilesAfter "coverage-reports" ["//*"]

needServerHaskellFiles :: Action ()
needServerHaskellFiles = needPatternsInDirectories ["//*.hs"] sourceDirsFromCabal

-- | All HS source directories from kontrakcja.cabal For
-- `needServerHaskellFiles` we could do with top-level directories,
-- but our sort imports procedure needs the directories as defined in
-- .cabal file as it uses this information to decide if an import is
-- foreign or not
--
-- FIXME This can be automated this using grep, ack, sort, uniq on
-- kontrakcja.cabal:
--
-- grep 'hs-source-dirs' -i kontrakcja.cabal
-- | sed -e 's/Hs-source-dirs://g' -e 's/^ *//g' | tr ' ' '\n' | sort -u
sourceDirsFromCabal :: [FilePath]
sourceDirsFromCabal =
  [ "backend/cron/inc"
  , "backend/cron/schema"
  , "backend/cron/src"
  , "backend/lib"
  , "backend/mailer/inc"
  , "backend/mailer/schema"
  , "backend/mailer/src"
  , "backend/messenger/inc"
  , "backend/messenger/schema"
  , "backend/messenger/src"
  , "backend/migrate/inc"
  , "backend/migrate/src"
  , "backend/misc/schema"
  , "backend/misc/src"
  , "backend/server/schema"
  , "backend/server/src"
  , "backend/test/src"
  , "localization/src"
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
         , "dist/setup-config" -- Need `cabal configure` as Grunt uses
                               -- localization
         ]
    alwaysRerun
    withGitHub "Grunt Build" $
      command ([EchoStdout True, Cwd "frontend"] ++ langEnv) "grunt" ["build"]

  "grunt-clean" ~> do
    need ["_build/npm-install"]
    cmd (Cwd "frontend") "grunt clean"

-- | Frontend test rules
frontendTestRules :: Rules ()
frontendTestRules = do
  "grunt-eslint" ~> do
    need ["_build/npm-install"]
    withGitHub "eslint" $
      cmd (Cwd "frontend") "grunt eslint"

  "grunt-coverage" ~> do
    need ["_build/grunt-build"]
    command_ [Cwd "frontend"] "grunt" ["test"]

  "grunt-test" ~> do
    need ["_build/grunt-build"]
    withGitHub "Grunt Test" $ do
      testCoverage <- askOracle (BuildTestCoverage ())
      case testCoverage of
        False -> cmd (Cwd "frontend") "grunt test:fast"
        True -> do
          need ["grunt-coverage"]
          command_ [Shell] "zip -r _build/JS_coverage.zip frontend/coverage/" []
          removeFilesAfter "frontend/coverage" ["//*"]

-- * Create distribution
distributionRules :: Rules ()
distributionRules = do
  "urls.txt" %> \_ -> do
    tc <- askOracle (TeamCity ())
    nginxconfpath <- askOracle (NginxConfPath ())
    when (not tc) $ do
      putLoud $ "ERROR: routinglist executable is only built "
        ++ "with Shake when running from TeamCity"
      liftIO $ exitFailure
    when (null nginxconfpath) $ do
      putLoud "ERROR: NGINX_CONF_PATH is empty"
      liftIO $ exitFailure
    need ["_build/cabal-build"]
    command_ [] "./dist/build/routinglist/routinglist" [nginxconfpath]
    removeFilesAfter "." ["urls.txt"]

  "_build/kontrakcja.tar.gz" %> \_ -> do
    need ["all", "urls.txt"]
    let distFiles = [ "dist/build/kontrakcja-server/kontrakcja-server"
                    , "dist/build/cron/cron"
                    , "dist/build/kontrakcja-migrate/kontrakcja-migrate"
                    , "dist/build/mailing-server/mailing-server"
                    , "dist/build/messenger-server/messenger-server"
                    , "dist/build/screenshot-review/screenshot-review"
                    , "evidence-package/samples.p"
                    , "frontend/app/img"
                    , "frontend/app/less"
                    , "frontend/dist"
                    , "scrivepdftools"
                    , "GuardTime"
                    , "templates"
                    , "files"
                    , "texts"
                    , "urls.txt"
                    , "build-scripts/deployDevNginxRules.sh"
                    , "certs"
                    ]
    command_ [Shell] "tar" $ ["-czf","_build/kontrakcja.tar.gz"] ++ distFiles

-- * Utility scripts.

utilityScriptRules :: Rules ()
utilityScriptRules = do
  "dist/build/detect_old_localizations/detect_old_localizations" <.> exe %> \_ -> do
    let detectOldLocalizationsSources = ["scripts/detect_old_localizations.hs"]
    need $ ["dist/setup-config"] ++ detectOldLocalizationsSources
    cmd "cabal build detect_old_localizations"

  "dist/build/detect_old_templates/detect_old_templates" <.> exe %> \_ -> do
    let detectOldTemplatesSources = ["scripts/detect_old_templates.hs"]
    transifexSources             <- getTransifexSources
    need $ ["dist/setup-config"] ++ detectOldTemplatesSources ++ transifexSources
    cmd "cabal build detect_old_templates"

  "dist/build/transifex/transifex" <.> exe %> \_ -> do
    transifexSources <- getTransifexSources
    need $ ["dist/setup-config"] ++ transifexSources
    cmd "cabal build transifex"

  "scripts-help" ~> putNormal scriptsUsageMsg

  where
    -- FIXME: Use Cabal API to get the list of hs-source-dirs/modules
    -- from the .cabal file instead of duplicating the info here.
    getTransifexSources = getDirectoryFiles "" ["transifex//*.hs"]

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
  , "Required arguments: --user, --password, --lang, --resource."
  , ""
  , "transifex-diff"
  , "-------------"
  , "Diff local translation against the version on the server."
  , "To use, run 'shake.sh transifex-diff ARGS'."
  , "Required arguments: --user, --password, --lang, --resource."
  , ""
  , "transifex-merge"
  , "-------------"
  , "Merge local translation with the remote one."
  , "To use, run 'shake.sh transifex-merge ARGS'."
  , "Required arguments: --user, --password, --lang, --resource."
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
  , "After running 'shake.sh take-reference-screenshots', do the following steps:"
  , ""
  -- TODO: Explain the process better.
  , "x = concatLines base64 /tmp/author.png"
  , "authorJson = {\"time\":str(now()), in format 2015-09-22T16:00:00Z"
  , "              \"image\":\"data:image/jpeg;base64,\" + x}"
  , "writeFile files/reference_screenshots/author.json (dumps(authorJson))"
  , "same thing for /tmp/desktop2.png into "
    ++ "files/reference_screenshots/standard.json"
  , "same thing for /tmp/mobile2.png into files/reference_screenshots/mobile.json"

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
  , " ./transifex.sh diff-lang user password en"
  , ""
  , "And check if response looks reasonable. If there are 500 texts"
  , "changed, it's bad, but 50 are ok. Now push your local source texts"
  , "to transifex. This will make all English texts available to"
  , "translators."
  , ""
  , "./transifex.sh push-lang user password en"
  , ""
  , "You should never do this with any other language, since it will"
  , "overwrite translations. And translations should be done in TX, not"
  , "in sources."
  , ""
  , "Now it's time to fetch stuff from TX. You do that with"
  , "./transifex.sh merge-all user password"
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

runTransifexScript :: String -> [TransifexFlag] -> Action ()
runTransifexScript subcommand flags = do
  checkFlags
  need ["dist/build/transifex/transifex" <.> exe]
  let extractArg (User u)     = u
      extractArg (Password p) = p
      extractArg (Lang l)     = l
      extractArg (Resource r) = r
      args = map extractArg $ sort flags
  cmd $ "dist/build/transifex/transifex " ++ subcommand
        ++ " " ++ intercalate " " args
    where
      checkFlags = if (subcommand `elem` ["push", "diff", "merge"])
                      && (length flags /= 4)
                   then error "transifex: Some required arguments are missing!"
                   else return ()

runDetectOldLocalizationsScript :: Action ()
runDetectOldLocalizationsScript = do
  need ["dist/build/detect_old_localizations/detect_old_localizations" <.> exe]
  cmd "dist/build/detect_old_localizations/detect_old_localizations"

runDetectOldTemplatesScript :: Action ()
runDetectOldTemplatesScript = do
  need ["dist/build/detect_old_templates/detect_old_templates" <.> exe]
  cmd "dist/build/detect_old_templates/detect_old_templates"

runTakeReferenceScreenshotsScript :: Action ()
runTakeReferenceScreenshotsScript = do
  need ["scripts/take_reference_screenshots.py"]
  cmd "python scripts/take_reference_screenshots.py"

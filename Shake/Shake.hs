{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.List
import Data.Maybe
import Extra
import Development.Shake
import Development.Shake.FilePath
import System.Console.GetOpt
import System.Exit (exitFailure)
import System.Info (os, arch)
import System.Process (readProcess)
import qualified Data.Map as M

import Shake.GetCabalDeps
import Shake.GetHsDeps
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
  , "   localization               : Update localization"
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

data ShakeFlag = TransifexUser String
               | TransifexPassword String
               | TransifexLang String
               | TransifexResource String
               | NewBuild
  deriving (Eq, Ord)

shakeFlags :: [OptDescr (Either String ShakeFlag)]
shakeFlags =
  [ Option "" ["user"]      (reqArg TransifexUser     "USER") "User name"
  , Option "" ["password"]  (reqArg TransifexPassword "PASS") "Password"
  , Option "" ["lang"]      (reqArg TransifexLang     "LANG") "Language"
  , Option "" ["resource"]  (reqArg TransifexResource "NAME") "Resource"
  , Option "" ["new-build"] (noArg  NewBuild)                 "Use 'new-build'."
  ]
  where
    noArg  flagVal     = NoArg  (Right flagVal)
    reqArg toFlag name = ReqArg (Right . toFlag) name

data UseNewBuild = UseNewBuild FilePath -- ^ New-build build dir.
                 | DontUseNewBuild

useNewBuild :: UseNewBuild -> Bool
useNewBuild (UseNewBuild _) = True
useNewBuild DontUseNewBuild = False

mkUseNewBuild :: [ShakeFlag] -> IO UseNewBuild
mkUseNewBuild flags = if NewBuild `elem` flags
  then do ghcVer <- trim <$> readProcess "ghc" ["--numeric-version"] ""
          return . UseNewBuild $ "dist-newstyle" </> "build"
            </> (arch ++ "-" ++ os)
            </> ("ghc-" ++ ghcVer) </> "kontrakcja-1.0"
  else return DontUseNewBuild

ifNewBuild :: UseNewBuild -> (FilePath -> m a) -> m a -> m a
ifNewBuild (UseNewBuild buildDir) act0 _act1 = act0 buildDir
ifNewBuild DontUseNewBuild       _act0  act1 = act1

main :: IO ()
main = do
  -- Used to check if Shake.hs rules changed, triggering a full rebuild.
  hsDeps       <- getHsDeps "Shake/Shake.hs"
  ver          <- getHashedShakeVersion $ ["shake.sh"] ++ hsDeps
  -- Dependency information needed by our rules.
  hsSourceDirs <- getHsSourceDirs "kontrakcja.cabal"
  shakeArgsWith (opts ver) shakeFlags $ \flags targets -> return . Just $ do
    newBuild <- liftIO $ mkUseNewBuild flags

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

    "transifex-fix"              ~> runTransifexScript newBuild "fix"   []
    "transifex-push"             ~> runTransifexScript newBuild "push"  flags
    "transifex-diff"             ~> runTransifexScript newBuild "diff"  flags
    "transifex-merge"            ~> runTransifexScript newBuild "merge" flags
    "transifex-help"             ~> do putNormal transifexUsageMsg
                                       putNormal "-----------------------------"
                                       putNormal "Output of 'transifex --help':"
                                       putNormal "-----------------------------"
                                       runTransifexScript newBuild "" []
    "detect-old-localizations"   ~> runDetectOldLocalizationsScript newBuild
    "detect-old-templates"       ~> runDetectOldTemplatesScript newBuild
    "take-reference-screenshots" ~> runTakeReferenceScreenshotsScript
    "localization"               ~> runLocalization newBuild
    "scripts-help"               ~> putNormal scriptsUsageMsg

    "clean" ~> need ["clean-server","clean-frontend"]
    "clean-server" ~> need ["cabal-clean"]
    "clean-frontend" ~> need ["grunt-clean"]
    "fresh" ~> do
      need ["clean"]
      removeFilesAfter "_build/" ["//*"]
      removeFilesAfter "_shake/" ["//*"]

    -- * Rules
    componentBuildRules newBuild hsSourceDirs
    serverBuildRules    newBuild hsSourceDirs
    serverTestRules     newBuild hsSourceDirs
    frontendBuildRules  newBuild
    frontendTestRules
    distributionRules   newBuild
    oracleHelpRule

-- * Server

-- | Server build rules
serverBuildRules :: UseNewBuild -> HsSourceDirsMap -> Rules ()
serverBuildRules newBuild hsSourceDirs =
  ifNewBuild newBuild (serverNewBuildRules hsSourceDirs)
                      (serverOldBuildRules hsSourceDirs)

getCabalConfigureFlags :: Action [String]
getCabalConfigureFlags = do
  tc           <- askOracle (TeamCity ())
  testCoverage <- askOracle (BuildTestCoverage ())
  flags0       <- askOracle (BuildCabalConfigureOptions ())
  let flags = if tc then ["-fenable-routinglist",flags0]
              else [flags0]
      flags'= if testCoverage then "-ftest-coverage":flags
              else flags
  return flags'

serverNewBuildRules :: HsSourceDirsMap -> FilePath -> Rules ()
serverNewBuildRules hsSourceDirs buildDir = do
  let cabalFiles = ["cabal.project.freeze", "kontrakcja.cabal"]

  "_build/cabal-update" %>>> do
    need cabalFiles
    cmd (EchoStdout True) "cabal update"

  "cabal.project.local" %> \_ -> do
    need ("_build/cabal-update":cabalFiles)
    flags <- getCabalConfigureFlags
    command [Shell] "cabal" ("new-configure":flags)

  "_build/cabal-build" %>>> do
    need ["cabal.project.local"]
    tc <- askOracle (TeamCity ())
    when tc $ do
      -- Need to rebuild on TeamCity because version code for
      -- resources that is generated in src/Version.hs is used for stuff
      -- that is rebuilt with new version code by `grunt build`.
      alwaysRerun
      -- Force GHC to rebuild the TH-containing module.
      cmd "touch" ("backend" </> "lib" </> "Version.hs")
    needServerHaskellFiles hsSourceDirs
    cmd (EchoStdout True) "cabal new-build"

  "_build/cabal-haddock.tar.gz" %> \_ -> do
    need ["_build/cabal-build"]
    needServerHaskellFiles hsSourceDirs
    -- Limit to library from package, due to Cabal bug:
    -- https://github.com/haskell/cabal/issues/1919
    command_ [] "cabal" ["act-as-setup", "--", "haddock", "--internal"
                        ,"--builddir", buildDir]
    command_ [Shell] "tar" ["-czf","_build/cabal-haddock.tar.gz"
                           ,buildDir </> "doc"]

  "cabal-clean" ~> cmd "rm -rf dist-newstyle"


serverOldBuildRules :: HsSourceDirsMap -> Rules ()
serverOldBuildRules hsSourceDirs = do
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
    -- Need to rebuild on TeamCity because version code for resources
    -- that is generated in src/Version.hs is used for stuff that is
    -- rebuilt with new version code by `grunt build`.
    when tc $ alwaysRerun
    -- Need to clean for flags to be effective.
    command_ [] "cabal" ["clean"]

    flags <- getCabalConfigureFlags
    command [Shell] "cabal" ("configure":flags)

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
    needServerHaskellFiles hsSourceDirs
    cmd (EchoStdout True) "cabal build"

  "_build/cabal-haddock.tar.gz" %> \_ -> do
    need ["_build/cabal-build"]
    needServerHaskellFiles hsSourceDirs
    -- Limit to library from package, due to Cabal bug:
    -- https://github.com/haskell/cabal/issues/1919
    command_ [] "cabal" ["haddock","--internal"]
    command_ [Shell] "tar" ["-czf","_build/cabal-haddock.tar.gz","dist/doc"]

  "cabal-clean" ~> cmd "cabal clean"

-- | Server test rules
serverTestRules :: UseNewBuild -> HsSourceDirsMap -> Rules ()
serverTestRules newBuild hsSourceDirs = do
  let hsImportOrderAction checkOnly = do
        let allSourceDirs = allHsSourceDirs hsSourceDirs
            flags = if checkOnly then ("--check":allSourceDirs)
                    else allSourceDirs
        command ([Shell] ++ langEnv)
          (componentTargetPath newBuild "sort_imports") flags

  "_build/hs-import-order" %>>> do
    needServerHaskellFiles hsSourceDirs
    need [componentTargetPath newBuild "sort_imports"]
    withGitHub "Haskell import order" $ hsImportOrderAction True

  "fix-hs-import-order" ~> do
    need [componentTargetPath newBuild "sort_imports"]
    hsImportOrderAction False

  "kontrakcja_test.conf" %> \_ -> do
    tc <- askOracle (TeamCity ())
    when tc $ do
      testConfFile <- askOracle (BuildTestConfPath ())
      copyFile' testConfFile "kontrakcja_test.conf"

  "kontrakcja-test" ~> do
    let kontrakcjaTestPath = componentTargetPath newBuild "kontrakcja-test"
    need [kontrakcjaTestPath, "kontrakcja_test.conf"]
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
                                   kontrakcjaTestPath
                                   flags
        liftIO $ hUnitForTeamCity c out
      else do
        cmd kontrakcjaTestPath
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

needServerHaskellFiles :: HsSourceDirsMap -> Action ()
needServerHaskellFiles hsSourceDirs =
  needPatternsInDirectories ["//*.hs"] (allHsSourceDirs hsSourceDirs)

-- * Frontend

-- | Frontend build rules
frontendBuildRules :: UseNewBuild -> Rules ()
frontendBuildRules newBuild = do
  "_build/npm-install" %>>> do
    need ["frontend/package.json"]
    command [EchoStdout True, Cwd "frontend"] "npm" ["install"]

  "_build/grunt-build" %>>> do
    need [ "_build/npm-install"
         , componentTargetPath newBuild "localization"
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
distributionRules :: UseNewBuild -> Rules ()
distributionRules newBuild = do
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
    let routingListPath = componentTargetPath newBuild "routinglist"
    need [routingListPath]
    command_ [] routingListPath [nginxconfpath]
    removeFilesAfter "." ["urls.txt"]

  "_build/kontrakcja.tar.gz" %> \_ -> do
    need ["all", "urls.txt"]
    let distFiles = [ componentTargetPath newBuild "kontrakcja-server"
                    , componentTargetPath newBuild "cron"
                    , componentTargetPath newBuild "kontrakcja-migrate"
                    , componentTargetPath newBuild "mailing-server"
                    , componentTargetPath newBuild "messenger-server"
                    , componentTargetPath newBuild "screenshot-review"
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

-- | For each exe/test-suite/benchmark component in the .cabal file,
-- add a rule for building the corresponding executable.
componentBuildRules :: UseNewBuild -> HsSourceDirsMap -> Rules ()
componentBuildRules newBuild hsSourceDirs = do
  forM_ (M.keys hsSourceDirs) $ \componentName ->
    if componentName /= ""
    then (componentTargetPath newBuild componentName) %> \_ ->
      -- Assumes that all sources of a component are in its hs-source-dirs.
      do let sourceDirs = componentHsSourceDirs componentName hsSourceDirs
         need ["dist/setup-config"]
         needPatternsInDirectories ["//*.hs"] sourceDirs
         if useNewBuild newBuild
           then cmd $ "cabal new-build " ++ componentName
           else cmd $ "cabal build " ++ componentName
    else return ()

-- | Given a component name, return the path to the corresponding
-- executable.
componentTargetPath :: UseNewBuild -> String -> FilePath
componentTargetPath DontUseNewBuild componentName =
  "dist" </> "build" </> componentName </> componentName <.> exe
componentTargetPath (UseNewBuild buildDir) componentName =
  buildDir </> "c" </> componentName </> "build"
           </> componentName </> componentName <.> exe

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
  , ""
  , "localization"
  , "--------------------------"
  , "Update the localization"
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

runTransifexScript :: UseNewBuild -> String -> [ShakeFlag] -> Action ()
runTransifexScript newBuild subcommand flags = do
  let scriptPath = componentTargetPath newBuild "transifex"
  checkFlags
  need [scriptPath]
  let extractArg (TransifexUser u)     = Just u
      extractArg (TransifexPassword p) = Just p
      extractArg (TransifexLang l)     = Just l
      extractArg (TransifexResource r) = Just r
      extractArg (NewBuild)            = Nothing
      -- NB: sorting here relies on ShakeFlag constructors being
      -- declared in correct order (user, password, lang, resource).
      args = mapMaybe extractArg $ sort flags
  cmd scriptPath (subcommand:args)
    where
      checkFlags = if (subcommand `elem` ["push", "diff", "merge"])
                      && (length flags /= 4)
                   then error "transifex: Some required arguments are missing!"
                   else return ()

runDetectOldLocalizationsScript :: UseNewBuild -> Action ()
runDetectOldLocalizationsScript newBuild = do
  let scriptPath = componentTargetPath newBuild "detect_old_localizations"
  need [scriptPath]
  cmd scriptPath

runDetectOldTemplatesScript :: UseNewBuild -> Action ()
runDetectOldTemplatesScript newBuild = do
  let scriptPath = componentTargetPath newBuild "detect_old_templates"
  need [scriptPath]
  cmd scriptPath

runTakeReferenceScreenshotsScript :: Action ()
runTakeReferenceScreenshotsScript = do
  need ["scripts/take_reference_screenshots.py"]
  cmd "python scripts/take_reference_screenshots.py"

runLocalization :: UseNewBuild -> Action ()
runLocalization newBuild = do
  let exePath = componentTargetPath newBuild "localization"
  need [exePath]
  cmd  exePath

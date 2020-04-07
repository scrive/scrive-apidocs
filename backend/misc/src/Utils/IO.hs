module Utils.IO ( checkExecutables
                , readCurl
                , waitForTermination
                )
where

import Control.Concurrent
import Control.Monad.Base
import Data.Either
import Log
import System.Directory (findExecutable)
import System.Exit
import System.Posix.IO (stdInput)
import System.Posix.Signals
import System.Posix.Terminal (queryTerminal)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)
import qualified Data.Text as T

-- | Wait for a signal (sigINT or sigTERM).
waitForTermination :: IO ()
waitForTermination = do
  istty <- queryTerminal stdInput
  mv    <- newEmptyMVar
  void $ installHandler softwareTermination (CatchOnce (putMVar mv ())) Nothing
  when istty $ do
    void $ installHandler keyboardSignal (CatchOnce (putMVar mv ())) Nothing
  takeMVar mv

curlExe :: String
curlExe = "curl"

-- | This function executes curl as external program. Args are args.
readCurl
  :: MonadBase IO m
  => [String]                 -- ^ any arguments
  -> BSL.ByteString           -- ^ standard input
  -> m (ExitCode, BSL.ByteString, BSL.ByteString) -- ^ exitcode, stdout, stderr
readCurl args input = liftBase
  $ readProcessWithExitCode curlExe (["--max-time", "60", "-s", "-S"] ++ args) input

checkExecutables :: forall  m . (MonadLog m, MonadBase IO m, Functor m) => m ()
checkExecutables =
  logInfo "Checking paths to executables:"
    .   object
    =<< mapM logFullPathAndVersion
    =<< checkMissing
    =<< mapM findExe importantExecutables
  where
    findExe :: (Text, [String]) -> m (Either Text (Text, [String], FilePath))
    findExe (name, options) =
      maybe (Left name) (Right . (name, options, ))
        <$> (liftBase . findExecutable . T.unpack $ name)

    checkMissing
      :: [Either Text (Text, [String], FilePath)] -> m [(Text, [String], FilePath)]
    checkMissing eithers = do
      let (missing, present) = partitionEithers eithers
      if null missing
        then return present
        else do
          logAttention "Not all important executables are present"
            $ object ["executables" .= missing]
          liftBase exitFailure

    logFullPathAndVersion :: (Text, [String], FilePath) -> m Aeson.Pair
    logFullPathAndVersion (name, options, fullpath) | null options =
      return $ name .= fullpath
    logFullPathAndVersion (name, options, fullpath) = do
      ver <- liftBase $ readExecutableVersion fullpath options
      return $ name .= (fullpath : lines ver)

    readExecutableVersion :: FilePath -> [String] -> IO String
    readExecutableVersion path options = do
      (_code', stdout', stderr') <- readProcessWithExitCode path options BSL.empty
      return $ BSL.toString stdout' ++ BSL.toString stderr'

    importantExecutables :: [(Text, [String])]
    importantExecutables =
      [ ("java"     , ["-version"])
      , ("curl"     , ["-V"])
      , ("mutool"   , ["-v"])
      , ("pngquant" , ["--version"])
      , ("convert"  , ["--version"])
      , ("identify" , ["--version"])
      , ("lessc"    , ["-v"])
      , ("gnuplot"  , ["--version"])
      , ("pdfdetach", ["-v"])
      , ("qrencode" , ["--version"])
      , ("xmlsec1"  , ["--version"])
      , ("psql"     , ["--version"])
      ]

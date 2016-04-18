module Utils.IO where

import Control.Concurrent
import Control.Monad.Base
import Log
import System.Exit
import System.IO
import System.Posix.IO (stdInput)
import System.Posix.Signals
import System.Posix.Terminal (queryTerminal)
import System.Process.ByteString.Lazy (readProcessWithExitCode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)
import qualified Data.Text as T

import KontraPrelude

-- | Wait for a signal (sigINT or sigTERM).
waitForTermination :: IO ()
waitForTermination = do
  istty <- queryTerminal stdInput
  mv <- newEmptyMVar
  _ <- installHandler softwareTermination (CatchOnce (putMVar mv ())) Nothing
  when istty $ do
    _ <- installHandler keyboardSignal (CatchOnce (putMVar mv ())) Nothing
    return ()
  takeMVar mv

curl_exe :: String
curl_exe = "curl"

-- | This function executes curl as external program. Args are args.
readCurl :: MonadBase IO m
         => [String]                 -- ^ any arguments
         -> BSL.ByteString           -- ^ standard input
         -> m (ExitCode, BSL.ByteString, BSL.ByteString) -- ^ exitcode, stdout, stderr
readCurl args input = liftBase $ readProcessWithExitCode curl_exe (["--max-time", "10", "-s", "-S"] ++ args) input

checkPathToExecutable :: FilePath -> IO FilePath
checkPathToExecutable filepath = do
    (_code',stdout',_stderr') <- readProcessWithExitCode "which" [filepath] (BSL.empty)
    return $ BSL.toString stdout'

checkExecutableVersion :: FilePath -> [String] -> IO String
checkExecutableVersion path options = do
    (_code',stdout',stderr') <- readProcessWithExitCode path options (BSL.empty)
    return $ BSL.toString stdout' ++ BSL.toString stderr'


importantExecutables :: [(T.Text, [String])]
importantExecutables =
  [ ("java", ["-version"])
  , ("curl", ["-V"])
  , ("stdbuf", [])
  , ("mutool", [])
  , ("convert", [])
  , ("lessc", ["-v"])
  , ("gnuplot", ["--version"])
  ]

checkExecutables :: (MonadLog m, MonadBase IO m, Functor m) => m ()
checkExecutables = logInfo "Checking paths to executables:" . object
  =<< mapM check (sort importantExecutables)
  where
    check (filepath, options) = do
      realpathlines <- lines `fmap` (liftBase $ checkPathToExecutable $ T.unpack filepath)
      case realpathlines of
        [] -> return $ filepath .= ("*** not found ***"::T.Text)
        (realpath:_) -> if null options
          then return $ filepath .= realpath
          else do
            ver <- liftBase $ checkExecutableVersion realpath options
            return $ filepath .= (realpath : lines ver)

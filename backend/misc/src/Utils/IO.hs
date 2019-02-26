module Utils.IO ( checkExecutables
                , readCurl
                , sftpTransfer
                , waitForTermination
                )
where

import Control.Concurrent
import Control.Monad.Base
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

import SFTPConfig

-- | Wait for a signal (sigINT or sigTERM).
waitForTermination :: IO ()
waitForTermination = do
  istty <- queryTerminal stdInput
  mv <- newEmptyMVar
  void $ installHandler softwareTermination (CatchOnce (putMVar mv ())) Nothing
  when istty $ do
    void $ installHandler keyboardSignal (CatchOnce (putMVar mv ())) Nothing
    return ()
  takeMVar mv

curl_exe :: String
curl_exe = "curl"

-- | This function executes curl as external program. Args are args.
readCurl
  :: MonadBase IO m
  => [String]                 -- ^ any arguments
  -> BSL.ByteString           -- ^ standard input
  -> m (ExitCode, BSL.ByteString, BSL.ByteString) -- ^ exitcode, stdout, stderr
readCurl args input = liftBase $
  readProcessWithExitCode curl_exe
  (["--max-time", "60", "-s", "-S"] ++ args) input

sftpTransfer
  :: (MonadBase IO m)
  => SFTPConfig
  -> FilePath
  -> m (ExitCode, BSL.ByteString, BSL.ByteString)
sftpTransfer SFTPConfig{..} filePath = do
  -- We want the directory specified to actually be interpreted as a
  -- directory and not as a file.
  let sftpRemoteDir' = sftpRemoteDir <> if (last sftpRemoteDir /= '/')
                                        then "/" else ""
  readCurl
    (concat [ ["-T", filePath]
            , ["sftp://" <> sftpUser <> ":" <> sftpPassword <> "@" <>
                sftpHost <> sftpRemoteDir']
            ])
    BSL.empty

checkExecutables :: forall m . (MonadLog m, MonadBase IO m, Functor m) => m ()
checkExecutables = logInfo "Checking paths to executables:" . object
                   =<< mapM check (sort importantExecutables)
  where
    check :: (T.Text, [String]) -> m Aeson.Pair
    check (filepath, options) =
      checkFullExePath =<<
      (liftBase . findExecutable . T.unpack $ filepath)
      where
        checkFullExePath Nothing = do
          logAttention "Not all important executables are present" $ object [
            "executable" .= show filepath
            ]
          liftBase exitFailure
        checkFullExePath (Just fullpath) | null options
          = return $ filepath .= fullpath
        checkFullExePath (Just fullpath) | otherwise
          = do ver <- liftBase $ readExecutableVersion fullpath options
               return $ filepath .= (fullpath : lines ver)

    readExecutableVersion :: FilePath -> [String] -> IO String
    readExecutableVersion path options = do
      (_code',stdout',stderr') <-
        readProcessWithExitCode path options (BSL.empty)
      return $ BSL.toString stdout' ++ BSL.toString stderr'

    importantExecutables :: [(T.Text, [String])]
    importantExecutables =
      [ ("java",      ["-version"])
      , ("curl",      ["-V"])
      , ("mutool",    ["-v"])
      , ("pngquant",  ["--version"])
      , ("convert",   ["--version"])
      , ("identify",  ["--version"])
      , ("lessc",     ["-v"])
      , ("gnuplot",   ["--version"])
      , ("pdfdetach", ["-v"])
      , ("qrencode",  ["--version"])
      , ("xmlsec1",   ["--version"])
      ]

module Utils.IO where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.List (sort)
import System.Exit
import System.IO
import System.IO.Temp
import System.Posix.IO (stdInput)
import System.Posix.Signals
import System.Posix.Terminal (queryTerminal)
import System.Process
import Text.JSON.Gen
import qualified Control.Exception.Lifted as C
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL (toString)

import qualified Log

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

-- | Create an external process with arguments. Feed it input, collect
-- exit code, stdout and stderr.
--
-- Standard input is first written to a temporary file. GHC 6.12.1
-- seemed to have trouble doing multitasking when writing to a slow
-- process like curl upload.
readProcessWithExitCode'
    :: MonadIO m
    => FilePath                                      -- ^ command to run
    -> [String]                                      -- ^ any arguments
    -> BSL.ByteString                                -- ^ standard input
    -> m (ExitCode, BSL.ByteString, BSL.ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' cmd args input = liftIO $ do

    (Just inh, Just outh, Just errh, pid) <- createProcess (proc cmd args) {
        std_in  = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
    }

    outM <- newEmptyMVar
    errM <- newEmptyMVar

    -- fork so that if process decides to not read everything we are
    -- not stuck
    _ <- forkIO $ do
      BSL.hPutStr inh input
      hClose inh

    -- fork off a thread to start consuming stdout
    _ <- forkIO $ do
      out <- BSL.hGetContents outh
      _ <- C.evaluate (BSL.length out)
      putMVar outM out
      -- here handle should get garbage collected but to close it
      -- faster we close explicitly
      hClose outh

    -- fork off a thread to start consuming stderr
    _ <- forkIO $ do
      err  <- BSL.hGetContents errh
      _ <- C.evaluate (BSL.length err)
      putMVar errM err
      hClose errh


    -- wait on the output
    out <- readMVar outM
    err <- readMVar errM

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)

-- | Create an external process with arguments. Feed it input, collect
-- exit code, stdout and stderr.
--
-- Standard input is first written to a temporary file. GHC 6.12.1
-- seemed to have trouble doing multitasking when writing to a slow
-- process like curl upload.
readProcessWithExitCodeOldWay'
    :: MonadIO m
    => FilePath                                      -- ^ command to run
    -> [String]                                      -- ^ any arguments
    -> BSL.ByteString                                -- ^ standard input
    -> m (ExitCode, BSL.ByteString, BSL.ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCodeOldWay' cmd args input = liftIO $
  withSystemTempFile "process" $ \_inputname inputhandle -> do
    BSL.hPutStr inputhandle input
    hFlush inputhandle
    hSeek inputhandle AbsoluteSeek 0

    (_, Just outh, Just errh, pid) <- createProcess (proc cmd args) {
        std_in  = UseHandle inputhandle
      , std_out = CreatePipe
      , std_err = CreatePipe
    }
    outMVar <- newEmptyMVar

    outM <- newEmptyMVar
    errM <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    _ <- forkIO $ do
      out <- BSL.hGetContents outh
      _ <- C.evaluate (BSL.length out)
      putMVar outM out
      putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    _ <- forkIO $ do
      err  <- BSL.hGetContents errh
      _ <- C.evaluate (BSL.length err)
      putMVar errM err
      putMVar outMVar ()

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    C.handle ((\_e -> return ()) :: (C.IOException -> IO ())) $ hClose outh
    C.handle ((\_e -> return ()) :: (C.IOException -> IO ())) $ hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    out <- readMVar outM
    err <- readMVar errM

    return (ex, out, err)

curl_exe :: String
curl_exe = "curl"

-- | This function executes curl as external program. Args are args.
readCurl :: MonadIO m
         => [String]                 -- ^ any arguments
         -> BSL.ByteString           -- ^ standard input
         -> m (ExitCode, BSL.ByteString, BSL.ByteString) -- ^ exitcode, stdout, stderr
readCurl args input = readProcessWithExitCodeOldWay' curl_exe (["--max-time", "10", "-s", "-S"] ++ args) input

checkPathToExecutable :: FilePath -> IO FilePath
checkPathToExecutable filepath = do
    (_code',stdout',_stderr') <- readProcessWithExitCode' "which" [filepath] (BSL.empty)
    return $ BSL.toString stdout'

checkExecutableVersion :: FilePath -> [String] -> IO String
checkExecutableVersion path options = do
    (_code',stdout',stderr') <- readProcessWithExitCode' path options (BSL.empty)
    return $ BSL.toString stdout' ++ BSL.toString stderr'


importantExecutables :: [(FilePath,[String])]
importantExecutables =
  [ ("java", ["-version"])
  , ("curl", ["-V"])
  , ("mutool", [])
  , ("mudraw", [])
  , ("convert", [])
  , ("lessc", ["-v"])
  ]

checkExecutables :: (Log.MonadLog m, MonadIO m, Functor m) => m ()
checkExecutables = do
  Log.mixlogt "Checking paths to executables:" $ do
    mapM_ check (sort importantExecutables)
  where
    check (filepath,options) = do
      realpathlines <- lines `fmap` (liftIO $ checkPathToExecutable filepath)
      case realpathlines of
        [] -> value filepath ("*** not found ***" :: String)
        (realpath:_) -> do
            if null options
               then value filepath realpath
               else do
                  ver <- liftIO $ checkExecutableVersion realpath options
                  value filepath (realpath : lines ver)

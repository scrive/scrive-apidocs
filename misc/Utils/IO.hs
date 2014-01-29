module Utils.IO where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.Exit
import System.IO
import System.IO.Temp
import System.Posix.Signals
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)
import System.Process
import qualified Control.Exception as C
import qualified Data.ByteString.Lazy as BSL
import Data.List (sort)
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
readProcessWithExitCode' cmd args input = liftIO $
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
readCurl args input = readProcessWithExitCode' curl_exe (["--max-time", "20", "-s", "-S"] ++ args) input

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
  ]

checkExecutables :: IO ()
checkExecutables = do
  Log.debug "Checking paths to executables:"
  mapM_ check (sort importantExecutables)
  where
    check (filepath,options) = do
      realpathlines <- lines `fmap` checkPathToExecutable filepath
      if null realpathlines
         then Log.debug $ "    " ++ filepath ++ ": *** not found ***"
         else do
            Log.debug $ "    " ++ filepath ++ ": " ++ head (realpathlines)
            when (options/=[]) $ do
              ver <- checkExecutableVersion (head realpathlines) options
              let ver2 = map ("      " ++) $ filter (/="") $ lines ver
              mapM_ Log.debug ver2

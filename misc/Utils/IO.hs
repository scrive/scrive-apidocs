module Utils.IO where

import Control.Concurrent
import Control.Monad
import System.Exit
import System.IO
import System.IO.Temp
import System.Posix.Signals
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)
import System.Process
import qualified Control.Exception as C
import qualified Data.ByteString.Lazy as BSL

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
    :: FilePath                                      -- ^ command to run
    -> [String]                                      -- ^ any arguments
    -> BSL.ByteString                                -- ^ standard input
    -> IO (ExitCode, BSL.ByteString, BSL.ByteString) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' cmd args input =
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
readCurl :: [String]                 -- ^ any arguments
         -> BSL.ByteString           -- ^ standard input
         -> IO (ExitCode, BSL.ByteString, BSL.ByteString) -- ^ exitcode, stdout, stderr
readCurl args input = readProcessWithExitCode' curl_exe args input

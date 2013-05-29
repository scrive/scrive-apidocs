#!/usr/bin/env runghc
{-# LANGUAGE ScopedTypeVariables #-}

import System.Process
import Data.Maybe
import System.Directory
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Time

executables :: [String]
executables =
  [ "dist/build/kontrakcja-server/kontrakcja-server"
  , "dist/build/mailing-server/mailing-server"
  , "dist/build/messenger-server/messenger-server"
  , "dist/build/cron/cron"
  ]

data ProcessState = ProcessState
                  { psExecutable :: String
                  , psPid :: Maybe ProcessHandle
                  , psLastModified :: Maybe ClockTime
                  }


-- Need to kill process if executable stated and mod time different.
-- Need start new process if executable stated and not running.
once :: ProcessState -> IO ProcessState
once ps = do
  eithertime <- try $ getModificationTime (psExecutable ps)
  case (eithertime, psLastModified ps, psPid ps) of
    (Left (_ :: SomeException), _, _) -> do
      -- some exception means that the file cannot be accesses
      -- or plainly does not exist. try some time later
      return $ ps { psLastModified = Nothing }
    (Right timestamp, Just lastmod, Just pid) | lastmod == timestamp -> do
          -- nothing has changed, check if process still running
          mexitcode <- getProcessExitCode pid
          case mexitcode of
            Just _ -> do
              -- process self exited, run it again
              putStrLn $ "Detected exit of " ++ (psExecutable ps)
              return (ps { psPid = Nothing })
            Nothing -> do
              return ps
    (Right timestamp, _, Just pid) -> do
          -- something has changed, we need to kill old process
          putStrLn $ "Terminating " ++ (psExecutable ps)
          terminateProcess pid
          _ <- waitForProcess pid
          putStrLn $ "Terminated " ++ (psExecutable ps)
          -- it should be dead by now, we can over loop
          return (ps { psPid = Nothing, psLastModified = Nothing })
    (Right timestamp, _, Nothing) -> do
          -- need to start process as nothing is running
          mx <- try $ createProcess (shell (psExecutable ps))
          case mx of
            Left (_ :: SomeException) -> do
              -- could not start a process
              -- try again later
              putStrLn $ "Cannot start " ++ (psExecutable ps)
              return (ps { psLastModified = Just timestamp})
            Right (_,_,_,pid) -> do
              -- our process is running, nice!
              putStrLn $ "Started " ++ (psExecutable ps)
              return (ps { psLastModified = Just timestamp, psPid = Just pid })


kill :: ProcessState -> IO ()
kill ps = case psPid ps of
            Just pid -> terminateProcess pid
            Nothing -> return ()

loop :: [ProcessState] -> IO ()
loop psx = do
  psx2 <- mapM once psx
  threadDelay 1000000
  loop psx2

main = do
  bracket (return $ map (\e -> ProcessState e Nothing Nothing) executables)
          (mapM kill)
          loop

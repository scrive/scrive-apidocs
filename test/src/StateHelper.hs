module StateHelper(
    withTestState
) where

import AppState (AppState(..))

import Happstack.Data (Proxy(..))
import Happstack.State (runTxSystem, TxControl, shutdownSystem, Saver(..))

import Control.Concurrent (MVar)
import Control.Exception (bracket,bracket_)
import System.Directory
import System.FilePath
import System.Random (randomIO)

startUp :: Saver -> IO (MVar TxControl)
startUp saver = runTxSystem saver stateProxy
    where stateProxy = Proxy :: Proxy AppState

shutdown :: MVar TxControl -> IO ()
shutdown control = shutdownSystem control

withSaver :: Saver -> IO () -> IO ()
withSaver saver action = bracket (startUp saver) (\control -> shutdown control) (\control -> action)

withFileSaver :: FilePath -> IO() -> IO()
withFileSaver dir action = withSaver (Queue (FileSaver dir)) action

--this was taken from happstack test code
-- http://patch-tag.com/r/mae/happstack/snapshot/current/content/pretty/happstack-state/tests/Happstack/State/Tests/Helpers.hs
withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action
    = do tmp <- getTemporaryDirectory
         n <- randomIO
         let dir = tmp </> (show (abs n :: Int))
         exist <- doesDirectoryExist dir
         if exist
            then withTemporaryDirectory action
            else bracket_ (createDirectoryIfMissing False dir)
                          (removeDirectoryRecursive dir)
                          (action dir)

withTestState :: IO() -> IO()
withTestState action = withTemporaryDirectory (\tmpDir -> withFileSaver tmpDir action)

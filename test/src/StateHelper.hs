{-# LANGUAGE CPP #-}

module StateHelper(
      withTestEnvironment
    , withTestDB
    , withTestState
    , withTemporaryDirectory
    ) where

import AppState
import DB.Classes

import Control.Monad.IO.Class
import Database.HDBC
import Happstack.Data (Proxy(..))
import Happstack.State (runTxSystem, TxControl, shutdownSystem, Saver(..))

import Control.Concurrent (MVar)
import System.IO.Temp
import qualified Control.Exception as E

-- create test environment
withTestEnvironment :: DBEnv -> DB () -> IO ()
withTestEnvironment env = withTestState . withTestDB env

-- pgsql database --

-- | Runs set of sql queries within one transaction and clears all tables in the end
withTestDB :: DBEnv -> DB () -> IO ()
withTestDB env f = do
  er <- ioRunDB env $ do
    er <- tryDB f
    let conn = envNexus env
    liftIO $ rollback conn
    --clearTables
    --liftIO $ commit conn
    return er
  case er of
    Right () -> return ()
    Left (e::E.SomeException) -> E.throw e

_clearTables :: DB ()
_clearTables = do
  kRunRaw "UPDATE users SET service_id = NULL, company_id = NULL"
  kRunRaw "DELETE FROM evidence_log"
  kRunRaw "DELETE FROM doc_stat_events"
  kRunRaw "DELETE FROM user_stat_events"
  kRunRaw "DELETE FROM sign_stat_events"
  kRunRaw "DELETE FROM companyinvites"

  kRunRaw "DELETE FROM author_attachments"
  kRunRaw "DELETE FROM signatory_attachments"
  kRunRaw "DELETE FROM signatory_links"
  kRunRaw "DELETE FROM documents"

  kRunRaw "DELETE FROM companies"
  kRunRaw "DELETE FROM services"
  kRunRaw "DELETE FROM users"
  kRunRaw "DELETE FROM files"

  kRunRaw "DELETE FROM mails"

-- happstack-state --

startUp :: Saver -> IO (MVar TxControl)
startUp saver = runTxSystem saver stateProxy
    where stateProxy = Proxy :: Proxy AppState

shutdown :: MVar TxControl -> IO ()
shutdown control = shutdownSystem control

withSaver :: Saver -> IO () -> IO ()
withSaver saver action =
    E.bracket (startUp saver) (\control -> shutdown control) (\_control -> action)

withFileSaver :: FilePath -> IO () -> IO ()
withFileSaver dir action = withSaver (Queue (FileSaver dir)) action

--this was taken from happstack test code
-- http://patch-tag.com/r/mae/happstack/snapshot/current/content/pretty/happstack-state/tests/Happstack/State/Tests/Helpers.hs
{-withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action
    = do tmp <- getTemporaryDirectory
         n <- randomIO
         let dir = tmp </> (show (abs n :: Int))
         exist <- doesDirectoryExist dir
         if exist
            then withTemporaryDirectory action
            else finally (action dir)  (removeDirectoryRecursive dir)-}

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory = withSystemTempDirectory "kontrakcja-test-"

withTestState :: IO () -> IO ()
withTestState action = withTemporaryDirectory (\tmpDir -> withFileSaver tmpDir action)

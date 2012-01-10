{-# LANGUAGE CPP #-}

module StateHelper(
      withTestEnvironment
    , withTestDB
    , withTestState
    , withTemporaryDirectory
    ) where

import AppState
import DB.Classes

import Happstack.Data (Proxy(..))
import Happstack.State (runTxSystem, TxControl, shutdownSystem, Saver(..))

import Control.Concurrent (MVar)
import Database.HDBC
import Database.HDBC.PostgreSQL
import System.IO.Temp
import qualified Control.Exception as E

-- create test environment
withTestEnvironment :: Connection -> DB () -> IO ()
withTestEnvironment conn = withTestState . withTestDB conn

-- pgsql database --

-- | Runs set of sql queries within one transaction and clears all tables in the end
withTestDB :: Connection -> DB () -> IO ()
withTestDB conn f = do
  er <- ioRunDB conn $ do
    er <- tryDB f
    clearTables
    return er
  case er of
    Right () -> return ()
    Left (e::E.SomeException) -> E.throw e

clearTables :: DB ()
clearTables = wrapDB $ \conn -> do
  runRaw conn "UPDATE users SET service_id = NULL, company_id = NULL"
  runRaw conn "DELETE FROM doc_stat_events"
  runRaw conn "DELETE FROM user_stat_events"
  runRaw conn "DELETE FROM sign_stat_events"  
  runRaw conn "DELETE FROM companyinvites"

#ifdef DOCUMENTS_IN_POSTGRES
  runRaw conn "DELETE FROM author_attachments"
  runRaw conn "DELETE FROM signatory_attachments"
  runRaw conn "DELETE FROM signatory_links"
  runRaw conn "DELETE FROM documents"
#endif

  runRaw conn "DELETE FROM companies"
  runRaw conn "DELETE FROM services"
  runRaw conn "DELETE FROM users"
  runRaw conn "DELETE FROM files"

  runRaw conn "DELETE FROM mails"
  return ()

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

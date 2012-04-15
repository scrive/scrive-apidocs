module StateHelper (runTestEnv) where

import AppState
import Crypto.RNG
import DB
import Misc
import TestKontra

import Control.Concurrent (MVar)
import Control.Monad.IO.Class
import Happstack.Data (Proxy(..))
import Happstack.State (runTxSystem, TxControl, shutdownSystem, Saver(..))
import qualified Control.Exception.Lifted as E

-- create test environment
runTestEnv :: (Nexus, CryptoRNGState) -> TestEnv () -> IO ()
runTestEnv (nex, rng) = runDBT nex . runCryptoRNGT rng . withTestState . withTestDB

-- pgsql database --

-- | Runs set of sql queries within one transaction and clears all tables in the end
withTestDB :: TestEnv () -> TestEnv ()
withTestDB m = E.finally m $ do
  clearTables
  dbCommit

clearTables :: TestEnv ()
clearTables = runDBEnv $ do
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

startUp :: Saver -> TestEnv (MVar TxControl)
startUp saver = liftIO $ runTxSystem saver (Proxy :: Proxy AppState)

withSaver :: Saver -> TestEnv () -> TestEnv ()
withSaver saver m = E.bracket (startUp saver) (liftIO . shutdownSystem) (const m)

withFileSaver :: FilePath -> TestEnv () ->TestEnv ()
withFileSaver dir m = withSaver (Queue (FileSaver dir)) m

withTemporaryDirectory :: (FilePath -> TestEnv a) -> TestEnv a
withTemporaryDirectory = withSystemTempDirectory' "kontrakcja-test-"

withTestState :: TestEnv () -> TestEnv ()
withTestState m = withTemporaryDirectory (\tmpDir -> withFileSaver tmpDir m)

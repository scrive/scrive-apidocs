module Doc.Extending.Consumer (
    DocumentExtendingConsumer
  , documentExtendingConsumer
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Aeson
import Data.Int
import Database.PostgreSQL.Consumers.Config
import Log.Class
import qualified Database.Redis as R

import DB
import DB.PostgreSQL
import Doc.Conditions
import Doc.DigitalSignature
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model.Query
import FileStorage
import GuardTime
import Log.Identifier
import Templates
import qualified FileStorage.Amazon.Config as A

data DocumentExtendingConsumer = DocumentExtendingConsumer {
    decDocumentID :: !DocumentID
  , decAttempts :: !Int32
  }

documentExtendingConsumer
  :: (CryptoRNG m, MonadLog m, MonadIO m, MonadBaseControl IO m, MonadMask m)
  => A.AmazonConfig
  -> GuardTimeConf
  -> KontrakcjaGlobalTemplates
  -> FileMemCache
  -> Maybe R.Connection
  -> ConnectionSourceM m
  -> Int
  -> ConsumerConfig m DocumentID DocumentExtendingConsumer
documentExtendingConsumer amazonConfig guardTimeConf templates memcache
                          mRedisConn pool maxRunningJobs = ConsumerConfig {
    ccJobsTable = "document_extending_jobs"
  , ccConsumersTable = "document_extending_consumers"
  , ccJobSelectors =
    [ "id"
    , "attempts"
    ]
  , ccJobFetcher = \(did, attempts) -> DocumentExtendingConsumer {
      decDocumentID = did
    , decAttempts = attempts
    }
  , ccJobIndex = decDocumentID
  , ccNotificationChannel = Nothing
  , ccNotificationTimeout = 60 * 1000000 -- 1 minute
  , ccMaxRunningJobs = maxRunningJobs
  , ccProcessJob = \dec ->
      -- We put handling of DocumentWasPurged exception here, because
      -- if we handle it in ccOnException, it is already logged as ATTENTION.
      runExtending dec `catchDBExtraException` (\(_ :: DocumentWasPurged) -> do
        logInfo "Document was purged before extending" $ object [
            identifier_ $ decDocumentID dec
          ]
        return $ Failed Remove)
  , ccOnException = const onFailure
  }
  where
    runExtending dec = do
      resultisok <- withPostgreSQL pool
        . withDocumentM (dbQuery $ GetDocumentByDocumentID $ decDocumentID dec)
        . runTemplatesT (def, templates)
        . runGuardTimeConfT guardTimeConf
        . runFileStorageT (amazonConfig, mRedisConn, memcache)
        $ extendDigitalSignature
      case resultisok of
        True  -> return $ Ok Remove
        False -> Failed <$> onFailure dec
    onFailure DocumentExtendingConsumer{..} = do
      when (decAttempts > 1) $ do
        logAttention "Document extending failed more than 1 time" $ object [
            identifier_ decDocumentID
          , "attempt_count" .= decAttempts
          ]
      return . RerunAfter $ idays 1

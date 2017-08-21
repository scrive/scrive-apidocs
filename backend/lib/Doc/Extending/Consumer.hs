module Doc.Extending.Consumer (
    DocumentExtendingConsumer
  , documentExtendingConsumer
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Crypto.RNG
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Int
import Database.PostgreSQL.Consumers.Config
import Log.Class
import qualified Database.Redis as R

import AppConf
import DB
import DB.PostgreSQL
import Doc.DigitalSignature
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model.Query
import File.FileID
import GuardTime
import KontraPrelude
import Log.Identifier
import MemCache (MemCache)
import Templates
import qualified Amazon as A

data DocumentExtendingConsumer = DocumentExtendingConsumer {
    decDocumentID :: !DocumentID
  , decAttempts :: !Int32
  }

documentExtendingConsumer
  :: (CryptoRNG m, MonadLog m, MonadIO m, MonadBaseControl IO m, MonadMask m)
  => Maybe AmazonConfig
  -> GuardTimeConf
  -> KontrakcjaGlobalTemplates
  -> MemCache FileID ByteString
  -> Maybe R.Connection
  -> ConnectionSourceM m
  -> Int
  -> ConsumerConfig m DocumentID DocumentExtendingConsumer
documentExtendingConsumer mbAmazonConf guardTimeConf templates localCache globalCache pool maxRunningJobs = ConsumerConfig {
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
  , ccProcessJob = \dec@DocumentExtendingConsumer{..} -> do
      let ac = A.AmazonConfig {
              A.awsConfig = mbAmazonConf
            , A.awsLocalCache = localCache
            , A.awsGlobalCache = globalCache
            }
      resultisok <- withPostgreSQL pool
        . withDocumentM (dbQuery $ GetDocumentByDocumentID decDocumentID)
        . runTemplatesT (def, templates)
        . runGuardTimeConfT guardTimeConf
        . A.runAmazonMonadT ac
        $ extendDigitalSignature
      case resultisok of
        True  -> return $ Ok Remove
        False -> Failed <$> onFailure dec
  , ccOnException = const onFailure
  }
  where
    onFailure DocumentExtendingConsumer{..} = do
      when (decAttempts > 1) $ do
        logAttention "Document extending failed more than 1 time" $ object [
            identifier_ decDocumentID
          , "attempt_count" .= decAttempts
          ]
      return . RerunAfter $ idays 1

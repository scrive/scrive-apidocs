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
import User.Lang (defaultLang)

data DocumentExtendingConsumer = DocumentExtendingConsumer {
    decDocumentID :: DocumentID
  , decAttempts :: Int32
  }

documentExtendingConsumer
  :: ( CryptoRNG m
     , MonadBaseControl IO m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     )
  => GuardTimeConf
  -> KontrakcjaGlobalTemplates
  -> ConnectionSourceM m
  -> Int
  -> ConsumerConfig m DocumentID DocumentExtendingConsumer
documentExtendingConsumer guardTimeConf templates pool maxRunningJobs = ConsumerConfig
  { ccJobsTable           = "document_extending_jobs"
  , ccConsumersTable      = "document_extending_consumers"
  , ccJobSelectors        = ["id", "attempts"]
  , ccJobFetcher = \(did, attempts) -> DocumentExtendingConsumer { decDocumentID = did
                                                                 , decAttempts = attempts
                                                                 }
  , ccJobIndex            = decDocumentID
  , ccNotificationChannel = Nothing
  -- The amount of queued jobs in the table approximately equals the amount of
  -- signed documents per month, so don't try to check for available jobs too
  -- often as that requires full table scan.
  , ccNotificationTimeout = 60 * 60 * 1000000 -- 1 hour
  , ccMaxRunningJobs      = maxRunningJobs
  , ccProcessJob          =
    \dec ->
      -- We put handling of DocumentWasPurged exception here, because
      -- if we handle it in ccOnException, it is already logged as ATTENTION.
      runExtending dec
        `catchDBExtraException` (\(_ :: DocumentWasPurged) -> do
                                  logInfo "Document was purged before extending"
                                    $ object [identifier $ decDocumentID dec]
                                  return $ Failed Remove
                                )
  , ccOnException         = const onFailure
  }
  where
    runExtending dec = do
      resultisok <-
        withPostgreSQL pool
        . withDocumentM (dbQuery . GetDocumentByDocumentID $ decDocumentID dec)
        . runTemplatesT (defaultLang, templates)
        . runGuardTimeConfT guardTimeConf
        $ extendDigitalSignature
      if resultisok then return $ Ok Remove else Failed <$> onFailure dec
    onFailure DocumentExtendingConsumer {..} = do
      when (decAttempts > 1) $ do
        logAttention "Document extending failed more than 1 time"
          $ object [identifier decDocumentID, "attempt_count" .= decAttempts]
      return . RerunAfter $ idays 1

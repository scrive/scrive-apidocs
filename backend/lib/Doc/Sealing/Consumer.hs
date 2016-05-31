module Doc.Sealing.Consumer (
    DocumentSealing
  , documentSealing
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.Int
import Database.PostgreSQL.Consumers.Config
import Log.Class
import qualified Database.Redis as R

import AppConf
import BrandedDomain.Model
import Crypto.RNG
import DB
import DB.PostgreSQL
import Doc.Action
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Sealing.Model
import File.FileID
import GuardTime
import KontraPrelude
import MailContext
import MemCache (MemCache)
import Templates
import User.Lang
import qualified Amazon as A

data DocumentSealing = DocumentSealing {
    dsDocumentID      :: !DocumentID
  , dsBrandedDomainID :: !BrandedDomainID
  , dsAttempts        :: !Int32
  }

documentSealing :: (CryptoRNG m, MonadLog m, MonadIO m, MonadBaseControl IO m, MonadMask m)
                => AppConf
                -> KontrakcjaGlobalTemplates
                -> MemCache FileID ByteString
                -> Maybe R.Connection
                -> ConnectionSource
                -> ConsumerConfig m DocumentID DocumentSealing
documentSealing appConf templates localCache globalCache pool = ConsumerConfig {
    ccJobsTable = "document_sealing_jobs"
  , ccConsumersTable = "document_sealing_consumers"
  , ccJobSelectors = ["id", "branded_domain_id", "attempts"]
  , ccJobFetcher = \(did, bdid, attempts) -> DocumentSealing {
      dsDocumentID      = did
    , dsBrandedDomainID = bdid
    , dsAttempts        = attempts
    }
  , ccJobIndex = dsDocumentID
  , ccNotificationChannel = Just documentSealingNotificationChannel
  , ccNotificationTimeout = 60 * 1000000 -- 1 minute
  , ccMaxRunningJobs = 2
  , ccProcessJob = \DocumentSealing{..} -> withPostgreSQL pool . withDocumentID dsDocumentID $ do
      now <- currentTime
      bd <- dbQuery $ GetBrandedDomainByID dsBrandedDomainID
      doc <- theDocument
      let lang = getLang doc
          ac = A.AmazonConfig {
              A.awsConfig = amazonConfig appConf
            , A.awsLocalCache = localCache
            , A.awsGlobalCache = globalCache
            }
          mc = MailContext {
              mctxmailsconfig = mailsConfig appConf
            , mctxlang = lang
            , mctxcurrentBrandedDomain = bd
            , mctxtime = now
            }
      runGuardTimeConfT (guardTimeConf appConf)
        . runTemplatesT (lang, templates)
        . A.runAmazonMonadT ac
        . runMailContextT mc
        $ postDocumentClosedActions True False
      return $ Ok Remove
  , ccOnException = const onFailure
  }
  where
    onFailure DocumentSealing{..} = case dsAttempts of
      1 -> return . RerunAfter $ iminutes 5
      2 -> return . RerunAfter $ iminutes 10
      3 -> return . RerunAfter $ iminutes 30
      4 -> return . RerunAfter $ ihours 1
      5 -> return . RerunAfter $ ihours 2
      6 -> return . RerunAfter $ ihours 4
      7 -> return . RerunAfter $ ihours 8
      8 -> return . RerunAfter $ ihours 16
      _ -> return . RerunAfter $ idays 1

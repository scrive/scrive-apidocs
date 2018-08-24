-- CORE-478: should be removed
module Amazon.Consumer (
    AmazonUploadConsumer
  , amazonUploadConsumer
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Crypto.RNG
import Data.Aeson
import Data.Int
import Database.PostgreSQL.Consumers.Config
import Log.Class

import DB
import DB.PostgreSQL
import File.FileID
import File.Model
import Log.Identifier
import qualified FileStorage.Amazon as A
import qualified FileStorage.Amazon.Config as A

data AmazonUploadConsumer = AmazonUploadConsumer {
    aucFileID :: !FileID
  , aucAttempts :: !Int32
  } deriving Show

amazonUploadConsumer
  :: (MonadIO m, MonadBase IO m, MonadLog m, CryptoRNG m, MonadMask m)
  => A.AmazonConfig
  -> ConnectionSourceM m
  -> Int
  -> ConsumerConfig m FileID AmazonUploadConsumer
amazonUploadConsumer amazonConf pool maxRunningJobs = ConsumerConfig {
    ccJobsTable = "amazon_upload_jobs"
  , ccConsumersTable = "amazon_upload_consumers"
  , ccJobSelectors =
    [ "id"
    , "attempts"
    ]
  , ccJobFetcher = \(fid, attempts) -> AmazonUploadConsumer {
      aucFileID = fid
    , aucAttempts = attempts
    }
  , ccJobIndex = aucFileID
  , ccNotificationChannel = Nothing
  , ccNotificationTimeout = 60 * 1000000 -- 1 minute
  , ccMaxRunningJobs = maxRunningJobs
  , ccProcessJob = \auc@AmazonUploadConsumer{..} -> do
      if A.isAmazonConfigValid amazonConf
        then do
          withPostgreSQL pool $ do
            mfile <- dbQuery $ GetMaybeFileByFileID aucFileID
            case mfile of
              Nothing -> do
                logInfo "File missing, so it cannot be uploaded to AWS." $ object [
                    identifier aucFileID
                  ]
                -- this means, that file was not found or was purged
                return $ Failed Remove
              Just file -> do
                success <- A.exportFile (A.mkAWSAction (Just amazonConf)) file
                case success of
                  True  -> return $ Ok Remove
                  False -> Failed <$> onFailure auc
        else return . Failed . RerunAfter $ idays 1
  , ccOnException = const onFailure
  }
  where
    onFailure AmazonUploadConsumer{..} = do
      when (aucAttempts > 1) $ do
        logAttention "File upload to Amazon failed more than 1 time" $ object [
            identifier aucFileID
          , "attempt_count" .= aucAttempts
          ]
      return . RerunAfter $ idays 1

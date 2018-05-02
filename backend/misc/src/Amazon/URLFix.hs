-- CORE-478: should be removed
module Amazon.URLFix
  ( AmazonURLFixConsumer
  , amazonURLFixConsumer
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Crypto.RNG
import Data.Aeson
import Data.Either (isRight)
import Data.Functor.Identity
import Database.PostgreSQL.Consumers.Config
import Log.Class
import Network.HTTP.Base (RequestMethod(..))
import qualified Network.AWS.Authentication as AWS
import qualified Network.AWS.AWSResult as AWS
import qualified Network.HTTP as HTTP

import DB
import DB.PostgreSQL
import File.File
import File.FileID
import File.Model
import Log.Identifier
import qualified FileStorage.Amazon as A
import qualified FileStorage.Amazon.Config as A

checkAndFixURL :: (MonadBase IO m, MonadIO m, MonadDB m, MonadLog m, CryptoRNG m) => AWS.S3Action -> FileStorage -> m Bool
checkAndFixURL _ (FileStorageMemory _) = return True
checkAndFixURL s3action (FileStorageAWS correctURL _) = do
  let wrongURL = HTTP.urlEncode correctURL
  res <- liftIO $ AWS.runAction $ s3action
    { AWS.s3object    = wrongURL
    , AWS.s3operation = HEAD
    }
  case res of
    Left (AWS.AWSError "NoSuchKey" _) -> return True
    Left _ -> return False
    Right _ -> do
      -- Copy file to the correct URL
      res' <- liftIO $ AWS.runAction $ s3action
        { AWS.s3object = correctURL
        , AWS.s3metadata =
            ("x-amz-copy-source", AWS.s3bucket s3action ++ '/' : wrongURL)
            : AWS.s3metadata s3action
        , AWS.s3operation = PUT
        }
      case res' of
        Left _ -> return False
        Right _ -> do
          -- Delete wrong one
          res'' <- liftIO $ AWS.runAction $ s3action
            { AWS.s3object    = wrongURL
            , AWS.s3operation = DELETE
            }
          return $ isRight res''

data AmazonURLFixConsumer = AmazonURLFixConsumer {
    aufcFileID :: !FileID
  } deriving Show

amazonURLFixConsumer
  :: (MonadIO m, MonadBase IO m, MonadLog m, CryptoRNG m, MonadMask m)
  => A.AmazonConfig -> ConnectionSourceM m
  -> ConsumerConfig m FileID AmazonURLFixConsumer
amazonURLFixConsumer config pool = ConsumerConfig {
    ccJobsTable = "amazon_url_fix_jobs"
  , ccConsumersTable = "amazon_url_fix_consumers"
  , ccJobSelectors = ["id"]
  , ccJobFetcher = \(Identity fid) -> AmazonURLFixConsumer {
      aufcFileID = fid
    }
  , ccJobIndex = aufcFileID
  , ccNotificationChannel = Nothing
  , ccNotificationTimeout = 60 * 1000000 -- 1 minute
  , ccMaxRunningJobs = 1
  , ccProcessJob = \aufc@AmazonURLFixConsumer{..} -> do
      if A.isAmazonConfigValid config
        then do
          withPostgreSQL pool $ do
            mfile <- dbQuery $ GetMaybeFileByFileID aufcFileID
            case mfile of
              Nothing -> do
                logInfo "File missing, so it cannot be checked on AWS." $ object [
                    identifier_ aufcFileID
                  ]
                -- this means, that file was not found or was purged
                return $ Failed Remove
              Just file -> do
                success <- checkAndFixURL (A.mkAWSAction (Just config)) (filestorage file)
                case success of
                  True  -> return $ Ok Remove
                  False -> Failed <$> onFailure aufc
        else return . Failed . RerunAfter $ idays 1
  , ccOnException = const onFailure
  }
  where
    onFailure AmazonURLFixConsumer{..} = return . RerunAfter $ idays 1

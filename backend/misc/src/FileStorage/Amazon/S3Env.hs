module FileStorage.Amazon.S3Env where

import Control.Monad.Base
import qualified Control.Lens as L
import qualified Network.AWS as AWS
import qualified Network.AWS.S3 as AWS

import FileStorage.Amazon.Config

data AmazonS3Env = AmazonS3Env
  { as3eEnv    :: AWS.Env
  , as3eBucket :: AWS.BucketName
  }

s3envFromConfig :: MonadBase IO m => AmazonConfig -> m AmazonS3Env
s3envFromConfig conf = liftBase $ do
  env <- L.set AWS.envRegion (amazonConfigRegion conf) . AWS.configure s3
    <$> AWS.newEnv (AWS.FromKeys accessKey secretKey)
  return AmazonS3Env
    { as3eEnv = env
    , as3eBucket = AWS.BucketName $ amazonConfigBucket conf
    }
  where
    s3 = AWS.setEndpoint (amazonConfigSecure conf)
                         (amazonConfigHost conf)
                         (amazonConfigPort conf)
                         AWS.s3

    accessKey = AWS.AccessKey $ amazonConfigAccessKey conf
    secretKey = AWS.SecretKey $ amazonConfigSecretKey conf

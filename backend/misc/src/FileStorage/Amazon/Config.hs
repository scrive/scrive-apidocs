module FileStorage.Amazon.Config
  ( AmazonConfig(..)
  , isAmazonConfigValid
  , s3ConnFromConfig
  , s3ActionFromConfig
  , s3ObjectFromConfig
  ) where

import Data.Unjson
import qualified Data.ByteString.Lazy as BSL
import qualified Network.AWS.Authentication as AWS
import qualified Network.AWS.AWSConnection as AWS
import qualified Network.AWS.S3Object as AWS
import qualified Network.HTTP as HTTP

-- | AWS config: (host, port, bucket, access key, secret key).
data AmazonConfig = AmazonConfig
  { amazonConfigHost      :: String
  , amazonConfigPort      :: Int
  , amazonConfigBucket    :: String
  , amazonConfigAccessKey :: String
  , amazonConfigSecretKey :: String
  } deriving (Eq, Show)

instance Unjson AmazonConfig where
  unjsonDef = objectOf $ AmazonConfig
    <$> fieldDef "host" AWS.defaultAmazonS3Host
          amazonConfigHost
          "Hostname of the S3 server"
    <*> fieldDef "port" AWS.defaultAmazonS3Port
          amazonConfigPort
          "Port to connect to"
    <*> field "bucket"
          amazonConfigBucket
          "In which bucket stored files exist"
    <*> field "access_key"
          amazonConfigAccessKey
          "Amazon access key"
    <*> field "secret_key"
          amazonConfigSecretKey
          "Amazon secret key"

isAmazonConfigValid :: AmazonConfig -> Bool
isAmazonConfigValid AmazonConfig{..} =
  not (null amazonConfigHost)
  && amazonConfigPort > 0
  && amazonConfigPort < 65536
  && not (null amazonConfigBucket)
  && not (null amazonConfigAccessKey)
  && not (null amazonConfigSecretKey)

s3ConnFromConfig :: AmazonConfig -> AWS.AWSConnection
s3ConnFromConfig AmazonConfig{..} = AWS.AWSConnection
  amazonConfigHost amazonConfigPort amazonConfigAccessKey amazonConfigSecretKey

s3ActionFromConfig :: AmazonConfig -> String -> AWS.S3Action
s3ActionFromConfig config url = AWS.S3Action
  { AWS.s3conn      = s3ConnFromConfig config
  , AWS.s3bucket    = amazonConfigBucket config
  , AWS.s3object    = url
  , AWS.s3query     = ""
  , AWS.s3metadata  = []
  , AWS.s3body      = BSL.empty
  , AWS.s3operation = HTTP.GET
  }

s3ObjectFromConfig :: AmazonConfig -> String -> AWS.S3Object
s3ObjectFromConfig config url = AWS.S3Object
  { AWS.obj_bucket   = amazonConfigBucket config
  , AWS.obj_name     = HTTP.urlDecode url
  , AWS.content_type = ""
  , AWS.obj_headers  = []
  , AWS.obj_data     = ""
  }

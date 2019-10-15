module FileStorage.Amazon.Config
  ( AmazonConfig(..)
  , isAmazonConfigValid
  ) where

import Data.Unjson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Network.AWS as AWS
import qualified Network.AWS.Data.Text as AWS

import Data.ByteString.Utils

-- | AWS config: (host, port, bucket, access key, secret key).
data AmazonConfig = AmazonConfig
  { amazonConfigHost      :: BSC.ByteString
  , amazonConfigPort      :: Int
  , amazonConfigSecure    :: Bool
  , amazonConfigTimeout   :: Int
  , amazonConfigRegion    :: AWS.Region
  , amazonConfigBucket    :: Text
  , amazonConfigAccessKey :: BSC.ByteString
  , amazonConfigSecretKey :: BSC.ByteString
  } deriving (Eq, Show)

instance Unjson AmazonConfig where
  unjsonDef = objectOf $ AmazonConfig
    <$> fieldDefBy "host" "s3.eu-west-1.amazonaws.com"
          amazonConfigHost
          "Hostname of the S3 server"
          unjsonByteString
    <*> fieldDef "port" 443
          amazonConfigPort
          "Port to connect to"
    <*> fieldDef "secure" True
          amazonConfigSecure
          "Whether to use HTTPS (ie. SSL)"
    <*> fieldDef "timeout" 60
          amazonConfigTimeout
          "Request timeout (seconds)"
    <*> fieldDefBy "region" AWS.Ireland
          amazonConfigRegion
          "Amazon region (eg. eu-west-1)"
          unjsonRegion
    <*> field "bucket"
          amazonConfigBucket
          "In which bucket stored files exist"
    <*> fieldBy "access_key"
          amazonConfigAccessKey
          "Amazon access key"
          unjsonByteString
    <*> fieldBy "secret_key"
          amazonConfigSecretKey
          "Amazon secret key"
          unjsonByteString

isAmazonConfigValid :: AmazonConfig -> Bool
isAmazonConfigValid AmazonConfig{..} =
  not (BSC.null amazonConfigHost)
  && amazonConfigPort > 0
  && amazonConfigPort < 65536
  && not (T.null amazonConfigBucket)
  && not (BSC.null amazonConfigAccessKey)
  && not (BSC.null amazonConfigSecretKey)

----------------------------------------

unjsonRegion :: UnjsonDef AWS.Region
unjsonRegion =
  unjsonInvmapR (either fail return . AWS.fromText) AWS.toText unjsonDef

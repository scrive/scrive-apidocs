module PdfToolsLambda.Conf.Internal where

import Control.Monad.Base
import Data.Unjson

import FileStorage.Amazon.Config
import FileStorage.Amazon.S3Env

data PdfToolsLambdaConf = PdfToolsLambdaConf
  { pdfToolsLambdaConfGatewayUrl :: String
  , pdfToolsLambdaConfApiKey :: String
  , pdfToolsLambdaConfConfig :: AmazonConfig
  } deriving (Show, Eq)

instance Unjson PdfToolsLambdaConf where
  unjsonDef =
    objectOf
      $   pure PdfToolsLambdaConf
      <*> field "gateway_url" pdfToolsLambdaConfGatewayUrl "Pdf Tools Lambda Gateway Url"
      <*> field "api_key"     pdfToolsLambdaConfApiKey     "Pdf Tools Lambda Api Key"
      <*> field "amazon_s3"   pdfToolsLambdaConfConfig     "Amazon bucket configuration"

----------------------------------------

data PdfToolsLambdaEnv = PdfToolsLambdaEnv
  { pdfToolsLambdaGatewayUrl :: String
  , pdfToolsLambdaApiKey :: String
  , pdfToolsLambdaS3Env :: AmazonS3Env
  }

pdfToolsLambdaEnvFromConf :: MonadBase IO m => PdfToolsLambdaConf -> m PdfToolsLambdaEnv
pdfToolsLambdaEnvFromConf PdfToolsLambdaConf {..} =
  PdfToolsLambdaEnv
    <$> pure pdfToolsLambdaConfGatewayUrl
    <*> pure pdfToolsLambdaConfApiKey
    <*> s3envFromConfig pdfToolsLambdaConfConfig

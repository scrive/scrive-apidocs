module PdfToolsLambda.Conf.Internal where

import Control.Monad.Base
import Data.Unjson

import FileStorage.Amazon.Config
import FileStorage.Amazon.S3Env

data PdfToolsLambdaConf = PdfToolsLambdaConf
  { _pdfToolsLambdaConfGatewayUrl :: String
  , _pdfToolsLambdaConfApiKey :: String
  , _pdfToolsLambdaConfConfig :: AmazonConfig
  } deriving (Show, Eq)

instance Unjson PdfToolsLambdaConf where
  unjsonDef =
    objectOf
      $   pure PdfToolsLambdaConf
      <*> field "gateway_url" _pdfToolsLambdaConfGatewayUrl "Pdf Tools Lambda Gateway Url"
      <*> field "api_key"     _pdfToolsLambdaConfApiKey     "Pdf Tools Lambda Api Key"
      <*> field "amazon_s3"   _pdfToolsLambdaConfConfig     "Amazon bucket configuration"

----------------------------------------

data PdfToolsLambdaEnv = PdfToolsLambdaEnv
  { _pdfToolsLambdaGatewayUrl :: String
  , _pdfToolsLambdaApiKey :: String
  , _pdfToolsLambdaS3Env :: AmazonS3Env
  }

pdfToolsLambdaEnvFromConf :: MonadBase IO m => PdfToolsLambdaConf -> m PdfToolsLambdaEnv
pdfToolsLambdaEnvFromConf PdfToolsLambdaConf {..} =
  PdfToolsLambdaEnv
    <$> pure _pdfToolsLambdaConfGatewayUrl
    <*> pure _pdfToolsLambdaConfApiKey
    <*> s3envFromConfig _pdfToolsLambdaConfConfig

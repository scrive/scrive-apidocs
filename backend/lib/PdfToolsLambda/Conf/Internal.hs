{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module PdfToolsLambda.Conf.Internal where

import Control.Monad.Base
import Data.Unjson
import Optics.TH

import FileStorage.Amazon.Config
import FileStorage.Amazon.S3Env

data PdfToolsLambdaConf = PdfToolsLambdaConf
  { gatewayUrl :: String
  , apiKey     :: String
  , config     :: AmazonConfig
  } deriving (Show, Eq)

data PdfToolsLambdaEnv = PdfToolsLambdaEnv
  { gatewayUrl :: String
  , apiKey     :: String
  , s3Env      :: AmazonS3Env
  }

makeFieldLabelsWith noPrefixFieldLabels ''PdfToolsLambdaConf
makeFieldLabelsWith noPrefixFieldLabels ''PdfToolsLambdaEnv

instance Unjson PdfToolsLambdaConf where
  unjsonDef =
    objectOf
      $   pure PdfToolsLambdaConf
      <*> field "gateway_url" (^. #gatewayUrl) "Pdf Tools Lambda Gateway Url"
      <*> field "api_key"     (^. #apiKey)     "Pdf Tools Lambda Api Key"
      <*> field "amazon_s3"   (^. #config)     "Amazon bucket configuration"

pdfToolsLambdaEnvFromConf :: MonadBase IO m => PdfToolsLambdaConf -> m PdfToolsLambdaEnv
pdfToolsLambdaEnvFromConf PdfToolsLambdaConf {..} =
  PdfToolsLambdaEnv <$> pure gatewayUrl <*> pure apiKey <*> s3envFromConfig config

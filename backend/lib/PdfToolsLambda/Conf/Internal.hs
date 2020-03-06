{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module PdfToolsLambda.Conf.Internal where

import Control.Monad.Base
import Data.Unjson
import Optics.TH

import FileStorage.Amazon.Config
import FileStorage.Amazon.S3Env

data LambdaConfig = LambdaConfig
  { gatewayUrl :: Text
  , apiKey     :: Text
  } deriving (Show, Eq)

data GlobalSignConfig = GlobalSignConfig
  { apiKey              :: Text
  , apiPassword         :: Text
  , certificate         :: Text
  , certificatePassword :: Text
  } deriving (Show, Eq)

data PdfToolsLambdaConf = PdfToolsLambdaConf
  { gatewayUrl :: Text
  , apiKey     :: Text
  , globalSign :: Maybe (GlobalSignConfig)
  , s3         :: AmazonConfig
  } deriving (Show, Eq)

data PdfToolsLambdaEnv = PdfToolsLambdaEnv
  { lambda     :: LambdaConfig
  , globalSign :: Maybe (GlobalSignConfig)
  , s3Env      :: AmazonS3Env
  }

makeFieldLabelsWith noPrefixFieldLabels ''LambdaConfig
makeFieldLabelsWith noPrefixFieldLabels ''GlobalSignConfig
makeFieldLabelsWith noPrefixFieldLabels ''PdfToolsLambdaConf
makeFieldLabelsWith noPrefixFieldLabels ''PdfToolsLambdaEnv

instance Unjson GlobalSignConfig where
  unjsonDef =
    objectOf
      $   pure GlobalSignConfig
      <*> field "api_key"      (^. #apiKey)      "GlobalSign API key"
      <*> field "api_password" (^. #apiPassword) "GlobalSign API password"
      <*> field "certificate" (^. #certificate) "GlobalSign certificate encoded as Base64"
      <*> field "certificate_password"
                (^. #certificatePassword)
                "GlobalSign certificate secret"

instance Unjson PdfToolsLambdaConf where
  unjsonDef =
    objectOf
      $   pure PdfToolsLambdaConf
      <*> field "gateway_url" (^. #gatewayUrl) "Pdf Tools Lambda Gateway Url"
      <*> field "api_key"     (^. #apiKey)     "Pdf Tools Lambda Api Key"
      <*> fieldOpt "global_sign" (^. #globalSign) "GlobalSign configuration"
      <*> field "amazon_s3" (^. #s3) "Amazon bucket configuration"

pdfToolsLambdaEnvFromConf :: MonadBase IO m => PdfToolsLambdaConf -> m PdfToolsLambdaEnv
pdfToolsLambdaEnvFromConf PdfToolsLambdaConf {..} =
  PdfToolsLambdaEnv <$> pure lambda <*> pure globalSign <*> s3envFromConfig s3
  where lambda = LambdaConfig gatewayUrl apiKey

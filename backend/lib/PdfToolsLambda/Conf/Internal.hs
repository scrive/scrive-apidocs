module PdfToolsLambda.Conf.Internal where

import FileStorage.Amazon.Config

data PdfToolsLambdaConf = PdfToolsLambdaConf {
    _pdfToolsLambdaGatewayUrl :: String
  , _pdfToolsLambdaApiKey :: String
  , _pdfToolsLambdaS3Config :: AmazonConfig
} deriving (Show, Eq)

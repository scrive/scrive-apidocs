module PdfToolsLambda.Conf.Internal where

import FileStorage.Amazon.Config
import UserGroup.Internal (UserGroupID)

data PdfToolsLambdaConf = PdfToolsLambdaConf {
    _pdfToolsLambdaGatewayUrl :: String
  , _pdfToolsLambdaApiKey :: String
  , _pdfToolsLambdaS3Config :: AmazonConfig
  , _pdfToolsLambdaSkip :: Bool
  , _pdfToolsUserGroupsWithExtendedFlattening :: Maybe [UserGroupID]
} deriving (Show, Eq)

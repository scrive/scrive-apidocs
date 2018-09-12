module PdfToolsLambda.Conf.Internal where

import FileStorage.Amazon.Config
import UserGroup.Internal (UserGroupID)

data PdfToolsLambdaConf = PdfToolsLambdaConf {
    _pdfToolsLambdaGatewayUrl :: String
  , _pdfToolsLambdaApiKey :: String
  , _pdfToolsLambdaS3Config :: AmazonConfig
  , _pdfToolsUserGroupsWithExtendedFlattening :: Maybe [UserGroupID]
  , _pdfToolsUserGroupsWithOldFlattening :: Maybe [UserGroupID]
} deriving (Show, Eq)

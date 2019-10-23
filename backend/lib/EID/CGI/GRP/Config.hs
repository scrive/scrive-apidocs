module EID.CGI.GRP.Config where

import Data.Unjson

data CgiGrpConfig = CgiGrpConfig
  { -- | CGI url to GRP API
    cgGateway     :: String
  -- | Path to the certificate file
  , cgCertFile    :: FilePath
  -- | Service ID registered with CGI
  , cgServiceID   :: Text
  -- | Display name registered with CGI
  , cgDisplayName :: Text
  } deriving (Eq, Ord, Show)

instance Unjson CgiGrpConfig where
  unjsonDef =
    objectOf
      $   CgiGrpConfig
      <$> field "gateway"      cgGateway     "URL"
      <*> field "cert_file" cgCertFile "Path to the certificate file"
      <*> field "service_id"   cgServiceID   "Service ID"
      <*> field "display_name" cgDisplayName "Display name"

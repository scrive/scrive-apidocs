module Callback.Types
  ( CallbackID(..)
  , AuthMethod(..)
  ) where

import Data.Aeson
import Data.Int
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import DB
import Log.Identifier

newtype CallbackID = CallbackID Int64
  deriving newtype (Eq, Ord, Show, PQFormat, FromSQL, ToSQL)

instance Identifier CallbackID where
  idDefaultLabel = "callback_id"
  idValue (CallbackID k) = int64AsStringIdentifier k

data AuthMethod
  = NoAuth
  | BasicAuth Text {- user -} Text {- password -}
  -- other things later if necessary
  deriving (Eq, Ord, Show)

instance PQFormat AuthMethod where
  pqFormat = pqFormat @(JSONB AuthMethod)

instance ToSQL AuthMethod where
  type PQDest AuthMethod = PQDest (JSONB BS.ByteString)
  toSQL = aesonToSQL

instance FromSQL AuthMethod where
  type PQBase AuthMethod = PQBase (JSONB BS.ByteString)
  fromSQL = aesonFromSQL

instance ToJSON AuthMethod where
  toJSON = \case
    NoAuth -> object ["auth" .= ("none" :: T.Text)]
    BasicAuth user password ->
      object ["auth" .= ("basic" :: T.Text), "user" .= user, "password" .= password]

instance FromJSON AuthMethod where
  parseJSON = withObject "AuthMethod" $ \v -> case "auth" `HM.lookup` v of
    Just (String "none" ) -> pure NoAuth
    Just (String "basic") -> BasicAuth <$> v .: "user" <*> v .: "password"
    Just auth             -> fail $ "invalid authentication method: " ++ show auth
    Nothing               -> fail "no authentication method"

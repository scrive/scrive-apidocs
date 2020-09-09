{-# LANGUAGE DeriveAnyClass #-}
module Flow.EID.AuthConfig
    ( AuthConfig(..)
    , AuthProvider(..)
    )
  where

import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Casing
import Database.PostgreSQL.PQTypes
import GHC.Generics
import qualified Data.Text.Lazy.Encoding as LT

aesonOptions :: Options
aesonOptions =
  defaultOptions { fieldLabelModifier = snakeCase, constructorTagModifier = snakeCase }


data AuthConfig = AuthConfig
    { provider :: AuthProvider
    , maxFailures :: Int
    }
  deriving (Show, Eq, Generic)

instance FromJSON AuthConfig where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON AuthConfig where
  toEncoding = genericToEncoding aesonOptions


data AuthProvider = SmsPin | Onfido
  deriving (Show, Eq, Generic)

instance FromJSON AuthProvider where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON AuthProvider where
  toEncoding = genericToEncoding aesonOptions

instance FromSQL AuthProvider where
  type PQBase AuthProvider = PQBase Text
  fromSQL mbase = fromSQL mbase >>= (either throwM pure . decodeAuthProvider)
    where
      decodeAuthProvider t = case decode (LT.encodeUtf8 t) of
        Just authProvider -> Right authProvider
        Nothing -> Left $ UnknownAuthProvider "FromSQL: can't decode AuthProvider"

instance ToSQL AuthProvider where
  type PQDest AuthProvider = PQDest Text
  toSQL = toSQL . LT.decodeUtf8 . encode

instance PQFormat AuthProvider where
  pqFormat = pqFormat @Text

newtype UnknownAuthProvider = UnknownAuthProvider Text
  deriving Show
  deriving anyclass Exception

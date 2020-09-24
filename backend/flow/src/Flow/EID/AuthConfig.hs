{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Flow.EID.AuthConfig
    ( AuthConfig(..)
    , AuthProvider(..)
    , toAuthenticationToViewMethod
    )
  where

import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Casing
import Database.PostgreSQL.PQTypes
import GHC.Generics
import Optics.TH
import Servant.API
import qualified Data.Text.Lazy.Encoding as LT

import Doc.Types.SignatoryLink (AuthenticationToViewMethod(..))

aesonOptions :: Options
aesonOptions =
  defaultOptions { fieldLabelModifier = snakeCase, constructorTagModifier = snakeCase }

-- Auth config

data AuthConfig = AuthConfig
    { provider :: AuthProvider
    , maxFailures :: Int
    }
  deriving (Show, Eq, Generic)

instance FromJSON AuthConfig where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON AuthConfig where
  toEncoding = genericToEncoding aesonOptions

-- Auth provider

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

instance ToHttpApiData AuthProvider where
  toUrlPiece = \case
    Onfido -> "onfido"
    SmsPin -> "sms_pin"

newtype UnknownAuthProvider = UnknownAuthProvider Text
  deriving Show
  deriving anyclass Exception

toAuthenticationToViewMethod :: AuthProvider -> AuthenticationToViewMethod
toAuthenticationToViewMethod = \case
  Onfido -> OnfidoDocumentCheckAuthenticationToView
  SmsPin -> SMSPinAuthenticationToView

-- Optics

makeFieldLabelsWith noPrefixFieldLabels ''AuthConfig

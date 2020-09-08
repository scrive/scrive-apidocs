module Flow.Core.Type.Callback
    ( Callback(..)
    , CallbackVersion(..)
    )
  where

import Control.Monad.Catch
import Data.Aeson
import Data.Aeson.Casing
import Data.Int
import Database.PostgreSQL.PQTypes
import GHC.Generics

import Flow.Core.Type.Url

data CallbackVersion = V1
  deriving (Eq, Generic, Show)

instance PQFormat CallbackVersion where
  pqFormat = pqFormat @Int32

instance FromSQL CallbackVersion where
  type PQBase CallbackVersion = PQBase Int32
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int32 of
      1 -> return V1
      _ -> throwM RangeError { reRange = [(1, 1)], reValue = n }

instance ToSQL CallbackVersion where
  type PQDest CallbackVersion = PQDest Int32
  toSQL = toSQL . callbackVersionToNumber

instance FromJSON CallbackVersion where
  parseJSON o = do
    version <- parseJSON o
    callbackNumberToVersion version

callbackVersionToNumber :: CallbackVersion -> Int32
callbackVersionToNumber V1 = 1

callbackNumberToVersion :: MonadFail m => Int32 -> m CallbackVersion
callbackNumberToVersion = \case
  1 -> pure V1
  n -> fail $ "Only callback payload version 1 is supported. Provided: " <> show n

instance ToJSON CallbackVersion where
  toEncoding v = toEncoding $ callbackVersionToNumber v

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

data Callback = Callback
    { url :: Url
    , version :: CallbackVersion
    }
  deriving (Eq, Generic, Show)

instance FromJSON Callback where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON Callback where
  toEncoding = genericToEncoding aesonOptions

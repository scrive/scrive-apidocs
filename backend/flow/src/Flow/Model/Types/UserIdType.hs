module Flow.Model.Types.UserIdType
  ( UserIdType(..)
  ) where

import Data.Aeson
import Data.Aeson.Casing
import GHC.Generics

data UserIdType = Email | PhoneNumber | UserId
  deriving (Eq, Generic, Show)

instance FromJSON UserIdType where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = snakeCase }

instance ToJSON UserIdType where
  toEncoding = genericToEncoding defaultOptions { constructorTagModifier = snakeCase }

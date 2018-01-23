module User.Email (
    Email(..)
  ) where

import Data.Aeson
import Data.Functor.Invariant
import Data.Typeable
import Data.Unjson

import DB

-- newtypes
newtype Email = Email { unEmail :: String }
  deriving (Eq, Ord, PQFormat, Typeable)
deriving newtype instance Read Email
deriving newtype instance Show Email

instance FromSQL Email where
  type PQBase Email = PQBase String
  fromSQL mbase = Email <$> fromSQL mbase
instance ToSQL Email where
  type PQDest Email = PQDest String
  toSQL (Email n) = toSQL n

instance FromJSON Email where
  parseJSON = fmap Email . parseJSON

instance ToJSON Email where
  toJSON     = toJSON . unEmail
  toEncoding = toEncoding . unEmail

instance Unjson Email where
  unjsonDef = invmap Email unEmail unjsonDef

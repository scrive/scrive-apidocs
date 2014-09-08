module User.Email (
    Email(..)
  ) where

import Control.Applicative
import DB
import Data.Aeson
import Data.Typeable
import Data.Unjson

-- newtypes
newtype Email = Email { unEmail :: String }
  deriving (Eq, Ord, PQFormat, Typeable)
$(newtypeDeriveUnderlyingReadShow ''Email)

instance FromSQL Email where
  type PQBase Email = PQBase String
  fromSQL mbase = Email <$> fromSQL mbase
instance ToSQL Email where
  type PQDest Email = PQDest String
  toSQL (Email n) = toSQL n

instance FromJSON Email where
  parseJSON = fmap Email . parseJSON

instance ToJSON Email where
  toJSON = toJSON . unEmail

instance Unjson Email where
  unjsonDef = unjsonAeson

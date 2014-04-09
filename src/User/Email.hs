module User.Email (
    Email(..)
  ) where

import Control.Applicative
import DB

-- newtypes
newtype Email = Email { unEmail :: String }
  deriving (Eq, Ord, PQFormat)
$(newtypeDeriveUnderlyingReadShow ''Email)

instance FromSQL Email where
  type PQBase Email = PQBase String
  fromSQL mbase = Email <$> fromSQL mbase
instance ToSQL Email where
  type PQDest Email = PQDest String
  toSQL (Email n) = toSQL n

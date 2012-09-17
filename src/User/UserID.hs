module User.UserID (
    UserID
  , unsafeUserID
  ) where

import Data.Int
import Data.Typeable
import Happstack.Server

import DB.Derive
import Utils.Read

newtype UserID = UserID Int64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''UserID)
$(newtypeDeriveUnderlyingReadShow ''UserID)

instance FromReqURI UserID where
  fromReqURI = maybeRead

unsafeUserID :: Int64 -> UserID
unsafeUserID = UserID

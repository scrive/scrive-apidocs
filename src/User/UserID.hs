module User.UserID (
    UserID
  , unsafeUserID
  , unUserID
  ) where

import Data.Int
import Data.Typeable
import Happstack.Server
import Data.Binary

import DB.Derive
import Utils.Read

newtype UserID = UserID Int64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''UserID)
$(newtypeDeriveUnderlyingReadShow ''UserID)

instance FromReqURI UserID where
  fromReqURI = maybeRead

instance Binary UserID where
  put (UserID uid) = put uid
  get = fmap UserID get

unsafeUserID :: Int64 -> UserID
unsafeUserID = UserID

unUserID :: UserID -> Int64
unUserID (UserID i) = i
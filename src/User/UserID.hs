module User.UserID (
    UserID
  , unsafeUserID
  ) where

import Data.Int
import Data.SafeCopy
import Data.Typeable
import Happstack.Server

import DB.Derive
import Misc

newtype UserID = UserID Int64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''UserID)
$(newtypeDeriveUnderlyingReadShow ''UserID)

$(deriveSafeCopy 0 'base ''UserID)

instance FromReqURI UserID where
  fromReqURI = maybeRead

unsafeUserID :: Int64 -> UserID
unsafeUserID = UserID

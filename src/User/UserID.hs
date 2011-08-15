module User.UserID (
  UserID(..)
  ) where

import Data.Data
import Data.Int
import Happstack.Server
import Happstack.State
import Happstack.Util.Common

import DB.Derive

newtype UserID = UserID Int64
  deriving (Eq, Ord, Data, Typeable)
$(newtypeDeriveConvertible ''UserID)
$(newtypeDeriveUnderlyingReadShow ''UserID)

instance Version UserID where
  mode = extension 2 (Proxy :: Proxy UserID_0)

instance FromReqURI UserID where
  fromReqURI = readM

-- blah, migration --

newtype UserID_0 = UserID_0 Int
  deriving (Eq, Ord, Typeable)

instance Version UserID_0

instance Migrate UserID_0 UserID where
  migrate (UserID_0 n) = UserID (fromIntegral n)

$(deriveSerializeFor [''UserID, ''UserID_0])

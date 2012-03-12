module User.UserID (
    UserID
  , unsafeUserID
  ) where

import Control.Monad
import Data.Data
import Data.Int
import Happstack.Server
import Happstack.State
import Happstack.Util.Common

import Crypto.RNG
import DB.Derive

newtype UserID = UserID Int64
  deriving (Eq, Ord, Data, Typeable)
$(newtypeDeriveConvertible ''UserID)
$(newtypeDeriveUnderlyingReadShow ''UserID)

instance Random UserID where
  random = UserID `liftM` randomR (10000000, 10000000000)

instance Version UserID where
  mode = extension 2 (Proxy :: Proxy UserID_0)

instance FromReqURI UserID where
  fromReqURI = readM

unsafeUserID :: Int64 -> UserID
unsafeUserID = UserID

-- blah, migration --

newtype UserID_0 = UserID_0 Int
  deriving (Eq, Ord, Typeable)

instance Version UserID_0

instance Migrate UserID_0 UserID where
  migrate (UserID_0 n) = UserID (fromIntegral n)

$(deriveSerializeFor [''UserID, ''UserID_0])

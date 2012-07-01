module User.UserID (
    UserID
  , unsafeUserID
  ) where

import Control.Monad
import Data.Int
import Data.SafeCopy
import Data.Typeable
import Happstack.Server

import Crypto.RNG
import DB.Derive
import Misc

newtype UserID = UserID Int64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveConvertible ''UserID)
$(newtypeDeriveUnderlyingReadShow ''UserID)

$(deriveSafeCopy 0 'base ''UserID)

instance Random UserID where
  random = UserID `liftM` randomR (10000000, 10000000000)

instance FromReqURI UserID where
  fromReqURI = readM

unsafeUserID :: Int64 -> UserID
unsafeUserID = UserID

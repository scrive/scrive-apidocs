module User.UserID (
  UserID(..)
  ) where

import Data.Int
import Happstack.Server
import Happstack.Util.Common

import DB.Derive

newtype UserID = UserID Int64
  deriving (Eq, Ord)
$(newtypeDeriveConvertible ''UserID)
$(newtypeDeriveUnderlyingReadShow ''UserID)

instance FromReqURI UserID where
  fromReqURI = readM

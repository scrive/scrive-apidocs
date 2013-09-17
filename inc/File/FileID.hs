module File.FileID (
    FileID
  , unsafeFileID
  ) where

import Data.Int
import Happstack.Server

import DB.Derive
import Utils.Read
import Data.Typeable

newtype FileID = FileID Int64
  deriving (Eq, Ord, Typeable)
$(newtypeDeriveUnderlyingReadShow ''FileID)
$(newtypeDeriveConvertible ''FileID)

instance FromReqURI FileID where
  fromReqURI = maybeRead

unsafeFileID :: Int64 -> FileID
unsafeFileID = FileID

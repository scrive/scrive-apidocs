module File.FileID (
    FileID
  , unsafeFileID
  ) where

import Control.Monad
import Data.Int
import Happstack.Server
import Happstack.Util.Common

import Crypto.RNG
import DB.Derive

newtype FileID = FileID Int64
  deriving (Eq, Ord)
$(newtypeDeriveUnderlyingReadShow ''FileID)
$(newtypeDeriveConvertible ''FileID)

instance Random FileID where
  random = FileID `liftM` randomR (10000000, 10000000000)

instance FromReqURI FileID where
  fromReqURI = readM

unsafeFileID :: Int64 -> FileID
unsafeFileID = FileID

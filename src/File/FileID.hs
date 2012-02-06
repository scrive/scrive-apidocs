module File.FileID
    ( FileID(..)
    )
where

import DB.Derive
import Happstack.Data
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Data.Int

newtype FileID = FileID { unFileID :: Int64 }
    deriving (Eq, Ord, Typeable)

$(newtypeDeriveUnderlyingReadShow ''FileID)

instance FromReqURI FileID where
    fromReqURI = readM

$(deriveSerialize ''FileID)
instance Version FileID where

$(newtypeDeriveConvertible ''FileID)

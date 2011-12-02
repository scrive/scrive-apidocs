module File.FileID
    ( FileID(..)
    )
where

import DB.Derive
import Happstack.Data
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common
import Numeric
import Data.Int

newtype FileID = FileID { unFileID :: Int64 }
    deriving (Eq, Ord, Typeable)

instance Show FileID where
    showsPrec prec (FileID val) = showsPrec prec val

instance Read FileID where
    readsPrec _prec = let make (i,v) = (FileID i,v)
                      in map make . readSigned readDec

instance FromReqURI FileID where
    fromReqURI = readM

$(deriveSerialize ''FileID)
instance Version FileID where

$(newtypeDeriveConvertible ''FileID)

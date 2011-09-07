module Doc.FileID
where

import Happstack.Data
import Happstack.Server.SimpleHTTP
import Happstack.Util.Common

newtype FileID = FileID { unFileID :: Int }
    deriving (Eq, Ord, Typeable)

instance Show FileID where
    showsPrec prec (FileID val) = showsPrec prec val

instance Read FileID where
    readsPrec prec = let make (i,v) = (FileID i,v)
                     in map make . readsPrec prec

instance FromReqURI FileID where
    fromReqURI = readM

$(deriveSerialize ''FileID)
instance Version FileID where


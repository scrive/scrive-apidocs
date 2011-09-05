module User.OldSystemServer
    ( SystemServer(..)
    , systemServerFromURL

) where

import Data.Data
import Happstack.Data hiding (defaultValue)
import Data.List
import DB.Derive

data SystemServer = SkrivaPa | Scrive
    deriving (Bounded, Enum, Show, Read, Ord, Eq, Typeable)
$(enumDeriveConvertible ''SystemServer)
instance Version SystemServer

systemServerFromURL :: String -> SystemServer
systemServerFromURL url = if ("localhost" `isInfixOf` url)
                            then Scrive
                            else SkrivaPa



$(deriveSerializeFor [ ''SystemServer  ])



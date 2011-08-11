module User.SystemServer
    ( SystemServer(..)
    , systemServerFromURL

) where

import Data.Data
import Happstack.Data hiding (defaultValue)
import Data.List


data SystemServer = SkrivaPa | Scrive
    deriving (Bounded, Enum, Show, Read, Ord, Eq, Typeable)
    
instance Version SystemServer

systemServerFromURL :: String -> SystemServer
systemServerFromURL url =  if ("scrive" `isInfixOf` url)
                             then Scrive
                             else SkrivaPa
 


$(deriveSerializeFor [ ''SystemServer  ])



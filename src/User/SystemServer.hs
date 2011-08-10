module User.SystemServer (
    SystemServer(..)
  , systemServerFromURL
  ) where

import DB.Derive

data SystemServer = SkrivaPa | Scrive
  deriving (Bounded, Enum, Show, Read, Ord, Eq)
$(enumDeriveConvertible ''SystemServer)

systemServerFromURL :: String -> SystemServer
systemServerFromURL _url = SkrivaPa
{-  if "localhost" `isInfixOf` url
     then Scrive
     else SkrivaPa
-}

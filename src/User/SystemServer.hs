module User.SystemServer (
    SystemServer(..)
  , systemServerFromURL
  ) where

import Data.List

import DB.Derive

data SystemServer = SkrivaPa | Scrive
  deriving (Bounded, Enum, Show, Read, Ord, Eq)
$(enumDeriveConvertible ''SystemServer)

systemServerFromURL :: String -> SystemServer
systemServerFromURL url =  if ("scrive" `isInfixOf` url)
                             then Scrive
                             else SkrivaPa




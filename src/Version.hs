module Version (versionID) where

import Data.Version (Version(..))

import qualified Paths_kontrakcja as Paths

import Data.List

versionID :: String
versionID = concat $ intersperse "." $ versionTags Paths.version

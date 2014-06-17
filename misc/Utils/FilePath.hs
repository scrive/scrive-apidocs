module Utils.FilePath ( takeBaseName
                      ) where

import qualified System.FilePath.Posix as PathPosix
import qualified System.FilePath.Windows as PathWindows

takeBaseName :: FilePath -> String
takeBaseName = PathWindows.takeBaseName . PathPosix.takeBaseName

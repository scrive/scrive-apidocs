module Utils.Directory where

import Control.Monad
import Control.Monad.Trans.Control
import System.Directory
import System.IO.Temp
import System.Time

withSystemTempDirectory' :: MonadBaseControl IO m => String -> (FilePath -> m a) -> m a
withSystemTempDirectory' dir handler =
  control $ \runInIO -> withSystemTempDirectory dir (runInIO . handler)

-- Check recursively time of modification of any file in directory
getRecursiveMTime :: [Char] -> IO ClockTime
getRecursiveMTime "."  = return $ TOD 0 0
getRecursiveMTime ".." = return $ TOD 0 0
getRecursiveMTime dir = do
  isDir <- doesDirectoryExist dir
  if not isDir
    then getRecursiveMTime dir
    else do
      files <- getDirectoryContents dir
      mts <- forM files $ \fn -> getModificationTime $ dir ++ "/" ++fn
      mt <- getModificationTime dir
      return $ maximum $ mt : mts

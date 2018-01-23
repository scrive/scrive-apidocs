module Utils.Directory where

import Control.Monad.Trans.Control
import System.IO.Temp

withSystemTempDirectory' :: MonadBaseControl IO m => String -> (FilePath -> m a) -> m a
withSystemTempDirectory' dir handler =
  control $ \runInIO -> withSystemTempDirectory dir (runInIO . handler)

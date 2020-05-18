module UserGroup.FolderListCallsTransition
  ( defaultUseFolderListCalls
  , enableFolderListCallsForDefaultUserGroupSettings
  ) where

import Data.IORef
import System.IO.Unsafe

-- A giant hack for gradual transition to folder list calls without explictly
-- passing the parameters everywhere as it's a pain.
--
-- Will be removed after transition is done.

globalVar :: IORef Bool
globalVar = unsafePerformIO $ newIORef False
{-# NOINLINE globalVar #-}

defaultUseFolderListCalls :: Bool
defaultUseFolderListCalls = unsafePerformIO $ readIORef globalVar
{-# NOINLINE defaultUseFolderListCalls #-}

-- | Invoke at startup of the application for 'defaultUserGroupSettings' to have
-- 'useFolderListCalls' field set to 'True'.
enableFolderListCallsForDefaultUserGroupSettings :: IO ()
enableFolderListCallsForDefaultUserGroupSettings = writeIORef globalVar True

{-# OPTIONS_GHC -fno-warn-orphans #-}
module AppState (
    AppState
  , startAcidStateSystem
  , stopAcidStateSystem
  , createStateCheckpoint
  ) where

import Control.Monad
import Data.Acid
import System.FilePath
import qualified Data.IxSet as I

import Acid.Monad
import Session

data AppState = AppState {
  asSessions :: AcidState Sessions
}

instance AcidStore AppState m => HasAcidState Sessions m where
  getAcidState = asSessions `liftM` getAcidStore

startAcidStateSystem :: (String -> IO ()) -> FilePath -> IO AppState
startAcidStateSystem logger path = do
  let sessionsPath = path </> "sessions"
  logger $ "Using store for Sessions: " ++ sessionsPath
  sessions <- openLocalStateFrom sessionsPath I.empty
  return AppState {
    asSessions = sessions
  }

stopAcidStateSystem :: (String -> IO ()) -> AppState -> IO ()
stopAcidStateSystem logger AppState{..} = do
  logger $ "Creating checkpoint before exit..."
  createCheckpoint asSessions
  logger $ "Closing acid-state system..."
  closeAcidState asSessions

createStateCheckpoint :: AppState -> IO ()
createStateCheckpoint AppState{..} = do
  createCheckpoint asSessions

{-# OPTIONS_GHC -fno-warn-orphans #-}
module AppState (
    AppState
  , openAcidState
  , closeAcidState
  , createCheckpoint
  ) where

import Control.Monad
import Data.Acid (AcidState)
import System.FilePath
import qualified Data.Acid as ACID
import qualified Data.IxSet as I

import Acid.Monad
import Session

data AppState = AppState {
  asSessions :: AcidState Sessions
}

instance AcidStore AppState m => HasAcidState Sessions m where
  getAcidState = asSessions `liftM` getAcidStore

openAcidState :: (String -> IO ()) -> FilePath -> IO AppState
openAcidState logger path = do
  let sessionsPath = path </> "sessions"
  logger $ "Using store for Sessions: " ++ sessionsPath
  sessions <- ACID.openLocalStateFrom sessionsPath I.empty
  return AppState {
    asSessions = sessions
  }

closeAcidState :: (String -> IO ()) -> AppState -> IO ()
closeAcidState logger AppState{..} = do
  logger $ "Closing acid-state system..."
  ACID.closeAcidState asSessions

createCheckpoint :: (String -> IO ()) -> AppState -> IO ()
createCheckpoint logger AppState{..} = do
  logger $ "Creating checkpoint..."
  ACID.createCheckpoint asSessions

module ForkAction (forkAction) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Database.PostgreSQL.PQTypes
import Numeric
import System.Time
import qualified Control.Concurrent.Lifted as C
import qualified Control.Exception.Lifted as E

import qualified Log

showDiffTime :: ClockTime -> ClockTime -> String
showDiffTime clock1 clock2 = showFFloat (Just 2) (fromIntegral (diffClockTimeMiliSeconds clock1 clock2) / (100 :: Double)) ""
  where
    diffClockTimeMiliSeconds (TOD sec1 pico1) (TOD sec2 pico2) = ((sec1 * 1000000000000 + pico1) - (sec2 * 1000000000000 + pico2)) `div` 10000000000

forkAction :: (MonadBaseControl IO m, Log.MonadLog m, MonadDB m, MonadIO m) => String -> m () -> m ()
forkAction title action = void . C.fork . withNewConnection $ do
  Log.mixlog_ $ "forkAction: " ++ title ++ " started"
  startTime <- liftIO getClockTime
  result <- E.try action
  endTime <- liftIO getClockTime
  case result of
    Left (e :: E.SomeException) -> do
      rollback
      Log.mixlog_ $ "forkAction: " ++ title ++ " finished in " ++ showDiffTime endTime startTime ++ "s with exception " ++ show e
    Right _ -> do
      commit
      Log.mixlog_ $ "forkAction: " ++ title ++ " finished in " ++ showDiffTime endTime startTime ++ "s"

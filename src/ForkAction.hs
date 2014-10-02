module ForkAction (forkAction) where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Time
import Database.PostgreSQL.PQTypes
import qualified Control.Concurrent.Lifted as C
import qualified Control.Exception.Lifted as E

import qualified Log

forkAction :: (MonadBaseControl IO m, Log.MonadLog m, MonadDB m) => String -> m () -> m ()
forkAction title action = void . C.fork . withNewConnection $ do
  Log.mixlog_ $ "forkAction: " ++ title ++ " started"
  startTime <- liftBase getCurrentTime
  result <- E.try action
  endTime <- liftBase getCurrentTime
  case result of
    Left (e :: E.SomeException) -> do
      rollback
      Log.mixlog_ $ "forkAction: " ++ title ++ " finished in " ++ show (diffUTCTime endTime startTime) ++ " with exception " ++ show e
    Right _ -> do
      commit
      Log.mixlog_ $ "forkAction: " ++ title ++ " finished in " ++ show (diffUTCTime endTime startTime)

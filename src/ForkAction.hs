module ForkAction (forkAction) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Database.HDBC
import Numeric
import System.Time
import qualified Control.Concurrent.Lifted as C
import qualified Control.Exception.Lifted as E

import DB
import qualified Log

showDiffTime :: ClockTime -> ClockTime -> String
showDiffTime clock1 clock2 = showFFloat (Just 2) (fromIntegral (diffClockTimeMiliSeconds clock1 clock2) / (100 :: Double)) ""
  where
    diffClockTimeMiliSeconds (TOD sec1 pico1) (TOD sec2 pico2) = ((sec1 * 1000000000000 + pico1) - (sec2 * 1000000000000 + pico2)) `div` 10000000000

forkAction :: (MonadBaseControl IO m, MonadDB m, MonadIO m) => String -> m () -> m ()
forkAction title action = do
  -- OMG, this logic of kClone, disconnect and friends should be put directly into
  -- MonadbaseControl instance of DBT. getNexus and localNexus should go away, btw.
  -- As this requires more type level hackery than available brain power allows
  -- we will keep this hack below for now.
  nex <- kClone
  _ <- C.fork $ flip E.finally (liftIO $ disconnect nex) $ localNexus (const nex) $ do
    Log.debug $ "forkAction: " ++ title ++ " started"
    startTime <- liftIO getClockTime
    result <- E.try action
    endTime <- liftIO getClockTime
    case result of
      Left (e :: E.SomeException) -> do
        kRollback
        Log.debug $ "forkAction: " ++ title ++ " finished in " ++ showDiffTime endTime startTime ++ "s with exception " ++ show e
        return ()
      Right _ -> do
        kCommit
        Log.debug $ "forkAction: " ++ title ++ " finished in " ++ showDiffTime endTime startTime ++ "s"
        return ()
  return ()

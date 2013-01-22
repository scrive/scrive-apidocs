module ForkAction (forkAction) where

import Control.Concurrent.MVar
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Database.HDBC
import Numeric
import System.IO.Unsafe
import System.Time
import qualified Control.Concurrent.Lifted as C
import qualified Control.Exception.Lifted as E
import qualified Data.Map as Map

import DB
import qualified Log

data ForkedAction = ForkedActionStarted  {
    title     :: String
  , start     :: ClockTime
  }
  | ForkedActionDone {
    title     :: String
  , start     :: ClockTime
  , finish    :: ClockTime
  }
  | ForkedActionError {
    title     :: String
  , start     :: ClockTime
  , finish    :: ClockTime
  , exception :: E.SomeException
  }

instance Show ForkedAction where
  show (ForkedActionStarted {title}) = title
  show (ForkedActionDone {title, start, finish}) = title ++ ", done (" ++ showDiffTime finish start ++ "s)"
  show (ForkedActionError {title, start, finish, exception}) = title ++ ", error (" ++ showDiffTime finish start ++ "s): " ++ show exception

showDiffTime :: ClockTime -> ClockTime -> String
showDiffTime clock1 clock2 = showFFloat (Just 2) (fromIntegral (diffClockTimeMiliSeconds clock1 clock2) / (100 :: Double)) ""
  where
    diffClockTimeMiliSeconds (TOD sec1 pico1) (TOD sec2 pico2) = ((sec1 * 1000000000000 + pico1) - (sec2 * 1000000000000 + pico2)) `div` 10000000000

allActions :: MVar (Map.Map Int ForkedAction)
allActions = unsafePerformIO $ newMVar Map.empty

forkAction :: (MonadBaseControl IO m, MonadDB m) => String -> m () -> m ()
forkAction title action = do
  nex <- kClone
  _ <- C.fork $ flip E.finally (liftIO $ disconnect nex) $ localNexus (const nex) $ do
    startTime <- liftIO getClockTime
    key <- liftIO $ modifyMVar allActions $ \themap -> do
      let newkey = case Map.maxViewWithKey themap of
            Nothing -> 0
            Just ((k,_),_) -> k + 1
      return (Map.insert newkey (ForkedActionStarted title startTime) themap, newkey)
    result <- E.try action
    endTime <- liftIO getClockTime
    retval <- case result of
      Left e -> do
        kRollback
        Log.error $ "forkAction: " ++ title ++ ": " ++ show e
        return $ ForkedActionError title startTime endTime e
      Right _ -> do
        kCommit
        return $ ForkedActionDone title startTime endTime
    liftIO $ modifyMVar_ allActions $ return . Map.insert key retval
  return ()

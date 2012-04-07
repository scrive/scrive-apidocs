
module ForkAction 
    ( forkActionIO
    , forkAction
    ) where

import KontraMonad

import Control.Monad.Trans
import Prelude hiding (error)
import qualified Control.Concurrent as C
import qualified Control.Exception as C
import qualified Data.Map as Map
import System.IO.Unsafe
import Control.Concurrent.MVar
import System.Time
import Numeric
import qualified Database.HDBC as HDBC
import DB.Classes (DBEnv, cloneDBEnv, envNexus)
import Context
import qualified Log

data ForkedAction = ForkedActionStarted  { title     :: String
                                         , start     :: ClockTime
                                         }
                  | ForkedActionDone     { title     :: String
                                         , start     :: ClockTime
                                         , finish    :: ClockTime
                                         }
                  | ForkedActionError    { title     :: String
                                         , start     :: ClockTime
                                         , finish    :: ClockTime
                                         , exception :: C.SomeException
                                         }

instance Show ForkedAction where
    show (ForkedActionStarted {title}) = title
    show (ForkedActionDone {title, start, finish}) = title ++ ", done (" ++ showDiffTime finish start ++ "s)"
    show (ForkedActionError {title, start, finish, exception}) = title ++ ", error (" ++ showDiffTime finish start ++ "s): " ++ show exception


diffClockTimeMiliSeconds :: ClockTime -> ClockTime -> Integer
diffClockTimeMiliSeconds (TOD sec1 pico1) (TOD sec2 pico2) = 
    ((sec1 * 1000000000000 + pico1) - (sec2 * 1000000000000 + pico2)) `div` 10000000000

showDiffTime :: ClockTime -> ClockTime -> String
showDiffTime clock1 clock2 = showFFloat (Just 2) (fromIntegral (diffClockTimeMiliSeconds clock1 clock2) / (100 :: Double)) ""

allActions :: MVar (Map.Map Int ForkedAction)
allActions = unsafePerformIO $ newMVar Map.empty

-- | Use only in IO monad and when you know that forked action won't
-- try to get access to db using 'Connection' object taken from 'Context'.
-- Preferable use 'clone' on 'Connection' and the pass it around.
forkActionIO :: DBEnv -> String -> (DBEnv -> IO ()) -> IO ()
forkActionIO env title' action = do
  env' <- cloneDBEnv env
  let conn' = envNexus env'
  _ <- C.forkIO $ flip C.finally (HDBC.disconnect conn') $ do
    startTime <- getClockTime
    key <- modifyMVar allActions $ \themap -> do
      let newkey = case Map.maxViewWithKey themap of
            Nothing -> 0
            Just ((k,_),_) -> k + 1
      return (Map.insert newkey (ForkedActionStarted title' startTime) themap, newkey)
    result <- C.try (action env')
    endTime <- getClockTime
    case result of
      Left someException -> do
        Log.error $ "forkActionIO: " ++ title' ++ ": " ++ show someException
        modifyMVar_ allActions $ return . Map.insert key (ForkedActionError title' startTime endTime someException)
      Right _ ->
        modifyMVar_ allActions $ return . Map.insert key (ForkedActionDone title' startTime endTime)
  return ()

-- | Standard forkAction to be used in KontraMonad. Setting ctxdbconnclose
-- to False guarantees that Connection won't be closed after we're done with
-- the request that called that function, so you can safely use Connection
-- object taken from current Context there.
forkAction :: (KontraMonad m, MonadIO m) => String -> (DBEnv -> IO ()) -> m ()
forkAction title action = do
  ctx <- getContext
  liftIO $ forkActionIO (ctxdbenv ctx) title action


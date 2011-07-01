
module ForkAction 
    ( forkAction
    , getAllActionAsString
    ) where

import Control.Monad.Trans
import Prelude hiding (error)
import qualified Control.Concurrent as C
import qualified Control.Exception as C
import qualified Data.Map as Map
import System.IO.Unsafe
import Control.Concurrent.MVar
import System.Time
import Numeric

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


showInTime :: ClockTime -> ForkedAction -> String
showInTime current (ForkedActionStarted {title, start}) = title ++ ",  (" ++ showDiffTime current start ++ "s)"
showInTime _current (ForkedActionDone {title, start, finish}) = title ++ ", done (" ++ showDiffTime finish start ++ "s)"
showInTime _current (ForkedActionError {title, start, finish, exception}) = title ++ ", error (" ++ showDiffTime finish start ++ "s): " ++ show exception

diffClockTimeMiliSeconds :: ClockTime -> ClockTime -> Integer
diffClockTimeMiliSeconds (TOD sec1 pico1) (TOD sec2 pico2) = 
    ((sec1 * 1000000000000 + pico1) - (sec2 * 1000000000000 + pico2)) `div` 10000000000

showDiffTime :: ClockTime -> ClockTime -> String
showDiffTime clock1 clock2 = showFFloat (Just 2) (fromIntegral (diffClockTimeMiliSeconds clock1 clock2) / (100 :: Double)) ""

allActions :: MVar (Map.Map Int ForkedAction)
allActions = unsafePerformIO $ newMVar Map.empty


forkAction :: (MonadIO m) => String -> IO () -> m ()
forkAction title' action =
  liftIO $ do
    _ <- C.forkIO $ do
               startTime <- getClockTime
               key <- modifyMVar allActions $ \themap -> do
                      let newkey = case Map.maxViewWithKey themap of
                                       Nothing -> 0
                                       Just ((k,_),_) -> k + 1
                      return (Map.insert newkey (ForkedActionStarted title' startTime) themap, newkey)
               result <- C.try action
               endTime <- getClockTime
               case result of
                   Left someException -> 
                       modifyMVar_ allActions $ return . Map.insert key (ForkedActionError title' startTime endTime someException)
                   Right _ -> 
                       modifyMVar_ allActions $ return . Map.insert key (ForkedActionDone title' startTime endTime)
    return ()

getAllActionAsString :: IO String
getAllActionAsString = 
    withMVar allActions $ \actions -> do
        current <- getClockTime
        return $ unlines $ map (showInTime current) $ Map.elems actions

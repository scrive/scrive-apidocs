module DB.Utils where

import Data.Int
import Database.HDBC
import System.Random
import qualified Control.Exception as E

import DB.Classes
import DB.Model
import Happstack.State()
import Control.Monad.IO.Class
import Control.Monad

getUniqueID :: Table -> DB Int64
getUniqueID table = do
  muid <- wrapDB $ \conn -> do
    uid <- randomRIO (0, maxBound)
    st <- prepare conn $ "SELECT id FROM " ++ tblName table ++ " WHERE id = ?"
    fetchRow st >>= return . maybe (Just uid) (const Nothing)
  maybe (getUniqueID table) return muid

oneRowAffectedGuard :: Monad m => Integer -> m Bool
oneRowAffectedGuard 0 = return False
oneRowAffectedGuard 1 = return True
oneRowAffectedGuard n =
  E.throw TooManyObjects {
      tmoExpected = 1
    , tmoGiven = n
  }

oneObjectReturnedGuard :: Monad m => [a] -> m (Maybe a)
oneObjectReturnedGuard []  = return Nothing
oneObjectReturnedGuard [x] = return $ Just x
oneObjectReturnedGuard xs  =
  E.throw TooManyObjects {
      tmoExpected = 1
    , tmoGiven = fromIntegral $ length xs
  }

checkIfOneObjectReturned :: Monad m => [a] -> m Bool
checkIfOneObjectReturned xs =
  oneObjectReturnedGuard xs
    >>= return . maybe False (const True)

fetchValues :: (MonadIO m) => Statement -> ([SqlValue] -> Maybe a) -> m [a]
fetchValues st decode = liftM reverse (worker [])
  where
    worker acc = do
      mrow <- liftIO $ fetchRow st
      case mrow of
        Nothing -> return acc
        Just row -> 
          case decode row of
            Just value -> worker (value : acc)
            Nothing -> do
                   liftIO $ finish st
                   liftIO $ E.throwIO CannotParseRow


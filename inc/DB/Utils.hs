module DB.Utils (
    getOne
  , oneRowAffectedGuard
  , oneObjectReturnedGuard
  , exactlyOneObjectReturnedGuard
  , checkIfOneObjectReturned
  , checkIfAnyReturned
  ) where

import Control.Monad.IO.Class
import Data.Convertible
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Database.HDBC.SqlValue
import qualified Control.Exception as E

import DB.Core
import DB.Env
import DB.Exception
import DB.Fetcher
import DB.Functions
import DB.SQL (IsSQL)

oneRowAffectedGuard :: MonadIO m => Integer -> m Bool
oneRowAffectedGuard 0 = return False
oneRowAffectedGuard 1 = return True
oneRowAffectedGuard n = liftIO $ E.throwIO TooManyObjects {
    originalQuery = mempty
  , tmoExpected = 1
  , tmoGiven = n
  }

oneObjectReturnedGuard :: MonadIO m => [a] -> m (Maybe a)
oneObjectReturnedGuard []  = return Nothing
oneObjectReturnedGuard [x] = return $ Just x
oneObjectReturnedGuard xs  = liftIO $ E.throwIO TooManyObjects {
    originalQuery = mempty
  , tmoExpected = 1
  , tmoGiven = fromIntegral $ length xs
  }

exactlyOneObjectReturnedGuard :: forall a m. (MonadIO m, Typeable a) => [a] -> m a
exactlyOneObjectReturnedGuard xs = oneObjectReturnedGuard xs
  >>= maybe (liftIO . E.throwIO $ NoObject (show $ typeOf (undefined::a))) return

checkIfOneObjectReturned :: MonadIO m => [a] -> m Bool
checkIfOneObjectReturned xs = oneObjectReturnedGuard xs
  >>= return . maybe False (const True)

getOne :: (IsSQL sql, MonadDB m) => Convertible SqlValue a => sql -> DBEnv m (Maybe a)
getOne sql = do
  _ <- kRun sql
  foldDB (\acc v -> v : acc) []
    >>= oneObjectReturnedGuard

checkIfAnyReturned :: forall m sql. (IsSQL sql, MonadDB m) => sql -> DBEnv m Bool
checkIfAnyReturned sql =
  (getOne sql :: DBEnv m (Maybe SqlValue))
    >>= checkIfOneObjectReturned . maybeToList

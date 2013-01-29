module DB.Utils (
    getMany
  , getOne
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

class OneObjectReturnedGuard f where
  exactlyOneObjectReturnedGuard :: (MonadIO m, Typeable a) => f a -> m a

instance OneObjectReturnedGuard Maybe where
  exactlyOneObjectReturnedGuard = impl
    where
      impl :: forall a m. (MonadIO m, Typeable a) => Maybe a -> m a
      impl = maybe
        (liftIO . E.throwIO $ NoObject (show $ typeOf (undefined::a)))
        return

instance OneObjectReturnedGuard [] where
  exactlyOneObjectReturnedGuard xs =
    oneObjectReturnedGuard xs
      >>= exactlyOneObjectReturnedGuard

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

checkIfOneObjectReturned :: MonadIO m => [a] -> m Bool
checkIfOneObjectReturned xs = oneObjectReturnedGuard xs
  >>= return . maybe False (const True)

getMany :: (IsSQL sql, MonadDB m) => Convertible SqlValue a => sql -> DBEnv m [a]
getMany s = do
  _ <- kRun s
  kFold (\acc v -> v : acc) []

getOne :: (IsSQL sql, MonadDB m) => Convertible SqlValue a => sql -> DBEnv m (Maybe a)
getOne s = getMany s >>= oneObjectReturnedGuard

checkIfAnyReturned :: forall m sql. (IsSQL sql, MonadDB m) => sql -> DBEnv m Bool
checkIfAnyReturned s =
  (getOne s :: DBEnv m (Maybe SqlValue))
    >>= checkIfOneObjectReturned . maybeToList

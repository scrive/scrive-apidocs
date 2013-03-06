module DB.Utils (
    getMany
  , getOne
  , oneRowAffectedGuard
  , oneObjectReturnedGuard
  , exactlyOneObjectReturnedGuard
  , checkIfOneObjectReturned
  , checkIfAnyReturned
  ) where

import Data.Convertible
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Database.HDBC.SqlValue

import DB.Core

import DB.Exception
import DB.Fetcher
import DB.Functions
import DB.SQL (IsSQL)

class OneObjectReturnedGuard f where
  exactlyOneObjectReturnedGuard :: (MonadDB m, Typeable a) => f a -> m a

instance OneObjectReturnedGuard Maybe where
  exactlyOneObjectReturnedGuard = impl
    where
      impl :: forall a m. (MonadDB m, Typeable a) => Maybe a -> m a
      impl = maybe
        (kThrow $ NoObject (show $ typeOf (undefined::a)))
        return

instance OneObjectReturnedGuard [] where
  exactlyOneObjectReturnedGuard xs =
    oneObjectReturnedGuard xs
      >>= exactlyOneObjectReturnedGuard

oneRowAffectedGuard :: MonadDB m => Integer -> m Bool
oneRowAffectedGuard 0 = return False
oneRowAffectedGuard 1 = return True
oneRowAffectedGuard n = kThrow TooManyObjects {
    originalQuery = mempty
  , tmoExpected = 1
  , tmoGiven = n
  }

oneObjectReturnedGuard :: MonadDB m => [a] -> m (Maybe a)
oneObjectReturnedGuard []  = return Nothing
oneObjectReturnedGuard [x] = return $ Just x
oneObjectReturnedGuard xs  = kThrow TooManyObjects {
    originalQuery = mempty
  , tmoExpected = 1
  , tmoGiven = fromIntegral $ length xs
  }

checkIfOneObjectReturned :: MonadDB m => [a] -> m Bool
checkIfOneObjectReturned xs = oneObjectReturnedGuard xs
  >>= return . maybe False (const True)

getMany :: (IsSQL sql, MonadDB m) => Convertible SqlValue a => sql -> m [a]
getMany s = do
  _ <- kRun s
  kFold (\acc v -> v : acc) []

getOne :: (IsSQL sql, MonadDB m) => Convertible SqlValue a => sql -> m (Maybe a)
getOne s = getMany s >>= oneObjectReturnedGuard

checkIfAnyReturned :: forall m sql. (IsSQL sql, MonadDB m) => sql -> m Bool
checkIfAnyReturned s =
  (getOne s :: m (Maybe SqlValue))
    >>= checkIfOneObjectReturned . maybeToList

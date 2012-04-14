module DB.Utils (
    getOne
  , oneRowAffectedGuard
  , oneObjectReturnedGuard
  , checkIfOneObjectReturned
  , checkIfAnyReturned
  ) where

import Data.Convertible
import Data.Maybe
import Data.Monoid
import Database.HDBC.SqlValue
import qualified Control.Exception as E

import DB.Core
import DB.Env
import DB.Exception
import DB.Fetcher
import DB.Functions
import DB.SQL

oneRowAffectedGuard :: Monad m => Integer -> m Bool
oneRowAffectedGuard 0 = return False
oneRowAffectedGuard 1 = return True
oneRowAffectedGuard n = E.throw TooManyObjects {
    originalQuery = mempty
  , tmoExpected = 1
  , tmoGiven = n
  }

oneObjectReturnedGuard :: Monad m => [a] -> m (Maybe a)
oneObjectReturnedGuard []  = return Nothing
oneObjectReturnedGuard [x] = return $ Just x
oneObjectReturnedGuard xs  = E.throw TooManyObjects {
    originalQuery = mempty
  , tmoExpected = 1
  , tmoGiven = fromIntegral $ length xs
  }

checkIfOneObjectReturned :: Monad m => [a] -> m Bool
checkIfOneObjectReturned xs =
  oneObjectReturnedGuard xs
    >>= return . maybe False (const True)

getOne :: MonadDB m => Convertible SqlValue a => SQL -> DBEnv m (Maybe a)
getOne sqlq = do
  _ <- kRun sqlq
  foldDB (\acc v -> v : acc) []
    >>= oneObjectReturnedGuard

checkIfAnyReturned :: forall m. MonadDB m => SQL -> DBEnv m Bool
checkIfAnyReturned sqlq =
  (getOne sqlq :: DBEnv m (Maybe SqlValue))
    >>= checkIfOneObjectReturned . maybeToList

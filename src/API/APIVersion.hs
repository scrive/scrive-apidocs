module API.APIVersion
 ( APIVersion(..)
 ) where

import Database.PostgreSQL.PQTypes
import Data.Int
import qualified Control.Exception.Lifted as E
import Utils.Default

data APIVersion =
    V1
 |  V2
  deriving (Eq, Show, Ord)

instance HasDefaultValue APIVersion where
  defaultValue = V1

instance PQFormat APIVersion where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL APIVersion where
  type PQBase APIVersion = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1  -> return V1
      2  -> return V2
      _  -> E.throwIO $ RangeError {
        reRange = [(1, 2)]
      , reValue = n
      }

instance ToSQL APIVersion where
  type PQDest APIVersion = PQDest Int16
  toSQL V1 = toSQL (1::Int16)
  toSQL V2 = toSQL (2::Int16)
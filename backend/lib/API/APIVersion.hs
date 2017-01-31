module API.APIVersion
 ( APIVersion(..)
 ) where

import Data.Int
import Database.PostgreSQL.PQTypes
import qualified Control.Exception.Lifted as E

import KontraPrelude
import Log.Identifier

data APIVersion =
    V1
 |  V2
  deriving (Eq, Show, Ord)

instance Identifier APIVersion Int where
  gidentifier f n = f "api_version" .= fmap versionToInt n
    where
      versionToInt :: APIVersion -> Int
      versionToInt V1 = 1
      versionToInt V2 = 2

instance PQFormat APIVersion where
  pqFormat = const $ pqFormat (undefined::Int16)

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

module PadApplication.Data (
    PadAppMode(..)
  ) where

import Control.Monad.Catch
import Data.Int (Int16)
import Database.PostgreSQL.PQTypes

import KontraPrelude

-- PadAppMode describes, how a pad application is going to behave.
data PadAppMode =
    -- pad application shows a list of documents prepared for signing,
    -- the salesperson chooses a document and hands over to the customer to
    -- sign
    ListView
    -- pad application shows a field to enter 6-digit code, the salesperson
    -- enters a 6-digit document shortcode to select the document and hands over
    -- to the customer to sign
  | PinCode
  deriving (Eq, Ord, Show, Read)

instance PQFormat PadAppMode where
  pqFormat = const $ pqFormat (undefined::Int16)

instance FromSQL PadAppMode where
  type PQBase PadAppMode = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return ListView
      2 -> return PinCode
      _ -> throwM RangeError {
        reRange = [(1,2)]
      , reValue = n
      }

instance ToSQL PadAppMode where
  type PQDest PadAppMode = PQDest Int16
  toSQL ListView  = toSQL (1::Int16)
  toSQL PinCode   = toSQL (2::Int16)

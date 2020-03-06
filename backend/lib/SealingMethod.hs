module SealingMethod (
    SealingMethod(..)
  , sealingMethodText
  , sealingMethodFromText
  ) where

import Control.Monad.Catch
import Data.Int (Int16)
import Database.PostgreSQL.PQTypes

-- | SealingMethod describes, which digital signature method is used to seal a document.
data SealingMethod
    -- | The default method uses keyless signature infrastructure provided by Guardtime.
    = Guardtime
    -- | Pades method uses PKI-based signing.
    | Pades
  deriving (Eq, Ord, Show)

instance PQFormat SealingMethod where
  pqFormat = pqFormat @Int16

instance FromSQL SealingMethod where
  type PQBase SealingMethod = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return Guardtime
      2 -> return Pades
      _ -> throwM RangeError { reRange = [(1, 2)], reValue = n }

instance ToSQL SealingMethod where
  type PQDest SealingMethod = PQDest Int16
  toSQL Guardtime = toSQL (1 :: Int16)
  toSQL Pades     = toSQL (2 :: Int16)

sealingMethodText :: SealingMethod -> Text
sealingMethodText Guardtime = "guardtime"
sealingMethodText Pades     = "pades"

sealingMethodFromText :: Text -> Maybe SealingMethod
sealingMethodFromText s = find (\p -> s == sealingMethodText p) [Guardtime, Pades]

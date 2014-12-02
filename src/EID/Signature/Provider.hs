module EID.Signature.Provider (SignatureProvider(..)) where

import Control.Monad.Catch
import Data.Int
import Database.PostgreSQL.PQTypes

data SignatureProvider
  = LegacyBankID
  | LegacyTelia
  | LegacyNordea
  | LegacyMobileBankID
  | CgiGrpBankID
    deriving (Eq, Ord, Show)

instance PQFormat SignatureProvider where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL SignatureProvider where
  type PQBase SignatureProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return LegacyBankID
      2 -> return LegacyTelia
      3 -> return LegacyNordea
      4 -> return LegacyMobileBankID
      5 -> return CgiGrpBankID
      _ -> throwM RangeError {
        reRange = [(1, 5)]
      , reValue = n
      }

instance ToSQL SignatureProvider where
  type PQDest SignatureProvider = PQDest Int16
  toSQL LegacyBankID       = toSQL (1::Int16)
  toSQL LegacyTelia        = toSQL (2::Int16)
  toSQL LegacyNordea       = toSQL (3::Int16)
  toSQL LegacyMobileBankID = toSQL (4::Int16)
  toSQL CgiGrpBankID       = toSQL (5::Int16)

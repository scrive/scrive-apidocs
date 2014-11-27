module EID.Signature.Provider (
    CurrentSignatureProvider(..)
  , SignatureProvider(..)
  ) where

import Control.Monad.Catch
import Data.Int
import Database.PostgreSQL.PQTypes

data ObsoleteSignatureProvider
  = ObsoleteBankID
  | ObsoleteTelia
  | ObsoleteNordea
  | ObsoleteMobileBankID
    deriving (Eq, Ord, Show)

data CurrentSignatureProvider
  = CgiGrpBankID
    deriving (Eq, Ord, Show)

data SignatureProvider
  = Obsolete ObsoleteSignatureProvider
  | Current CurrentSignatureProvider
    deriving (Eq, Ord, Show)

instance PQFormat SignatureProvider where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL SignatureProvider where
  type PQBase SignatureProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return $ Obsolete ObsoleteBankID
      2 -> return $ Obsolete ObsoleteTelia
      3 -> return $ Obsolete ObsoleteNordea
      4 -> return $ Obsolete ObsoleteMobileBankID
      5 -> return $ Current CgiGrpBankID
      _ -> throwM RangeError {
        reRange = [(1, 5)]
      , reValue = n
      }

instance ToSQL SignatureProvider where
  type PQDest SignatureProvider = PQDest Int16
  toSQL (Obsolete ObsoleteBankID)       = toSQL (1::Int16)
  toSQL (Obsolete ObsoleteTelia)        = toSQL (2::Int16)
  toSQL (Obsolete ObsoleteNordea)       = toSQL (3::Int16)
  toSQL (Obsolete ObsoleteMobileBankID) = toSQL (4::Int16)
  toSQL (Current CgiGrpBankID)          = toSQL (5::Int16)

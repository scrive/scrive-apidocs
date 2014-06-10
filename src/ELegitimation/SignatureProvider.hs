module ELegitimation.SignatureProvider (
    SignatureProvider(..)
  ) where

import Data.Int
import Database.PostgreSQL.PQTypes
import Happstack.Server.SimpleHTTP
import qualified Control.Exception.Lifted as E

import Utils.Enum
import Utils.Read

data SignatureProvider =
    BankIDProvider
  | TeliaProvider
  | NordeaProvider -- Nordea is currently not used, but kept for legacy purposes.
  | MobileBankIDProvider
    deriving (Eq, Ord, Show)

instance FromReqURI SignatureProvider where
    fromReqURI = maybeRead

instance PQFormat SignatureProvider where
  pqFormat _ = pqFormat (undefined::Int16)

instance FromSQL SignatureProvider where
  type PQBase SignatureProvider = PQBase Int16
  fromSQL mbase = do
    n <- fromSQL mbase
    case n :: Int16 of
      1 -> return BankIDProvider
      2 -> return TeliaProvider
      3 -> return NordeaProvider
      4 -> return MobileBankIDProvider
      _ -> E.throwIO $ RangeError {
        reRange = [(1, 4)]
      , reValue = n
      }

instance ToSQL SignatureProvider where
  type PQDest SignatureProvider = PQDest Int16
  toSQL BankIDProvider       = toSQL (1::Int16)
  toSQL TeliaProvider        = toSQL (2::Int16)
  toSQL NordeaProvider       = toSQL (3::Int16)
  toSQL MobileBankIDProvider = toSQL (4::Int16)

instance Read SignatureProvider where
    readsPrec _ "bankid"       = [(BankIDProvider,"")]
    readsPrec _ "telia"        = [(TeliaProvider,"")]
    readsPrec _ "nordea"       = [(NordeaProvider,"")]
    readsPrec _ "mobilebankid" = [(MobileBankIDProvider,"")]
    readsPrec _ _              = []

instance SafeEnum SignatureProvider where
    fromSafeEnum BankIDProvider = 6
    fromSafeEnum TeliaProvider  = 5
    fromSafeEnum NordeaProvider = 4
    fromSafeEnum MobileBankIDProvider = 10

    toSafeEnum 6 = Just BankIDProvider
    toSafeEnum 5 = Just TeliaProvider
    toSafeEnum 4 = Just  NordeaProvider
    toSafeEnum 10 = Just MobileBankIDProvider
    toSafeEnum _ = Nothing

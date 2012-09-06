module ELegitimation.SignatureProvider (
    SignatureProvider(..)
  ) where

import Happstack.Server.SimpleHTTP

import DB.Derive
import Utils.Enum
import Utils.Read

{-# LANGUAGE CPP #-}
instance FromReqURI SignatureProvider where
    fromReqURI = maybeRead

instance Read SignatureProvider where
    readsPrec _ "bankid" = [(BankIDProvider,"")]
    readsPrec _ "telia"  = [(TeliaProvider,"")]   
    readsPrec _ "nordea" = [(NordeaProvider,"")]
    readsPrec _ "mobilebankid" = [(MobileBankIDProvider,"")]
    readsPrec _ _        = []

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

data SignatureProvider = BankIDProvider
                       | TeliaProvider
                       | NordeaProvider
                       | MobileBankIDProvider
    deriving (Eq, Ord, Show)

$(enumDeriveConvertible ''SignatureProvider)

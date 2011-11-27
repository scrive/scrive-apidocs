module ELegitimation.SignatureProvider (
    SignatureProvider(..)) 
    where

import Happstack.Data
import Misc
import Happstack.Server.SimpleHTTP
import DB.Derive

{-# LANGUAGE CPP #-}
instance FromReqURI SignatureProvider where
    fromReqURI = maybeRead
    
instance Read SignatureProvider where
    readsPrec _ "bankid" = [(BankIDProvider,"")]
    readsPrec _ "telia"  = [(TeliaProvider,"")]   
    readsPrec _ "nordea" = [(NordeaProvider,"")]
    readsPrec _ _        = []

instance SafeEnum SignatureProvider where
    fromSafeEnum BankIDProvider = 6
    fromSafeEnum TeliaProvider  = 5
    fromSafeEnum NordeaProvider = 4
    
    toSafeEnum 6 = Just BankIDProvider
    toSafeEnum 5 = Just TeliaProvider
    toSafeEnum 4 = Just  NordeaProvider
    toSafeEnum _ = Nothing
    

data SignatureProvider = BankIDProvider
                       | TeliaProvider
                       | NordeaProvider
    deriving (Eq, Ord, Typeable)

deriving instance Show SignatureProvider
$(deriveSerialize ''SignatureProvider)
instance Version SignatureProvider
$(enumDeriveConvertible ''SignatureProvider)

{-# LANGUAGE CPP, OverloadedStrings, TupleSections , OverlappingInstances , ViewPatterns  #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  PayEx.PayExRequest
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
--  Datatypes for generating requests to PayEx. PC is wrapper for requests, so we can pack it with 
--  some config or other data that PayEx requeires
-----------------------------------------------------------------------------
module PayEx.PayExRequest where
import "mtl" Control.Monad.State
import Payments.PaymentsState
import PayEx.PayExState
import PayEx.PayExConfig
import Text.XML.HaXml.XmlContent.Parser 
import User
import Happstack.Server
import Data.Hash.MD5
import Control.Category hiding ((.))
import Data.Monoid

-- | Request with configuration and something else wrapper 
newtype PC a = PC (PayExConfig,a) deriving Show

-- | REQUESTS
-- | PayExInit starts a request

data  PayExInit  = PayExInit Request Payment 

instance HTypeable (PC PayExInit) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PC PayExInit) where
    parseContents = error "Never serialize response"
    toContents (PC (config, PayExInit request payment)) =
          [CElem (Elem "Initialize7" [mkAttr "xmlns" "http://external.payex.com/PxOrder/"] $ 
                   fieldBox $  (setField "accountNumber" $ accountNumber config) >>>
                               (setField "purchaseOperation" "AUTHORIZATION") >>>
                               (setField "price" $ show $ money $ value payment) >>>
                               (setField "priceArgList" $ "") >>>
                               (setField "currency" $ "") >>>
                               (setField "vat" $ show $ vat config) >>>
                               (setField "orderID" $ show $ unPaymentId $ paymentId payment) >>>
                               (setField "productNumber" "0") >>>
                               (setField "description" "0") >>>
                               (setField "clientIPAddress" $ fst $ rqPeer $ request) >>>
                               (setField "clientIdentifier" $ "") >>>
                               (setField "additionalValues" $ "") >>>
                               (setField "externalID" $ "") >>>
                               (setField "returnUrl" $ returnUrl config) >>>
                               (setField "view" "CC") >>>
                               (setField "agreementRef" $ "") >>>
                               (setField "cancelUrl" $ "") >>>
                               (setField "clientLanguage" $ "") >>>
                               (addHash $ encryptionKey config)
                            ) ()]


-- | PayExComplete (our implementation) locks money for transaction, but not charges anyone
data  PayExComplete = PayExComplete Payment

instance HTypeable (PC PayExComplete) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PC PayExComplete) where
    parseContents = error "Never serialize response"
    toContents (PC (config, PayExComplete payment)) =
          [CElem (Elem "Complete" [mkAttr "xmlns" "http://external.payex.com/PxOrder/"] $ 
                   fieldBox $  (setField "accountNumber" $ accountNumber config) >>>
                               (setField "orderRef" $ orderRef payment) >>>
                               (addHash $ encryptionKey config)
                            ) ()]
                            
-- | PayExCapture (our implementation) gives us money we earlier locked
data PayExCapture = PayExCapture Payment

instance HTypeable (PC PayExCapture) where
    toHType _ = Defined "PayExCapture" [] []
    
instance XmlContent (PC PayExCapture) where
    parseContents = error "Never serialize response"
    toContents (PC (config, PayExCapture payment)) =
          [CElem (Elem "Complete" [mkAttr "xmlns" "http://external.payex.com/PxOrder/"] $ 
                   fieldBox $  (setField "accountNumber" $ accountNumber config) >>>
                               (setField "transactionNumber" $ accountNumber config) >>>
                               (setField "amount" $ orderRef payment) >>>
                               (setField "orderId" $ orderRef payment) >>>
                               (setField "vatAmount" $ orderRef payment) >>>
                               (setField "additionalValues" $ orderRef payment) >>>
                               (addHash $ encryptionKey config)
                            ) ()]


-- | PayExCancel (our implementation)  unlocks money. If we would do capture we could not cancel
data PayExCancel = PayExCancel Payment

instance HTypeable (PC PayExCancel) where
    toHType _ = Defined "PayExCapture" [] []
    
instance XmlContent (PC PayExCancel) where
    parseContents = error "Never serialize response"
    toContents (PC (config, PayExCancel payment)) =
          [CElem (Elem "Complete" [mkAttr "xmlns" "http://external.payex.com/PxOrder/"] $ 
                   fieldBox $  (setField "accountNumber" $ accountNumber config) >>>
                               (setField "transactionNumber" $ orderRef payment) >>>
                               (addHash $ encryptionKey config)
                            ) ()]


-- | Wrapping from content
toPC::a -> Kontra (PC a)
toPC a = do
          pc <- liftIO $ getPayExConfig
          return $ PC (pc,a)



-- |  For each request we need to generate hash, and this constructs lets us do that in quite simple way
fieldBox :: (Monoid a) => (a -> t) -> t
fieldBox f = f mempty

setField::String->String->([Content ()],String) -> ([Content ()],String)
setField n v (l,vs)= (l ++ [mkElemC n (toText v)], vs ++ v)

addHash::String -> ([Content ()],String) -> [Content ()] 
addHash hash (l,vs)= l ++ [mkElemC "hash" (toText $ md5s $ Str $ vs ++ hash)]


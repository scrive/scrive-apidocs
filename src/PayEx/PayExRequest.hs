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
import Control.Monad.State
import Payments.PaymentsState
import PayEx.PayExState
import PayEx.PayExConfig
import Text.XML.HaXml.XmlContent.Parser  hiding (content)
import Kontra
import Happstack.Server
import Data.Hash.MD5 hiding (md5)
import Control.Category hiding ((.))
import Data.Monoid
import KontraLink
import Data.ByteString (unpack)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Word
import Data.List
import Data.Bits
import Data.Encoding
import Data.Encoding.ISO88591
import Templates.Templates (KontrakcjaTemplates)
import Data.Maybe
-- | Request with configuration and something else wrapper 
newtype PC a = PC (PayExConfig,a) deriving Show

class PayExRequest a where 
  actionName::a-> String
  actionGroup::a -> String

actionURL::(PayExRequest a) => PC a -> String
actionURL (PC (config,a)) = (serverAddress config) ++ "/" ++ (actionGroup a) ++ "/" ++ (actionGroup a) ++ ".asmx"

soapActionName::(PayExRequest a) => PC a -> String
soapActionName (PC (config,a)) = (actionPrefix config) ++ "/" ++ (actionGroup a) ++ "/" ++ (actionName a)

rqHeader::(PayExRequest a) => PC a -> [Content ()] -> [Content ()]
rqHeader (PC (config,a)) content =  [CElem (Elem (actionName a) [mkAttr "xmlns"  ((actionPrefix config)++"/"++(actionGroup a)++"/")] content) ()]
-- | REQUESTS
-- | PayExInit starts a request

data  PayExInit  = PayExInit Request KontrakcjaTemplates (Maybe String) Payment 

instance PayExRequest PayExInit where 
  actionName _ = "Initialize7" 
  actionGroup _ = "PxOrder"
  
instance HTypeable (PC PayExInit) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PC PayExInit) where
    parseContents = error "Never serialize response"
    toContents rq@(PC (config, PayExInit request templates agreement payment)) =
         rqHeader rq $ 
                   fieldBox $  (setField "accountNumber" $ accountNumber config) >>>
                               (setField "purchaseOperation" "SALE") >>>
                               (setField "price" $ show $ money $ paymentValue payment ) >>>
                               (setField "priceArgList" $ "") >>>
                               (setField "currency" $ "") >>>
                               (setField "vat" $ "0") >>>
                               (setField "orderID" $ show $ unPaymentId $ paymentId payment) >>>
                               (setField "productNumber" "0") >>>
                               (setField "description" "All information du delger SkrivaPå eller dess Tjänster hanteras med största") >>>
                               (setField "clientIPAddress" $ fst $ rqPeer $ request) >>>
                               (setField "clientIdentifier" $ "") >>>
                               (setField "additionalValues" $ "") >>>
                               (setField "externalID" $ "") >>>
                               (setField "returnUrl" $ returnUrl config ++ (show $ LinkPayExView $ Just $ paymentId payment)) >>>
                               (setField "view" $ "CC") >>>
                               (setField "agreementRef" $ fromMaybe "" agreement) >>>
                               (setField "cancelUrl" $ "") >>>
                               (setField "clientLanguage" $ "") >>>
                               (addHash $ encryptionKey config)
                             
-- | Creating reocurring payment agreement
data  PayExAgreement  = PayExAgreement {
                                          agreementDescription::String,
                                          agreementMax::Money -- |  Max amount that can be changed, something arround double of max signing cost
                                    }

instance PayExRequest PayExAgreement where 
  actionName _ = "CreateAgreement3" 
  actionGroup _ = "PxAgreement"
  
instance HTypeable (PC PayExAgreement) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PC PayExAgreement) where
    parseContents = error "Never serialize response"
    toContents rq@(PC (config, payexagreement)) =
         rqHeader rq $ 
                   fieldBox $  (setField "accountNumber" $ accountNumber config) >>>
                               (setField "merchantRef" "SALE") >>>
                               (setField "description" $ agreementDescription payexagreement) >>>
                               (setField "purchaseOperation" $ "SALE") >>>
                               (setField "maxAmount" $ show $ money $ agreementMax payexagreement ) >>>
                               (setField "notifyUrl" $ "") >>>
                               (setField "startDate" $ "") >>>
                               (setField "stopDate" $ "") >>>
                               (addHash $ encryptionKey config)
                             
-- | Paing using autopay
data  PayExAutopay  = PayExAutopay String Payment

instance PayExRequest PayExAutopay where 
  actionName _ = "AutoPay2" 
  actionGroup _ = "PxAgreement"
  
instance HTypeable (PC PayExAutopay) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PC PayExAutopay) where
    parseContents = error "Never serialize response"
    toContents rq@(PC (config, PayExAutopay agreement payment)) =
         rqHeader rq $ 
                   fieldBox $  (setField "accountNumber" $ accountNumber config) >>>
                               (setField "agreementRef" $ agreement) >>>
                               (setField "price" $ show $ money $ paymentValue payment ) >>>
                               (setField "productNumber" "0") >>>
                               (setField "description" "All information du delger SkrivaPå eller dess Tjänster hanteras med största") >>>
                               (setField "orderId" $ show $ unPaymentId $ paymentId payment) >>>
                               (setField "purchaseOperation" "SALE") >>>
                               (addHash $ encryptionKey config)
                             
                             
-- | PayExComplete (our implementation) locks money for transaction, but not charges anyone
data  PayExComplete = PayExComplete Payment

instance PayExRequest PayExComplete where 
  actionName _ = "Complete"
  actionGroup _ = "PxOrder"
  
instance HTypeable (PC PayExComplete) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PC PayExComplete) where
    parseContents = error "Never serialize response"
    toContents rq@(PC (config, PayExComplete payment)) =
           rqHeader rq $  fieldBox $ 
                               (setField "accountNumber" $ accountNumber config) >>>
                               (setField "orderRef" $ orderRef payment) >>>
                               (addHash $ encryptionKey config)

                            
-- | PayExCapture (our implementation) gives us money we earlier locked
data PayExCapture = PayExCapture Payment

instance PayExRequest PayExCapture where 
  actionName _ = "Capture4"
  actionGroup _ = "PxOrder"
  
instance HTypeable (PC PayExCapture) where
    toHType _ = Defined "PayExCapture" [] []
    
instance XmlContent (PC PayExCapture) where
    parseContents = error "Never serialize rq"
    toContents rq@(PC (config, PayExCapture payment)) =
                 rqHeader rq $ 
                   fieldBox $  (setField "accountNumber" $ accountNumber config) >>>
                               (setField "transactionNumber" $ transactionNumber payment) >>>
                               (setField "amount" $ show $ money $ paymentValue payment) >>>
                               (setField "orderId" $ show $ paymentId payment) >>>
                               (setField "vatAmount" $ "0") >>>
                               (setField "additionalValues" $ "") >>>
                               (addHash $ encryptionKey config)
                 
  
-- | PayExCancel (our implementation)  unlocks money. If we would do capture we could not cancel
data PayExCancel = PayExCancel Payment

instance PayExRequest PayExCancel where 
  actionName _ = "Cancel2"
  actionGroup _ = "PxOrder"
  
instance HTypeable (PC PayExCancel) where
    toHType _ = Defined "PayExCapture" [] []
    
instance XmlContent (PC PayExCancel) where
    parseContents = error "Never serialize rq"
    toContents rq@(PC (config, PayExCancel payment)) =
                  rqHeader rq $ 
                   fieldBox $  (setField "accountNumber" $ accountNumber config) >>>
                               (setField "transactionNumber" $  transactionNumber payment) >>>
                               (addHash $ encryptionKey config)
                  


-- | Wrapping from content
toPC::a -> IO (PC a)
toPC a = do
          pc <-getPayExConfig
          return $ PC (pc,a)



-- |  For each request we need to generate hash, and this constructs lets us do that in quite simple way
fieldBox :: (Monoid a) => (a -> t) -> t
fieldBox f = f mempty

setField::String->String->([Content ()],String) -> ([Content ()],String)
setField n v (l,vs)= (l ++ [mkElemC n (toText $ escapeAmps v)], vs ++ v)

addHash::String -> ([Content ()],String) -> [Content ()] 
addHash hash (l,vs)= l ++ [ mkElemC "hash" (toText $ md5s $ Str $ encodeString ISO88591 $ vs ++ hash) ]

escapeAmps ('&':ss) = "&amp;"++(escapeAmps ss)
escapeAmps (s:ss) = s:(escapeAmps ss)
escapeAmps [] = []
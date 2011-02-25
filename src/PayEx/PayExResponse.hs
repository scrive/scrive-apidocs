{-# OPTIONS_GHC -Wall -XArrows -XViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  PayEx.PayExResponse
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  stable
-- Portability :  portable
--
--  Response that we get from payex with parsers. Since PayEx returs soap response as string param in soap
--  there is some ugly parsing. (hxt with arrows, HaXML is unhelpfull).
-----------------------------------------------------------------------------
module PayEx.PayExResponse where
import Text.XML.HaXml.XmlContent.Parser 
import SOAP.SOAP
import Data.List
import Control.Category hiding ((.))
import Text.XML.HXT.Arrow.XmlArrow hiding (mkAttr)
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.DOM.TypeDefs
import Control.Arrow
import Control.Arrow.ArrowTree
import Control.Arrow.ListArrow
import PayEx.PayExState
--This is wrapper for response, it supports parsing PayEx error info 
newtype PX a = PX (Either PayExError a) deriving Show

--Responses with parsers
data InitResponse  = InitResponse { irOrderRef::String,      
                                    irRedirectUrl::String
                                  }
                                    deriving Show
instance HTypeable (PX InitResponse) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PX InitResponse) where
    toContents _ = error "Do not serialize PayExInitResponse"
    parseContents = payExRequest "Initialize7" $ 
         proc xml -> 
               do
                orderRef    <- fieldText "orderRef"  -< xml
                redirectUrl <- fieldText "redirectUrl" -< xml
                returnA -< InitResponse { irOrderRef= orderRef,
                                          irRedirectUrl=redirectUrl
                                        }


data CompleteResponse  = CompleteResponse {
                                    crOrderStatus::String,
                                    crTransactionStatus::String,
                                    crTransactionNumber::String
                                  }
                                    deriving Show
instance HTypeable (PX CompleteResponse) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PX CompleteResponse) where
    toContents _ = error "Do not serialize PayExInitResponse"
    parseContents = payExRequest "Complete" $ 
         proc xml -> 
               do
                orderStatus <- fieldText "orderStatus" -< xml 
                transactionStatus <- fieldText' "transactionStatus" -< xml 
                tNumber <- fieldText' "transactionNumber" -< xml 
                returnA -< CompleteResponse { crOrderStatus=orderStatus, crTransactionStatus=transactionStatus, crTransactionNumber = tNumber    }



data AutopayResponse  = AutopayResponse {
                                    arTransactionStatus::String,
                                    arTransactionNumber::String
                                  }
                                    deriving Show
instance HTypeable (PX AutopayResponse) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PX AutopayResponse) where
    toContents _ = error "Do not serialize PayExInitResponse"
    parseContents = payExRequest "AutoPay2" $ 
         proc xml -> 
               do
                transactionStatus <- fieldText "transactionStatus" -< xml 
                tNumber <- fieldText' "transactionNumber" -< xml 
                returnA -< AutopayResponse { arTransactionStatus=transactionStatus, arTransactionNumber = tNumber }



data CaptureResponse  = CaptureResponse {
                                        captureStatus::String
                                  }
                                    deriving Show
instance HTypeable (PX CaptureResponse) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PX CaptureResponse) where
    toContents _ = error "Do not serialize PayExInitResponse"
    parseContents = payExRequest "Capture4" $ 
         proc xml -> 
               do
                transactionStatus <- fieldText' "transactionStatus" -< xml 
                returnA -< CaptureResponse { captureStatus=transactionStatus  }


data CancelResponse  = CancelResponse {
                                        cancelStatus::String
                                  }
                                    deriving Show
instance HTypeable (PX CancelResponse) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PX CancelResponse) where
    toContents _ = error "Do not serialize PayExInitResponse"
    parseContents = payExRequest "Cancel2" $ 
         proc xml -> 
               do
                status <- fieldText' "transactionStatus" -< xml 
                returnA -< CancelResponse { cancelStatus=status }

data AgreementResponse  = AgreementResponse {
                                        agreementRef::String
                                  }
                                    deriving Show
instance HTypeable (PX  AgreementResponse) where
    toHType _ = Defined "PayExCall" [] []
    
instance XmlContent (PX  AgreementResponse) where
    toContents _ = error "Do not serialize  AgreementResponse"
    parseContents = payExRequest "CreateAgreement3" $ 
         proc xml -> 
               do
                ref <- fieldText "agreementRef" -< xml 
                returnA -< AgreementResponse {  agreementRef=ref}

-- | Error type - for PayEx error info
data PayExError = PayExError {code::String,
                              errorCode::String,
                              description::String,
                              paramName::String,
                              thirdPartyError::String
                           } deriving Show
                           
--Main utils 
class CanUpdateResponse a where
   updatePayment::a -> Payment -> Payment


instance CanUpdateResponse (PX CompleteResponse) where
  updatePayment (PX (Left err)) payment =  payment { paymentState = Failed ((errorCode err) ++ ":" ++ (description err)) (paymentState payment)  }
  updatePayment (PX (Right ires)) payment =  case (crTransactionStatus ires) of
                                              "0" ->  payment { paymentState =  Finished, transactionNumber = crTransactionNumber ires  }
                                              _ ->  payment

instance CanUpdateResponse (PX AutopayResponse) where
  updatePayment (PX (Left err)) payment =  payment { paymentState = Failed ((errorCode err) ++ ":" ++ (description err)) (paymentState payment)  ,  triedAutopay =True}
  updatePayment (PX (Right ires)) payment =  case (arTransactionStatus ires) of
                                              "0" ->  payment { paymentState =  Finished, transactionNumber = arTransactionNumber ires ,  triedAutopay =True}
                                              _ ->    payment {  triedAutopay =True }
                                                      
instance CanUpdateResponse (PX InitResponse) where
  updatePayment (PX (Left err)) payment =  payment {
                                             paymentState = Failed ((errorCode err) ++ ":" ++ (description err)) (paymentState payment)
                                                }
  updatePayment (PX (Right ires)) payment =  payment {
                                                       orderRef = irOrderRef ires ,
                                                       redirectUrl = irRedirectUrl ires,
                                                       paymentState = Send
                                                      }
-- | Helper to unpack response soap insides and running real data parser
payExRequest::String -> LA XmlTree b ->  XMLParser (PX b)  
payExRequest rqName parser = 
    do inElementNS (rqName ++ "Response") $ 
        inElementNS (rqName ++ "Result") $ 
           do
            xml <- text
            let ppr = (parser >>> (arr Right)) <+> (payExError >>> (arr Left))
            let rs = (runLA  (xread >>>  ppr)) $ clearXML $ unEscapeXML xml
            case rs of
             (r:_ ) -> return $ PX r
             _ ->  return $ PX $ Left $ PayExError "Parser died" "" xml "" ""
 
-- | Error parser 
payExError::(ArrowXml a) => a XmlTree (PayExError)                    
payExError =  proc xml -> 
               do
                code <- fieldText "code" -< xml
                errorCode <- fieldText "errorCode"  -< xml
                description <- fieldText "description"  -< xml
                paramName <- fieldText' "paramName" -< xml
                thirdPartyError <- fieldText' "thirdPartyError" -< xml
                returnA -< PayExError {code=code,
                                     errorCode=errorCode,
                                     description=description,
                                     paramName=paramName,
                                     thirdPartyError=thirdPartyError
                                                }

joinPayExError ::Either String (PX a) -> PX a
joinPayExError (Right r) = r
joinPayExError (Left  s) = PX $ Left $ PayExError {code=s,   errorCode="",  description=s, paramName="", thirdPartyError="" }

--Parsing utils                                                
fieldText::(ArrowXml a) => String -> a XmlTree String                                   
fieldText  name =  deep (hasName name >>> (deep getText)) 

fieldText'::(ArrowXml a) => String -> a XmlTree String
fieldText' name =  deep (hasName name >>> (deep getText)) <+> (arr $ const "") 


--I can't find good lib function for escaping xml, and I need it really bad for payex
--I also need to remove xml header from xml
clearXML :: String -> String
clearXML s = clearXML' s
              where clearXML' (stripPrefix "?>" -> Just s') =  s'
                    clearXML' (_:ss) = clearXML' ss
                    clearXML' _ = s

unEscapeXML :: String -> String
unEscapeXML (stripPrefix "&quot;" -> Just s) = '\\': (unEscapeXML s)
unEscapeXML (stripPrefix "&lt;"   -> Just s) = '<' : (unEscapeXML s)
unEscapeXML (stripPrefix "&gt;"   -> Just s) = '>' : (unEscapeXML s)
unEscapeXML (stripPrefix "&amp;"  -> Just s) = '&' : (unEscapeXML s)
unEscapeXML (s:ss) = s:(unEscapeXML ss)
unEscapeXML [] = []


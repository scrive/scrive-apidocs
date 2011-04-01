-----------------------------------------------------------------------------
-- |
-- Module      :  TrustWeaver
-- Maintainer  :  
-- Stability   :  development
-- Portability :  portable
--
-- TrustWeaver interface 
--
-----------------------------------------------------------------------------

module TrustWeaver 
    ( TrustWeaverConf(..)
    , signDocument
    , signDocumentEx -- for testing purposes
    , validateDocument
    , registerAndEnableSection
    , storeInvoice
    , getInvoice
    , registerSection
    , enableSection
    )
    where
import Codec.Binary.Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length, drop, break)
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length, drop)
import qualified Data.ByteString.Lazy as BSL
import Misc
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Process
import Control.Monad
import Text.XML.HaXml.XmlContent.Parser 
import Text.XML.HaXml.XmlContent
import SOAP.SOAP
import Control.Concurrent
import qualified AppLogger as Log
import Data.List

data TrustWeaverConf = TrustWeaverConf
    { signConf    :: Maybe (String,String,String)
    , adminConf   :: Maybe (String,String,String)
    , storageConf :: Maybe (String,String,String)
    , retries     :: Int
    , timeout     :: Int
    } deriving (Eq,Ord,Show,Read)

data SignRequest = SignRequest BS.ByteString String String
            deriving (Eq,Ord,Show,Read)

instance HTypeable (SignRequest) where
    toHType x = Defined "SignRequest" [] []
instance XmlContent (SignRequest) where
    toContents (SignRequest pdfdata senderTag receiverTag) =
        let base64data = encode (BS.unpack pdfdata) in
        [CElem (Elem "SignRequest" [mkAttr "xmlns" "http://www.trustweaver.com/tsswitch"] 
                         [ mkElemC "InputType" (toText "PDF")
                         , mkElemC "JobType" (toText "CADESA")
                         , mkElemC "OutputType" (toText "PDF")
                         , mkElemC "SenderTag" (toText senderTag)
                         , mkElemC "ReceiverTag" (toText receiverTag)
                         , mkElemC "Document" (toText base64data)
                         , mkElemC "AgreementIdentifier" (toText "Prelaunch")
                         ]) ()]
    parseContents = error "Please do not parse SignRequest"

data SignResult = SignResult BS.ByteString
            deriving (Eq,Ord,Show,Read)

instance HTypeable (SignResult) where
    toHType x = Defined "SignResult" [] []
instance XmlContent (SignResult) where
    toContents (SignResult pdfdata) = error "Please do not serialize SignResult"
    parseContents =  do
        { e <- elementNS "SignResult"
        ; base64 <- interior e $ do
            { result <- elementNS "Result"
            ; signedDocument <- elementNS "SignedDocument"
            ; details <- optional $ elementNS "Details"
            ; interior signedDocument $ text
            }
        ; case decode base64 of
            Nothing -> fail "Cannot parse base64 encoded PDF signed document"
            Just value -> return (SignResult (BS.pack value))
        } `adjustErr` ("in <SignResult>, "++)


data ValidateRequest = ValidateRequest BS.ByteString
            deriving (Eq,Ord,Show,Read)

instance HTypeable (ValidateRequest) where
    toHType x = Defined "ValidateRequest" [] []
instance XmlContent (ValidateRequest) where
    toContents (ValidateRequest pdfdata) =
        let base64data = encode (BS.unpack pdfdata) in
        [CElem (Elem "ValidateRequest" [mkAttr "xmlns" "http://www.trustweaver.com/tsswitch"] 
                         [ mkElemC "InputType" (toText "PDF")
                         , mkElemC "JobType" (toText "CADESA")
                         , mkElemC "OutputType" (toText "PDF")
                         , mkElemC "SenderTag" (toText "SE")
                         , mkElemC "ReceiverTag" (toText "SE")
                         , mkElemC "SignedDocument" (toText base64data)
                         , mkElemC "ExcludeOriginalDocument" (toText "false")
                         , mkElemC "AgreementIdentifier" (toText "Prelaunch")
                         ]) ()]
    parseContents = error "Please do not parse ValidateRequest"

data ValidateResult = ValidateResult String String
            deriving (Eq,Ord,Show,Read)

instance HTypeable (ValidateResult) where
    toHType x = Defined "ValidateResult" [] []
instance XmlContent (ValidateResult) where
    toContents _ = error "Please do not serialize ValidateResult"
    parseContents =  do
        { e <- elementNS "ValidateResult"
        ; interior e $ do
            { result <- optional $ inElementNS "Result" $ do
                          code <- inElementNS "Code" text
                          desc <- inElementNS "Desc" text
                          return (code ++ ": " ++ desc)
            ; document <- optional $ elementNS "Document"
            -- ; validationResult <- optional $ inElementNS "ValidationResult" text
            ; archive <- optional $ elementNS "Archive"
            ; details <- optional $ elementNS "Details"
            ; return (ValidateResult (maybe "" id result) "")
            }
        } `adjustErr` ("in <ValidateResult>, "++)

data RegisterSectionRequest = RegisterSectionRequest String

instance HTypeable (RegisterSectionRequest) where
    toHType x = Defined "RegisterSectionRequest" [] []
instance XmlContent (RegisterSectionRequest) where
    toContents (RegisterSectionRequest name) =
        [CElem (Elem "RegisterSection" [mkAttr "xmlns" "http://www.trustweaver.com/trustarchive/admin/v1"] 
                         [mkElemC "Request" 
                                      [ mkElemC "Name" (toText name)
                                      , mkElemC "StorageSectionInfo" 
                                                    [ mkElemC "CountryOfEstablishment" (toText "SE")
                                                    -- , mkElemC "FriendlyName" (toText "???")
                                                    ]
                                      , mkElemC "RegistrationMode" (toText "CreateNew")
                                      ]]) ()]
    parseContents = error "Do not parse RegisterSectionRequest"


data RegisterSectionResponse = RegisterSectionResponse

instance HTypeable (RegisterSectionResponse) where
    toHType x = Defined "RegisterSectionResponse" [] []
instance XmlContent (RegisterSectionResponse) where
    toContents (RegisterSectionResponse) = error "Please do not serialize RegisterSectionResponse"
    parseContents =  do
      --  this element just has to be there, contains nothing
        { e <- elementNS "RegisterSectionResponse"
        ; return RegisterSectionResponse
        } `adjustErr` ("in <RegisterSectionRequest>, "++)


data EnableSectionRequest = EnableSectionRequest String

instance HTypeable (EnableSectionRequest) where
    toHType x = Defined "RegisterSectionRequest" [] []
instance XmlContent (EnableSectionRequest) where
    toContents (EnableSectionRequest name) =
        [CElem (Elem "EnableSection" [mkAttr "xmlns" "http://www.trustweaver.com/trustarchive/admin/v1"] 
                         [mkElemC "Request" 
                                      [ mkElemC "Name" (toText name)
                                      ]]) ()]
    parseContents = error "Do not parse RegisterSectionRequest"

data EnableSectionResponse = EnableSectionResponse String String String

instance HTypeable (EnableSectionResponse) where
    toHType x = Defined "EnableSectionResponse" [] []
instance XmlContent (EnableSectionResponse) where
    toContents _ = error "Do not serialize EnableSectionResponse"
    parseContents =  do
        { e <- elementNS "EnableSectionResponse"
        ; interior e $ do
            { r  <- elementNS "Result"
            ; interior r $ do
                { superAdminUsername <- inElementNS "SuperAdminUsername" text
                ; superAdminPwd <- inElementNS "SuperAdminPwd" text
                ; sectionPath <- inElementNS "SectionPath" text
                ; return (EnableSectionResponse superAdminUsername superAdminPwd sectionPath)
                }
            }
        } `adjustErr` ("in <EnableSectionResponse>, "++)

data StoreInvoiceRequest = StoreInvoiceRequest String String String BS.ByteString 

instance HTypeable (StoreInvoiceRequest) where
    toHType x = Defined "StoreInvoiceRequest" [] []
instance XmlContent (StoreInvoiceRequest) where
    toContents (StoreInvoiceRequest documentid documentdate ownertwname pdfdata) =
        let base64data = encode (BS.unpack pdfdata) in
        [CElem (Elem "StoreInvoice" [mkAttr "xmlns" "http://www.trustweaver.com/trustarchive/storage/v1"] 
                         [mkElemC "Request" 
                                      [ mkElemC "Document" 
                                                    [ mkElemC "Data" (toText base64data)
                                                    , mkElemC "DocumentFormat" (toText "PDF")
                                                    , mkElemC "ImplicitSignatureInfo"
                                                              [ mkElemC "SignatureFormat" (toText "PDF")
                                                              , mkElemC "AuditCategory" (toText "CADESA")
                                                              ]
                                                    ]
                                      , mkElemC "InvoiceInfo" 
                                                    [ mkElemC "InvoiceNo" (toText documentid)
                                                    , mkElemC "InvoiceDate" (toText documentdate)
                                                    -- , mkElemC "PurchaseOrderNo" (toText 1)
                                                    ]
                                      , mkElemC "SupplierInfo"
                                                [ mkElemC "Name" (toText ownertwname)
                                                , mkElemC "StoreFor" (toText "true")
                                                , mkElemC "CountryCode" (toText "SE")
                                                ]
                                      ]]) ()]
    parseContents = error "Do not parse StoreInvoiceRequest"

data StoreInvoiceResponse = StoreInvoiceResponse String

instance HTypeable (StoreInvoiceResponse) where
    toHType x = Defined "StoreInvoiceResponse" [] []
instance XmlContent (StoreInvoiceResponse) where
    toContents (StoreInvoiceResponse name) = error "Do not serialize StoreInvoiceResponse"
    parseContents =  do
        { e <- elementNS "StoreInvoiceResponse"
        ; interior e $ do
            { r  <- elementNS "Result"
            ; interior r $ do
                { supplierReference <- inElementNS "SupplierReference" text
                ; return (StoreInvoiceResponse supplierReference)
                }
            }
        } `adjustErr` ("in <StoreInvoiceResponse>, "++)



data GetInvoiceRequest = GetInvoiceRequest String

instance HTypeable (GetInvoiceRequest) where
    toHType x = Defined "GetInvoiceRequest" [] []
instance XmlContent (GetInvoiceRequest) where
    toContents (GetInvoiceRequest supplierReference) =
        [CElem (Elem "GetInvoice" [mkAttr "xmlns" "http://www.trustweaver.com/trustarchive/storage/v1"] 
                         [mkElemC "Request" 
                                      [ mkElemC "Reference" (toText supplierReference)
                                      ]
                         ]) ()]
    parseContents = error "Do not parse GetInvoiceRequest"

data GetInvoiceResponse = GetInvoiceResponse BS.ByteString

instance HTypeable (GetInvoiceResponse) where
    toHType x = Defined "GetInvoiceResponse" [] []
instance XmlContent (GetInvoiceResponse) where
    toContents (GetInvoiceResponse pdfdata) = error "Do not serialize GetInvoiceResponse"
    parseContents =  do
        { e <- elementNS "GetInvoiceResponse"
        ; interior e $ do
            { r  <- elementNS "Result"
            ; interior r $ do
                { document <- elementNS "Document"
                ; invoiceInfo <- elementNS "InvoiceInfo"
                ; supplierInfo <- elementNS "SupplierInfo"
                ; buyerInfo <- optional $ elementNS "BuyerInfo"
                ; attachments <- optional $ elementNS "Attachments"
                -- to get that document data we would need to parse multiparts
                -- and the make use of xop and something
                -- ugly, lets skip this!
                ; return (GetInvoiceResponse (BS.empty))
                {-
                ; interior document $ do
                    { dta <- elementNS "Data"
                    ; base64encoded <- interior dta $ text
                    ; case decode base64encoded of
                        Nothing -> fail "cannot decode base64 data"
                        Just bin -> return (GetInvoiceResponse (BS.pack bin))
                    }
             -}
                }
            }
        } `adjustErr` ("in <GetInvoiceResponse>, "++)

retry TrustWeaverConf{timeout,retries} action = worker retries
    where
      worker n = do
        result <- action
        case result of
          Left errmsg -> do
                      Log.trustWeaver $ errmsg
                      if n==0 || ":Client:" `isPrefixOf` (dropWhile (/=':') errmsg)
                       then return result
                       else do
                        Log.trustWeaver $ "Retrying TrustWeaver action"
                        threadDelay timeout
                        worker (n-1)
          _ -> return result

signDocument' :: TrustWeaverConf
              -> BS.ByteString
              -> String
              -> String
              -> IO (Either String BS.ByteString)
signDocument' TrustWeaverConf{signConf = Just (url, cert, certpwd)} pdfdata senderTag receiverTag = do
  result <- makeSoapCall url 
            "http://www.trustweaver.com/tsswitch#Sign"
            cert certpwd
           (SignRequest pdfdata senderTag receiverTag)
  let extract (SignResult pdfdata') = pdfdata'
  return (fmap extract result)

signDocument :: TrustWeaverConf
             -> BS.ByteString
             -> IO (Either String BS.ByteString)
signDocument twconf pdfdata = retry twconf $ signDocument' twconf pdfdata "SE" "SE"

signDocumentEx :: TrustWeaverConf
               -> BS.ByteString
               -> String
               -> String
               -> IO (Either String BS.ByteString)
signDocumentEx twconf pdfdata senderTag receiverTag = retry twconf $ signDocument' twconf pdfdata senderTag receiverTag 

validateDocument' :: TrustWeaverConf
                  -> BS.ByteString
                  -> IO (Either String (String,String))
validateDocument' TrustWeaverConf{signConf = Just (url, cert, certpwd)} pdfdata = do

  result <- makeSoapCall url
            "http://www.trustweaver.com/tsswitch#Validate"
            cert certpwd
           (ValidateRequest pdfdata)
  let extract (ValidateResult result validateResult) = (result,validateResult)
  return (fmap extract result)

validateDocument :: TrustWeaverConf
                 -> BS.ByteString
                 -> IO (Either String (String,String))
validateDocument twconf = retry twconf . validateDocument' twconf

registerAndEnableSection' :: TrustWeaverConf
                          -> String
                          -> IO (Either String (String,String,String))
registerAndEnableSection' TrustWeaverConf{adminConf = Just (url, cert, certpwd)} name = do
  result  <- makeSoapCall url
            "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/RegisterSection"
            cert certpwd
            (RegisterSectionRequest name)

  case result of
    Left errmsg -> return $ Left errmsg
    Right RegisterSectionResponse -> do
                result2 <- makeSoapCall url
                           "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/EnableSection"
                           cert certpwd
                           (EnableSectionRequest name)

                let extract (EnableSectionResponse superAdminUsername superAdminPwd sectionPath) =
                        (superAdminUsername, superAdminPwd, sectionPath)
                return (fmap extract result2)

registerAndEnableSection :: TrustWeaverConf
                          -> String
                          -> IO (Either String (String,String,String))
registerAndEnableSection twconf = retry twconf . registerAndEnableSection' twconf



registerSection' :: TrustWeaverConf
                 -> String
                 -> IO (Either String ())
registerSection' TrustWeaverConf{adminConf = Just (url, cert, certpwd)} name = do
  result  <- makeSoapCall url
            "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/RegisterSection"
            cert certpwd
            (RegisterSectionRequest name)

  let extract (RegisterSectionResponse) = ()
  return (fmap extract result)

registerSection :: TrustWeaverConf
                -> String
                -> IO (Either String ())
registerSection twconf = retry twconf . registerSection' twconf


enableSection' :: TrustWeaverConf
               -> String
               -> IO (Either String (String,String,String))
enableSection' TrustWeaverConf{adminConf = Just (url, cert, certpwd)} name = do
  result <- makeSoapCall url
            "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/EnableSection"
            cert certpwd
            (EnableSectionRequest name)

  let extract (EnableSectionResponse superAdminUsername superAdminPwd sectionPath) =
          (superAdminUsername, superAdminPwd, sectionPath)
  return (fmap extract result)

enableSection :: TrustWeaverConf
              -> String
              -> IO (Either String (String,String,String))
enableSection twconf = retry twconf . enableSection' twconf



storeInvoice' :: TrustWeaverConf 
             -> String 
             -> String 
             -> String 
             -> BS.ByteString 
             -> IO (Either String String)
storeInvoice' TrustWeaverConf{storageConf = Just (url, cert, certpwd)} documentid documentdate ownertwname pdfdata = do
  result <- makeSoapCall url
            "http://www.trustweaver.com/trustarchive/storage/v1/StorageServicePort/StoreInvoice"
            cert certpwd
           (StoreInvoiceRequest documentid documentdate ownertwname pdfdata)
  let extract (StoreInvoiceResponse referece) = referece
  return (fmap extract result)

storeInvoice :: TrustWeaverConf 
             -> String 
             -> String 
             -> String 
             -> BS.ByteString 
             -> IO (Either String String)
storeInvoice twconf documentid documentdate ownertwname pdfdata = retry twconf $ storeInvoice' twconf documentid documentdate ownertwname pdfdata

getInvoice' :: TrustWeaverConf 
           -> String 
           -> IO (Either String BS.ByteString)
getInvoice' TrustWeaverConf{storageConf = Just (url, cert, certpwd)} reference = do
  result <- makeSoapCall url
            "http://www.trustweaver.com/trustarchive/storage/v1/StorageServicePort/GetInvoice"
            cert certpwd
           (GetInvoiceRequest reference)
  let extract (GetInvoiceResponse pdfdata) = pdfdata
  return (fmap extract result)

getInvoice :: TrustWeaverConf 
           -> String 
           -> IO (Either String BS.ByteString)
getInvoice twconf = retry twconf . getInvoice' twconf

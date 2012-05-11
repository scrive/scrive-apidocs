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
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import Text.XML.HaXml.XmlContent.Parser

import Control.Concurrent
import Data.List
import SOAP.SOAP

import qualified Log
import qualified Data.ByteString as BS

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
    toHType _ = Defined "SignRequest" [] []
instance XmlContent (SignRequest) where
    toContents (SignRequest pdfdata senderTag receiverTag) =
        let base64data = BSC.unpack (Base64.encode pdfdata) in
        [CElem (Elem "SignRequest" [mkAttr "xmlns" "http://www.trustweaver.com/tsswitch"]
                         [ mkElemC "InputType" (toText "PDF")
                         , mkElemC "JobType" (toText "CADESA")
                         , mkElemC "OutputType" (toText "PDF")
                         , mkElemC "SenderTag" (toText senderTag)
                         , mkElemC "ReceiverTag" (toText receiverTag)
                         , mkElemC "Document" (toText base64data)
                         ]) ()]
    parseContents = error "Please do not parse SignRequest"

data SignResult = SignResult BS.ByteString
            deriving (Eq,Ord,Show,Read)

instance HTypeable (SignResult) where
    toHType _ = Defined "SignResult" [] []
instance XmlContent (SignResult) where
    toContents (SignResult _pdfdata) = error "Please do not serialize SignResult"
    parseContents =  do
        { e <- elementNS "SignResult"
        ; base64 <- interior e $ do
            { _result <- elementNS "Result"
            ; signedDocument <- elementNS "SignedDocument"
            ; _details <- optional $ elementNS "Details"
            ; interior signedDocument $ text
            }
        ; case Base64.decode (BSC.pack base64) of
            Left errmsg -> fail errmsg
            Right value -> return (SignResult value)
        } `adjustErr` ("in <SignResult>, "++)


data ValidateRequest = ValidateRequest BS.ByteString
            deriving (Eq,Ord,Show,Read)

instance HTypeable (ValidateRequest) where
    toHType _ = Defined "ValidateRequest" [] []
instance XmlContent (ValidateRequest) where
    toContents (ValidateRequest pdfdata) =
        let base64data = BSC.unpack (Base64.encode pdfdata) in
        [CElem (Elem "ValidateRequest" [mkAttr "xmlns" "http://www.trustweaver.com/tsswitch"]
                         [ mkElemC "InputType" (toText "PDF")
                         , mkElemC "JobType" (toText "CADESA")
                         , mkElemC "OutputType" (toText "PDF")
                         , mkElemC "SenderTag" (toText "SE")
                         , mkElemC "ReceiverTag" (toText "SE")
                         , mkElemC "SignedDocument" (toText base64data)
                         , mkElemC "ExcludeOriginalDocument" (toText "false")
                         ]) ()]
    parseContents = error "Please do not parse ValidateRequest"

data ValidateResult = ValidateResult String String
            deriving (Eq,Ord,Show,Read)

instance HTypeable (ValidateResult) where
    toHType _ = Defined "ValidateResult" [] []
instance XmlContent (ValidateResult) where
    toContents _ = error "Please do not serialize ValidateResult"
    parseContents =  do
        { e <- elementNS "ValidateResult"
        ; interior e $ do
            { result <- optional $ inElementNS "Result" $ do
                          code <- inElementNS "Code" text
                          desc <- inElementNS "Desc" text
                          return (code ++ ": " ++ desc)
            ; _document <- optional $ elementNS "Document"
            -- ; validationResult <- optional $ inElementNS "ValidationResult" text
            ; _archive <- optional $ elementNS "Archive"
            ; _details <- optional $ elementNS "Details"
            ; return (ValidateResult (maybe "" id result) "")
            }
        } `adjustErr` ("in <ValidateResult>, "++)

data RegisterSectionRequest = RegisterSectionRequest String

instance HTypeable (RegisterSectionRequest) where
    toHType _ = Defined "RegisterSectionRequest" [] []
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
    toHType _ = Defined "RegisterSectionResponse" [] []
instance XmlContent (RegisterSectionResponse) where
    toContents (RegisterSectionResponse) = error "Please do not serialize RegisterSectionResponse"
    parseContents =  do
      --  this element just has to be there, contains nothing
        { _e <- elementNS "RegisterSectionResponse"
        ; return RegisterSectionResponse
        } `adjustErr` ("in <RegisterSectionRequest>, "++)


data EnableSectionRequest = EnableSectionRequest String

instance HTypeable (EnableSectionRequest) where
    toHType _ = Defined "RegisterSectionRequest" [] []
instance XmlContent (EnableSectionRequest) where
    toContents (EnableSectionRequest name) =
        [CElem (Elem "EnableSection" [mkAttr "xmlns" "http://www.trustweaver.com/trustarchive/admin/v1"]
                         [mkElemC "Request"
                                      [ mkElemC "Name" (toText name)
                                      ]]) ()]
    parseContents = error "Do not parse RegisterSectionRequest"

data EnableSectionResponse = EnableSectionResponse String String String

instance HTypeable (EnableSectionResponse) where
    toHType _ = Defined "EnableSectionResponse" [] []
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
    toHType _ = Defined "StoreInvoiceRequest" [] []
instance XmlContent (StoreInvoiceRequest) where
    toContents (StoreInvoiceRequest documentid documentdate ownertwname pdfdata) =
        let base64data = BSC.unpack (Base64.encode pdfdata) in
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
    toHType _ = Defined "StoreInvoiceResponse" [] []
instance XmlContent (StoreInvoiceResponse) where
    toContents (StoreInvoiceResponse _name) = error "Do not serialize StoreInvoiceResponse"
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
    toHType _ = Defined "GetInvoiceRequest" [] []
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
    toHType _ = Defined "GetInvoiceResponse" [] []
instance XmlContent (GetInvoiceResponse) where
    toContents (GetInvoiceResponse _pdfdata) = error "Do not serialize GetInvoiceResponse"
    parseContents =  do
        { e <- elementNS "GetInvoiceResponse"
        ; interior e $ do
            { r  <- elementNS "Result"
            ; interior r $ do
                { _document <- elementNS "Document"
                ; _invoiceInfo <- elementNS "InvoiceInfo"
                ; _supplierInfo <- elementNS "SupplierInfo"
                ; _buyerInfo <- optional $ elementNS "BuyerInfo"
                ; _attachments <- optional $ elementNS "Attachments"
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

retry :: TrustWeaverConf -> IO (Either String t) -> IO (Either String t)
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
  result <- makeSoapCallWithCert url
            "http://www.trustweaver.com/tsswitch#Sign"
            cert certpwd
           (SignRequest pdfdata senderTag receiverTag)
  let extract (SignResult pdfdata') = pdfdata'
  return (fmap extract result)
signDocument' _ _ _ _ = return $ Left "Not possible: signDocument' without signConf"

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
  result <- makeSoapCallWithCert url
            "http://www.trustweaver.com/tsswitch#Validate"
            cert certpwd
            (ValidateRequest pdfdata)
  let extract (ValidateResult vresult validateResult) = (vresult, validateResult)
  return (fmap extract result)
validateDocument' _ _ = return $ Left "Not possible: validateDocument' without signConf"

validateDocument :: TrustWeaverConf
                 -> BS.ByteString
                 -> IO (Either String (String,String))
validateDocument twconf = retry twconf . validateDocument' twconf

registerAndEnableSection' :: TrustWeaverConf
                          -> String
                          -> IO (Either String (String,String,String))
registerAndEnableSection' TrustWeaverConf{adminConf = Just (url, cert, certpwd)} name = do
  result  <- makeSoapCallWithCert url
            "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/RegisterSection"
            cert certpwd
            (RegisterSectionRequest name)
  case result of
    Left errmsg -> return $ Left errmsg
    Right RegisterSectionResponse -> do
                result2 <- makeSoapCallWithCert url
                           "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/EnableSection"
                           cert certpwd
                           (EnableSectionRequest name)

                let extract (EnableSectionResponse superAdminUsername superAdminPwd sectionPath) =
                        (superAdminUsername, superAdminPwd, sectionPath)
                return (fmap extract result2)
registerAndEnableSection' _ _ = return $ Left "Not possible: registerAndEnableSection' without adminConf"

registerAndEnableSection :: TrustWeaverConf
                          -> String
                          -> IO (Either String (String,String,String))
registerAndEnableSection twconf = retry twconf . registerAndEnableSection' twconf

registerSection' :: TrustWeaverConf
                 -> String
                 -> IO (Either String ())
registerSection' TrustWeaverConf{adminConf = Just (url, cert, certpwd)} name = do
  result  <- makeSoapCallWithCert url
            "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/RegisterSection"
            cert certpwd
            (RegisterSectionRequest name)
  let extract (RegisterSectionResponse) = ()
  return (fmap extract result)
registerSection' _ _ = return $ Left "Not possible: registerSection' without adminConf"

registerSection :: TrustWeaverConf
                -> String
                -> IO (Either String ())
registerSection twconf = retry twconf . registerSection' twconf

enableSection' :: TrustWeaverConf
               -> String
               -> IO (Either String (String,String,String))
enableSection' TrustWeaverConf{adminConf = Just (url, cert, certpwd)} name = do
  result <- makeSoapCallWithCert url
            "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/EnableSection"
            cert certpwd
            (EnableSectionRequest name)
  let extract (EnableSectionResponse superAdminUsername superAdminPwd sectionPath) =
          (superAdminUsername, superAdminPwd, sectionPath)
  return (fmap extract result)
enableSection' _ _ = return $ Left "Not possible: enableSection' without adminConf"

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
  result <- makeSoapCallWithCert url
            "http://www.trustweaver.com/trustarchive/storage/v1/StorageServicePort/StoreInvoice"
            cert certpwd
           (StoreInvoiceRequest documentid documentdate ownertwname pdfdata)
  let extract (StoreInvoiceResponse referece) = referece
  return (fmap extract result)
storeInvoice' _ _ _ _ _ = return $ Left "Not possible: storeInvoice' without storageConf"

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
  result <- makeSoapCallWithCert url
            "http://www.trustweaver.com/trustarchive/storage/v1/StorageServicePort/GetInvoice"
            cert certpwd
           (GetInvoiceRequest reference)
  let extract (GetInvoiceResponse pdfdata) = pdfdata
  return (fmap extract result)
getInvoice' _ _ = return $ Left "Not possible: getInvoice' without storageConf"

getInvoice :: TrustWeaverConf
           -> String
           -> IO (Either String BS.ByteString)
getInvoice twconf = retry twconf . getInvoice' twconf

{-# LANGUAGE ScopedTypeVariables #-}
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
    -- (signDocument)
    where
import Codec.Binary.Base64
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length, drop)
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length, drop)
import qualified Data.ByteString.Lazy as BSL
import Misc
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Process
import Control.Monad
import User (Context(..))
import Text.XML.HaXml.XmlContent.Parser 
import Text.XML.HaXml.XmlContent

data SOAP a = SOAP a
            deriving (Eq,Ord,Show,Read)

instance HTypeable (SOAP a) where
    toHType x = Defined "soap" [] []
instance (XmlContent a) => XmlContent (SOAP a) where
    toContents (SOAP a) =
        [CElem (Elem "Envelope" [mkAttr "xmlns" "http://schemas.xmlsoap.org/soap/envelope/"] 
                         [CElem (Elem "Body" [] 
                         (toContents a)) ()]) ()]
    parseContents = do
        { e <- elementNS "Envelope"
        ; interior e $ do
            { optional $ elementNS "Header"
            ; b <- elementNS "Body"
            ; interior b $ return SOAP `apply` parseContents
            }
        } `adjustErr` ("in <Envelope/Body>, "++)


data SignRequest = SignRequest BS.ByteString
            deriving (Eq,Ord,Show,Read)

instance HTypeable (SignRequest) where
    toHType x = Defined "SignRequest" [] []
instance XmlContent (SignRequest) where
    toContents (SignRequest pdfdata) =
        let base64data = encode (BS.unpack pdfdata) in
        [CElem (Elem "SignRequest" [mkAttr "xmlns" "http://www.trustweaver.com/tsswitch"] 
                         [ mkElemC "InputType" (toText "PDF")
                         , mkElemC "JobType" (toText "CADESA")
                         , mkElemC "OutputType" (toText "PDF")
                         , mkElemC "SenderTag" (toText "SE")
                         , mkElemC "ReceiverTag" (toText "SE")
                         , mkElemC "Document" (toText base64data)
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
            ; details <- elementNS "Details"
            ; interior signedDocument $ text
            }
        ; case decode base64 of
            Nothing -> fail "Cannot parse base64 encoded PDF signed document"
            Just value -> return (SignResult (BS.pack value))
        } `adjustErr` ("in <SignResult>, "++)

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
                { superAdminUsername <- inElementWith skipNamespacePrefix "SuperAdminUsername" text
                ; superAdminPwd <- inElementWith skipNamespacePrefix "SuperAdminPwd" text
                ; sectionPath <- inElementWith skipNamespacePrefix "SectionPath" text
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
        [CElem (Elem "StoreInvoice" [mkAttr "xmlns" "http://www.trustweaver.com/trustarchive/admin/v1"] 
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
                { supplierReference <- inElementWith skipNamespacePrefix "SupplierReference" text
                ; return (StoreInvoiceResponse supplierReference)
                }
            }
        } `adjustErr` ("in <StoreInvoiceResponse>, "++)



data GetInvoiceRequest = GetInvoiceRequest String

instance HTypeable (GetInvoiceRequest) where
    toHType x = Defined "GetInvoiceRequest" [] []
instance XmlContent (GetInvoiceRequest) where
    toContents (GetInvoiceRequest supplierReference) =
        [CElem (Elem "GetInvoice" [mkAttr "xmlns" "http://www.trustweaver.com/trustarchive/admin/v1"] 
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
                ; interior document $ do
                    { dta <- elementNS "Data"
                    ; base64encoded <- interior dta $ text
                    ; case decode base64encoded of
                        Nothing -> fail "cannot decode base64 data"
                        Just bin -> return (GetInvoiceResponse (BS.pack bin))
                    }
                }
            }
        } `adjustErr` ("in <GetInvoiceResponse>, "++)


skipNamespacePrefix fqname tomatch = 
    case break (==':') fqname of 
      (name,"") -> name == tomatch
      (_,':':name) -> name == tomatch

elementNS name = elementWith skipNamespacePrefix [name]
 

signDocument :: Context -> BS.ByteString -> IO (Maybe BS.ByteString)
signDocument ctx pdfdata = do
  let Context{ctxtwsigncert,ctxtwsigncertpwd} = ctx

  result <- makeSoapCall "https://tseiod-dev.trustweaver.com/ts/svs.asmx"
            "http://www.trustweaver.com/tsswitch#Sign"
            ctxtwsigncert ctxtwsigncertpwd
           (SignRequest pdfdata)
  case result of
    Just (SignResult pdfdata') -> return (Just pdfdata')
    Nothing -> return Nothing

registerAndEnableSection :: Context -> String -> IO (String,String,String)
registerAndEnableSection ctx name = do
  let Context{ctxtwadmincert,ctxtwadmincertpwd} = ctx

  Just RegisterSectionResponse <- makeSoapCall "https://twa-test-db.trustweaver.com/ta_hubservices/Admin/AdminService.svc"
            "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/RegisterSection"
            ctxtwadmincert ctxtwadmincertpwd
           (RegisterSectionRequest name)

  result2 <- makeSoapCall "https://twa-test-db.trustweaver.com/ta_hubservices/Admin/AdminService.svc"
            "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/EnableSection"
            ctxtwadmincert ctxtwadmincertpwd
           (EnableSectionRequest name)

  case result2 of
    Just (EnableSectionResponse superAdminUsername superAdminPwd sectionPath) -> 
        return (superAdminUsername, superAdminPwd, sectionPath)
    Nothing -> error "Section not enabled, sorry"
  

makeSoapCall :: (XmlContent a, XmlContent b) => String -> String -> String -> String -> a -> IO (Maybe b)
makeSoapCall url action cert certpwd request = do
  let input = fpsShowXml False (SOAP request)
  let args = [ "-X", "POST", 
               "-k", "--silent", "--show-error",
               "--cert", cert ++ ":" ++ certpwd,
               "--cacert", cert,
               "--data-binary", "@-",
               "-H", "Content-Type: text/xml; charset=UTF-8",
               "-H", "Expect: 100-continue",
               "-H", "SOAPAction: " ++ action,
               url
             ]

  (code,stdout,stderr) <- readProcessWithExitCode' "curl.exe" args input

  when (code /= ExitSuccess) $ do
       putStrLn "Cannot execute ./curl for TrustWeaver"
       BSL.hPutStr System.IO.stdout stderr

  case readXml (BSL.toString stdout) of
    Right (SOAP result) -> return (Just result)
    Left errmsg -> do
      print $ "makeSoapCall error: " ++ errmsg
      print args
      print input
      print stdout
      return Nothing

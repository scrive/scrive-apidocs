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

data TrustWeaverConf = TrustWeaverConf
    { signcert          :: FilePath
    , signcertpwd       :: String
    , admincert         :: FilePath
    , admincertpwd      :: String
    }
                       deriving (Eq,Ord,Show,Read)


data SOAP a = SOAP a
            | SOAPFault String String String
            deriving (Eq,Ord,Show,Read)

instance HTypeable (SOAP a) where
    toHType x = Defined "soap" [] []
instance (XmlContent a) => XmlContent (SOAP a) where
    toContents (SOAP a) =
        [CElem (Elem "Envelope" [mkAttr "xmlns" "http://schemas.xmlsoap.org/soap/envelope/"] 
                         [CElem (Elem "Body" [] 
                         (toContents a)) ()]) ()]
    toContents _ = error "Please do not serialize SOAPFault"
    parseContents = do
        { inElementNS "Envelope" $ do
            { optional $ elementNS "Header"
            ; inElementNS "Body" $ choice SOAP 
                                             (inElementNS "Fault" $ do
                                                { faultcode <- inElementNS "faultcode" text
                                                ; faultstring <- inElementNS "faultstring" text
                                                -- ; faultactor <- optional $ inElementNS "faultactor" text
                                                ; detail <- optional $ inElementNS "detail" (optional text)
                                                -- ; return (SOAPFault faultcode faultstring (maybe "" id faultactor))
                                                ; return (SOAPFault faultcode faultstring "")
                                                }
                                              )
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


skipNamespacePrefix fqname tomatch = 
    case break (==':') fqname of 
      (name,"") -> name == tomatch
      (_,':':name) -> name == tomatch

elementNS name = elementWith skipNamespacePrefix [name]
 
inElementNS name action = do
  e <- elementNS name
  interior e action

signDocument :: TrustWeaverConf -> BS.ByteString -> IO (Either String BS.ByteString)
signDocument TrustWeaverConf{signcert,signcertpwd} pdfdata = do

  result <- makeSoapCall "https://tseiod-dev.trustweaver.com/ts/svs.asmx"
            "http://www.trustweaver.com/tsswitch#Sign"
            signcert signcertpwd
           (SignRequest pdfdata)
  let extract (SignResult pdfdata') = pdfdata'
  return (fmap extract result)


validateDocument :: TrustWeaverConf -> BS.ByteString -> IO (Either String (String,String))
validateDocument TrustWeaverConf{signcert,signcertpwd} pdfdata = do

  result <- makeSoapCall "https://tseiod-dev.trustweaver.com/ts/svs.asmx"
            "http://www.trustweaver.com/tsswitch#Validate"
            signcert signcertpwd
           (ValidateRequest pdfdata)
  let extract (ValidateResult result validateResult) = (result,validateResult)
  return (fmap extract result)


registerAndEnableSection :: TrustWeaverConf -> String -> IO (Either String (String,String,String))
registerAndEnableSection TrustWeaverConf{admincert,admincertpwd} name = do

  Right RegisterSectionResponse <- makeSoapCall "https://twa-test-db.trustweaver.com/ta_hubservices/Admin/AdminService.svc"
            "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/RegisterSection"
            admincert admincertpwd
           (RegisterSectionRequest name)

  result2 <- makeSoapCall "https://twa-test-db.trustweaver.com/ta_hubservices/Admin/AdminService.svc"
            "http://www.trustweaver.com/trustarchive/admin/v1/AdminServicePort/EnableSection"
            admincert admincertpwd
           (EnableSectionRequest name)

  let extract (EnableSectionResponse superAdminUsername superAdminPwd sectionPath) =
        (superAdminUsername, superAdminPwd, sectionPath)
  return (fmap extract result2)

storeInvoice :: TrustWeaverConf 
             -> String 
             -> String 
             -> String 
             -> BS.ByteString 
             -> IO (Either String String)
storeInvoice TrustWeaverConf{admincert,admincertpwd} documentid documentdate ownertwname pdfdata = do
  result <- makeSoapCall "https://twa-test-db.trustweaver.com/ta_hubservices/Storage/StorageService.svc"
            "http://www.trustweaver.com/trustarchive/storage/v1/StorageServicePort/StoreInvoice"
            admincert admincertpwd
           (StoreInvoiceRequest documentid documentdate ownertwname pdfdata)
  let extract (StoreInvoiceResponse referece) = referece
  return (fmap extract result)
  
getInvoice :: TrustWeaverConf 
           -> String 
           -> IO (Either String BS.ByteString)
getInvoice TrustWeaverConf{admincert,admincertpwd} reference = do
  result <- makeSoapCall "https://twa-test-db.trustweaver.com/ta_hubservices/Storage/StorageService.svc"
            "http://www.trustweaver.com/trustarchive/storage/v1/StorageServicePort/GetInvoice"
            admincert admincertpwd
           (GetInvoiceRequest reference)
  let extract (GetInvoiceResponse pdfdata) = pdfdata
  return (fmap extract result)
  


makeSoapCall :: (XmlContent request, XmlContent result) 
                => String
             -> String
             -> String
             -> String
             -> request
             -> IO (Either String result)
makeSoapCall url action cert certpwd request = do
  let input = fpsShowXml False (SOAP request)
  -- BSL.appendFile "soap.xml" input

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
  -- BSL.appendFile "soap.xml" stdout

  {-
   stdout here might be a multipart stream
   lets do it the painful way

   --uuid:1616144b-0f50-40a5-9587-92b5b989e330+id=48
   Content-ID: <http://tempuri.org/0>
   Content-Transfer-Encoding: 8bit
   Content-Type: application/xop+xml;charset=utf-8;type="text/xml"

   <s:Envelope ...>,,,</s:Envelope>
   --uuid:1616144b-0f50-40a5-9587-92b5b989e330+id=48--

   so:
   1. See if it starts with --
   2. Cut off to first \n and remember
   3. Cut-off to first \n\n
   4. Cut up to the remembered part
   5. Use middle as XML to parse
   -}
  let (boundary,rest) = BS.breakSubstring (BS.fromString "\r\n") (BS.concat (BSL.toChunks (BSL.drop 2 stdout)))
      (header,rest2) = BS.breakSubstring (BS.fromString "\r\n\r\n") rest
      (xml1,_) = BS.breakSubstring boundary rest2
  
  xml <- if BSL.fromString "\r\n--" `BSL.isPrefixOf` stdout
            then do
              return (BSL.fromChunks [xml1])
            else return stdout

  if (code /= ExitSuccess)
       then return (Left $ "Cannot execute ./curl for TrustWeaver: " ++ BSL.toString stderr)
     else case readXml (BSL.toString xml) of
            Right (SOAP result) -> return (Right result)
            Right (SOAPFault code string actor) -> return (Left (code ++":" ++ string ++":" ++ actor))
            Left errmsg -> return (Left (errmsg ++ ": " ++ BSL.toString stdout))

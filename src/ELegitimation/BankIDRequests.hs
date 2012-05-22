module ELegitimation.BankIDRequests (
          ImplStatus(..)
        , LogicaConfig(..)
        , generateChallenge
        , encodeTBS
        , verifySignature
        , mbiRequestSignature
        , mbiRequestCollect
        , CollectResponse(..)
        ) where

import Data.Maybe
import Misc hiding (optional)
import SOAP.SOAP
import Text.XML.HaXml.Posn (Posn)
import Text.XML.HaXml.XmlContent.Parser
import ELegitimation.SignatureProvider
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Base64 as Base64
import Happstack.Data (Typeable, Version, deriveSerialize)

data LogicaConfig = LogicaConfig { logicaEndpoint  :: String,  -- ^ URL to Logica
                                   logicaServiceID :: String,  -- ^ ServiceID from Logica
                                   logicaCertFile  :: String,   -- ^ Path to certificate file
                                   logicaMBIDisplayName:: String,  -- ^ Display Name for Mobile Bank ID (must match display name registered with Logica)
                                   logicaMBIEndpoint :: String -- ^ URL for MobileBankID at Logica
                                 }
                  deriving (Show, Read, Ord, Eq)

data ImplStatus = ImplStatus { errorGroup            :: Int, 
                               errorGroupDescription :: String, 
                               errorCode             :: Int, 
                               errorCodeDescription  :: String
                            } 

data GenerateChallengeRequest = GenerateChallengeRequest SignatureProvider String -- serviceid

instance HTypeable (GenerateChallengeRequest) where
    toHType _x = Defined "GenerateChallengeRequest" [] []
instance XmlContent (GenerateChallengeRequest) where
    toContents (GenerateChallengeRequest provider policy) =
        [CElem (Elem "generateChallengeRequest"
                [mkAttr "xmlns" "urn:www.sll.se/wsdl/soap/osif"]
                [CElem (Elem "provider"
                                 [mkAttr "xmlns" ""]
                                 (toText $ show $ fromSafeEnumInt provider)) ()
                ,CElem (Elem "policy"
                                 [mkAttr "xmlns" ""]
                                 (toText policy)) ()
                ])
         ()]
    parseContents = error "Please do not parse GenerateChallengeRequest"

data GenerateChallengeResponse = GenerateChallengeResponse ImplStatus String String

instance HTypeable (GenerateChallengeResponse) where
    toHType _x = Defined "GenerateChallengeResponse" [] []
instance XmlContent (GenerateChallengeResponse) where
    toContents _ = error "Do not serialize GenerateChallengeResponse"
    parseContents =  do
        { e <- elementNS "generateChallengeResponse"
        ; interior e $ do
            { s <- elementNS "status"
            ; status <- interior s $ do
                          { errorGroup <- inElementNS "errorGroup" text
                          ; errorGroupDescription <- optional $ inElementNS "errorGroupDescription" text
                          ; errorCode <- inElementNS "errorCode" text
                          ; errorCodeDescription <- optional $ inElementNS "errorCodeDescription" text
                          ; return (ImplStatus (read errorGroup)
                                               (fromMaybe "" errorGroupDescription)
                                               (read errorCode)
                                               (fromMaybe "" errorCodeDescription))
                          }

            ; challenge <- optional $ inElementNS "challenge" text
            ; transactionid <- optional $ inElementNS "transactionID" text
            ; return (GenerateChallengeResponse status
                                                (fromMaybe "" challenge)
                                                (fromMaybe "" transactionid))
            }
        } `adjustErr` ("in <GenerateChallengeResponse>, "++)

data EncodeTBSRequest = EncodeTBSRequest SignatureProvider String String String

instance HTypeable (EncodeTBSRequest) where
    toHType _x = Defined "EncodeTBSRequest" [] []
instance XmlContent (EncodeTBSRequest) where
    toContents (EncodeTBSRequest provider policy tbs transactionID) =
        [CElem (Elem "encodeTBSRequest"
                [mkAttr "xmlns" "urn:www.sll.se/wsdl/soap/osif"]
                [CElem (Elem "provider"
                                 [mkAttr "xmlns" ""]
                                 (toText $ show $ fromSafeEnumInt provider)) ()
                ,CElem (Elem "policy"
                                 [mkAttr "xmlns" ""]
                                 (toText policy)) ()
                ,CElem (Elem "transactionID"
                                 [mkAttr "xmlns" ""]
                                 (toText transactionID)) ()
                ,CElem (Elem "tbsText"
                                 [mkAttr "xmlns" ""]
                                 (toText tbs)) ()
                ])
         ()]
    parseContents = error "Please do not parse EncodeTBSRequest"

data EncodeTBSResponse = EncodeTBSResponse ImplStatus String String

instance HTypeable (EncodeTBSResponse) where
    toHType _x = Defined "EncodeTBSResponse" [] []
instance XmlContent (EncodeTBSResponse) where
    toContents _ = error "Do not serialize EncodeTBSResponse"
    parseContents =  do
        { e <- elementNS "encodeTBSResponse"
        ; interior e $ do
            { s <- elementNS "status"
            ; status <- interior s $ do
                          { errorGroup <- inElementNS "errorGroup" text
                          ; errorGroupDescription <- optional $ inElementNS "errorGroupDescription" text
                          ; errorCode <- inElementNS "errorCode" text
                          ; errorCodeDescription <- optional $ inElementNS "errorCodeDescription" text
                          ; return (ImplStatus (read errorGroup)
                                               (fromMaybe "" errorGroupDescription)
                                               (read errorCode)
                                               (fromMaybe "" errorCodeDescription))
                          }

            ; text2 <- optional $ inElementNS "text" text
            ; transactionid <- optional $ inElementNS "transactionID" text
            ; return (EncodeTBSResponse status
                                                (fromMaybe "" text2)
                                                (fromMaybe "" transactionid))
            }
        } `adjustErr` ("in <EncodeTBSResponse>, "++)

data VerifySignatureRequest = VerifySignatureRequest SignatureProvider String String String (Maybe String) String

instance HTypeable (VerifySignatureRequest) where
    toHType _x = Defined "VerifySignatureRequest" [] []
instance XmlContent (VerifySignatureRequest) where
    toContents (VerifySignatureRequest provider policy tbs signature mnonce transactionID) =
        [CElem (Elem "verifySignatureRequest"
                [mkAttr "xmlns" "urn:www.sll.se/wsdl/soap/osif"]
                ([CElem (Elem "provider"
                                 [mkAttr "xmlns" ""]
                                 (toText $ show $ fromSafeEnumInt provider)) ()
                ,CElem (Elem "policy"
                                 [mkAttr "xmlns" ""]
                                 (toText policy)) ()
                ,CElem (Elem "transactionID"
                                 [mkAttr "xmlns" ""]
                                 (toText transactionID)) ()
                ,CElem (Elem "tbsText"
                                 [mkAttr "xmlns" ""]
                                 (toText tbs)) ()
                ,CElem (Elem "signature"
                                 [mkAttr "xmlns" ""]
                                 (toText signature)) ()
                                 ]
                                 ++ [CElem (Elem "nonce"
                                                [mkAttr "xmlns" ""]
                                                (toText $ fromJust mnonce)) () | isJust mnonce]
                ))
         ()]
    parseContents = error "Please do not parse VerifySignatureRequest"

slurpAttributes :: Parser (Content Text.XML.HaXml.Posn.Posn) [(String, String)]
slurpAttributes = worker []
  where
    worker acc = do
      att <- optional $ inElementNS "attributes" $ do
        name <- inElementNS "name" text
        value <- inElementNS "value" text
        return (name, value)
      case att of
        Nothing -> return acc
        Just nv -> worker $ nv : acc

data VerifySignatureResponse = VerifySignatureResponse ImplStatus [(String, String)] String

instance HTypeable (VerifySignatureResponse) where
    toHType _x = Defined "VerifySignatureResponse" [] []
instance XmlContent (VerifySignatureResponse) where
    toContents _ = error "Do not serialize VerifySignatureResponse"
    parseContents =  do
        { e <- elementNS "verifySignatureResponse"
        ; interior e $ do
            { s <- elementNS "status"
            ; status <- interior s $ do
                          { errorGroup <- inElementNS "errorGroup" text
                          ; errorGroupDescription <- optional $ inElementNS "errorGroupDescription" text
                          ; errorCode <- inElementNS "errorCode" text
                          ; errorCodeDescription <- optional $ inElementNS "errorCodeDescription" text
                          ; return (ImplStatus (read errorGroup)
                                               (fromMaybe "" errorGroupDescription)
                                               (read errorCode)
                                               (fromMaybe "" errorCodeDescription))
                          }
            ; attrs <- slurpAttributes
            ; _transactionid <- optional $ inElementNS "transactionID" text
            ; certificate <- optional $ inElementNS "certificate" text
            ; return (VerifySignatureResponse status attrs (fromMaybe "" certificate))
            }
        } `adjustErr` ("in <VerifySignatureResponse>, "++)

generateChallenge :: LogicaConfig -> SignatureProvider -> IO (Either ImplStatus (String, String)) -- challenge + transactionid
generateChallenge (LogicaConfig{..}) provider = do
    eresponse <- makeSoapCallWithCA logicaEndpoint logicaCertFile "GenerateChallenge" $ GenerateChallengeRequest provider logicaServiceID
    case eresponse of
        Left msg -> return $ Left $ ImplStatus (-1) "SOAP Call Failure" (-1) msg
        Right (GenerateChallengeResponse (ImplStatus _ _ 0 _) challenge transactionid) -> return $ Right (challenge, transactionid)
        Right (GenerateChallengeResponse status _ _) -> return $ Left status

encodeTBS :: LogicaConfig -> SignatureProvider -> String -> String -> IO (Either ImplStatus String) -- encoded TBS + transactionid
encodeTBS (LogicaConfig{..}) provider tbs transactionID = do
    eresponse <- makeSoapCallWithCA logicaEndpoint logicaCertFile "EncodeTBS" $ EncodeTBSRequest provider logicaServiceID tbs transactionID
    case eresponse of
        Left msg -> return $ Left $ ImplStatus (-1) "SOAP Call Failure" (-1) msg
        Right (EncodeTBSResponse (ImplStatus _ _ 0 _) txt _) -> return $ Right txt
        Right (EncodeTBSResponse status _ _) -> return $ Left status

verifySignature :: LogicaConfig
                -> SignatureProvider
                -> String
                -> String
                -> Maybe String
                -> String
                -> IO (Either ImplStatus (String, [(String, String)])) -- certificate and user attributes
verifySignature LogicaConfig{..} provider tbs signature mnonce transactionID = do
    eresponse <- 
        makeSoapCallWithCA logicaEndpoint logicaCertFile
            "VerifySignature" $
            VerifySignatureRequest provider
                logicaServiceID
                tbs
                signature
                mnonce
                transactionID
    case eresponse of
        Left msg -> return $ Left $ ImplStatus (-1) "SOAP Call Failure" (-1) msg
        Right (VerifySignatureResponse (ImplStatus _ _ 0 _) attrs certificate) -> return $ Right (certificate, attrs)
        Right (VerifySignatureResponse status _ _) -> return $ Left status

-- Mobile BankID Stuff

data SignatureRequest = SignatureRequest { srPolicy            :: String
                                         , srDisplayName       :: String
                                         , srPersonalNumber    :: String
                                         , srUserVisibleData   :: String
                                         }
                        
instance HTypeable (SignatureRequest) where
    toHType _x = Defined "SignatureRequest" [] []
instance XmlContent (SignatureRequest) where
    toContents (SignatureRequest{..}) =
        [CElem (Elem "SignRequest"
                [mkAttr "xmlns" "http://logica.com/mbi/service/v1.0.0/"]
                [CElem (Elem "policy" [mkAttr "xmlns" ""] $ toText srPolicy) ()
                ,CElem (Elem "displayName" [mkAttr "xmlns" ""] $ toText srDisplayName) ()
                ,CElem (Elem "personalNumber" [mkAttr "xmlns" ""] $ toText srPersonalNumber) ()
                ,CElem (Elem "userVisibleData" [mkAttr "xmlns" ""] $ toText srUserVisibleData) ()]) ()]
    parseContents = error "Please do not parse SignatureRequest"

data SignatureResponse = SignatureResponse String String
                         
instance HTypeable (SignatureResponse) where
    toHType _x = Defined "SignatureResponse" [] []
instance XmlContent (SignatureResponse) where
    toContents _ = error "Do not serialize SignatureResponse"
    parseContents =  do
        { e <- elementNS "SignResponse"
        ; interior e $ do
            { transactionid <- inElementNS "transactionId" text
            ; orderRef <- inElementNS "orderRef" text
            ; return (SignatureResponse transactionid orderRef)
            }
        } `adjustErr` ("in <SignatureResponse>, "++)

data CollectRequest = CollectRequest { crPolicy        :: String
                                     , crTransactionID :: String
                                     , crOrderRef      :: String
                                     , crDisplayName   :: String
                                     }

instance HTypeable (CollectRequest) where
  toHType _ = Defined "CollectRequest" [] []
instance XmlContent (CollectRequest) where
  toContents (CollectRequest{..}) =
    [CElem (Elem "CollectRequest"
            [mkAttr "xmlns" "http://logica.com/mbi/service/v1.0.0/"]
            [CElem (Elem "policy" [mkAttr "xmlns" ""] $ toText crPolicy) ()
            ,CElem (Elem "displayName" [mkAttr "xmlns" ""] $ toText crDisplayName) ()
            ,CElem (Elem "transactionId" [mkAttr "xmlns" ""] $ toText crTransactionID) ()
            ,CElem (Elem "orderRef" [mkAttr "xmlns" ""] $ toText crOrderRef) ()]) ()]
  parseContents = error "Please do not parse CollectRequest."
  
data CollectResponse = CROutstanding { cresTransactionID :: String }
                     | CRUserSign { cresTransactionID :: String }
                     | CRComplete { cresTransactionID :: String
                                  , cresSignature :: String
                                  , cresAttributes :: [(String, String)]
                                  }
                     deriving (Show, Eq, Read, Ord, Typeable)
                              
$(deriveSerialize ''CollectResponse)
instance Version (CollectResponse)

instance HTypeable (CollectResponse) where
    toHType _x = Defined "CollectResponse" [] []
instance XmlContent (CollectResponse) where
    toContents _ = error "Do not serialize CollectResponse"
    parseContents =  do
        { e <- elementNS "CollectResponse"
        ; interior e $ do
            { transactionid <- inElementNS "transactionId" text
            ; progressStatus <- inElementNS "progressStatus" text
            ; msignature <- optional $ inElementNS "signature" text
            ; attrs <- slurpAttributes
            ; return $ case (progressStatus, msignature) of
                ("OUTSTANDING_TRANSACTION", _) -> CROutstanding transactionid
                ("USER_SIGN", _) -> CRUserSign transactionid
                ("COMPLETE", Just signature) -> CRComplete transactionid signature attrs
                _ -> error $ "Trying to parse CollectResponse but did not understand it; status: " ++ show progressStatus ++ ", signature: " ++ show msignature
            }
        } `adjustErr` ("in <CollectResponse>, "++)
                     
mbiRequestSignature :: LogicaConfig -> String -> String -> IO (Either String (String, String))
mbiRequestSignature LogicaConfig{..} personalnumber uvd = do
  eresponse <- makeSoapCallWithCA logicaMBIEndpoint logicaCertFile "Sign" $ SignatureRequest logicaServiceID logicaMBIDisplayName personalnumber (BS.toString $ Base64.encode $ BS.fromString uvd)
  case eresponse of
    Left m -> return $ Left m
    Right (SignatureResponse tid oref) -> return $ Right (tid, oref)

mbiRequestCollect :: LogicaConfig -> String -> String -> IO (Either String CollectResponse)
mbiRequestCollect LogicaConfig{..} tid oref = do
  makeSoapCallWithCA logicaMBIEndpoint logicaCertFile "Collect" $ CollectRequest logicaServiceID tid oref logicaMBIDisplayName
  
  
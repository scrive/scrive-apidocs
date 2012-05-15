module ELegitimation.BankIDRequests (
          ImplStatus(..)
        , LogicaConfig(..)
        , generateChallenge
        , encodeTBS
        , verifySignature
        ) where

import Data.Maybe
import Misc hiding (optional)
import SOAP.SOAP
import Text.XML.HaXml.Posn (Posn)
import Text.XML.HaXml.XmlContent.Parser
import ELegitimation.SignatureProvider

data LogicaConfig = LogicaConfig { logicaEndpoint  :: String,  -- ^ URL to Logica
                                   logicaServiceID :: String,  -- ^ ServiceID from Logica
                                   logicaCertFile  :: String   -- ^ Path to certificate file
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

generateChallenge :: LogicaConfig -> SignatureProvider -> IO (Either ImplStatus (String, String))
generateChallenge (LogicaConfig{..}) provider = do
    eresponse <- makeSoapCallWithCA logicaEndpoint logicaCertFile "GenerateChallenge" $ GenerateChallengeRequest provider logicaServiceID
    case eresponse of
        Left msg -> return $ Left $ ImplStatus (-1) "SOAP Call Failure" (-1) msg
        Right (GenerateChallengeResponse (ImplStatus _ _ 0 _) challenge transactionid) -> return $ Right (challenge, transactionid)
        Right (GenerateChallengeResponse status _ _) -> return $ Left status

encodeTBS :: LogicaConfig -> SignatureProvider -> String -> String -> IO (Either ImplStatus String)
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
                -> IO (Either ImplStatus (String, [(String, String)]))
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


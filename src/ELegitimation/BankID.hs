module ELegitimation.BankID where

import User
import UserControl
import System.Process
import System.Exit
import System.IO
import System.Directory
import Control.Concurrent
import Data.List
import Happstack.Server
import Happstack.State
import DocState
import Misc
import Session
import "mtl" Control.Monad.State
import DocControl
import SOAP.SOAP
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS hiding (length, drop, break)
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length, drop)
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe

import Text.XML.HaXml.XmlContent.Parser 
import Text.XML.HaXml.XmlContent

import KontraLink

import ELegitimation.ELeg
import MinutesTime

handleSign :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra Response
handleSign docid signid magic = do
  -- document <- queryOrFail $ GetDocumentByDocumentID docid
  -- checkLinkIDAndMagicHash document signid magic
  ctx <- get

  liftIO $ print $ ctxelegtransactions ctx

  nonceresponse <- generateChallenge

  -- should generate this somehow
  let tbs = "This is what you need to sign."
  case nonceresponse of
    Left (ImplStatus _a _b code msg) -> return $ toResponse $ toJSON [("status", JInt code)
                                                                     ,("msg", JString msg)]
    Right (nonce, transactionid) -> do
        encodetbsresponse <- encodeTBS tbs transactionid 
        case encodetbsresponse of
          Left (ImplStatus _a _b code msg) -> return $ toResponse $ toJSON [("status", JInt code)
                                                                           ,("msg", JString msg)]
          Right text -> do
                       -- store in session
                       addELegTransaction $ ELegTransaction 
                                              { transactionservertime = ctxtime ctx
                                              , transactiontransactionid = transactionid
                                              , transactiontbs = tbs
                                              , transactionencodedtbs = text
                                              , transactionsignatorylinkid = Just signid
                                              , transactiondocumentid = docid
                                              , transactionmagichash = Just magic
                                              , transactionnonce = nonce
                                              }
                       let MinutesTime time = ctxtime ctx
                       return $ toResponse $ toJSON [("status", JInt 0)
                                                    ,("servertime", JString $ show $ 60 * time)
                                                    ,("nonce", JString nonce)
                                                    ,("tbs", JString text)
                                                    ,("transactionid", JString transactionid)]

handleSignPost :: DocumentID -> SignatoryLinkID -> MagicHash -> Kontra KontraLink
handleSignPost docid signid magic = do
  document <- queryOrFail $ GetDocumentByDocumentID docid
  checkLinkIDAndMagicHash document signid magic

  let allowedidtypes = documentallowedidtypes document
      allowsELeg = isJust $ find (== ELegitimationIdentification) allowedidtypes

  when (not allowsELeg) mzero

  signature <- getDataFnM $ look "signature"
  transactionid <- getDataFnM $ look "transactionid"

  ctx@Context { ctxelegtransactions } <- get

  let mtrans = find ((== transactionid) . transactiontransactionid) ctxelegtransactions
  case mtrans of
    Nothing -> mzero
    Just trans -> do
                -- validate transaction with document info
                when (isNothing $ transactionsignatorylinkid trans) mzero
                when (isNothing $ transactionmagichash trans) mzero
                let ELegTransaction { transactionsignatorylinkid = Just tsignid
                                    , transactiondocumentid = tdocid
                                    , transactionmagichash = Just tmagic
                                    } = trans
                when (not (tsignid == signid && tdocid == docid && tmagic == magic)) mzero
                -- end validation

                done <- verifySignature (transactionencodedtbs trans)
                                        signature
                                        (transactionnonce trans)
                                        transactionid

                case done of
                  Left (ImplStatus _a _b code msg) -> do
                                      liftIO $ print $ toJSON [("status", JInt code)
                                                              ,("msg", JString msg)]
                                      -- change me! I should return back to the same page
                                      return $ LinkMain
                  Right (cert, attrs) -> do
                            Context { ctxtime, ctxipnumber } <- get
                            document@DocState.Document{ documentstatus = olddocumentstatus, documentsignatorylinks } <- queryOrFail $ GetDocumentByDocumentID docid
                            fieldnames <- getAndConcat "fieldname"
                            fieldvalues <- getAndConcat "fieldvalue"
                            let fields = zip fieldnames fieldvalues
                                signinfo = SignatureInfo { signatureinfotext = transactiontbs trans
                                                         , signatureinfosignature = signature
                                                         , signatureinfocertificate = cert
                                                         , signatureinfoprovider = BankIDProvider
                                                         }
                                

                            newdocument <- update $ SignDocument docid signid ctxtime ctxipnumber (Just signinfo) fields
                            case newdocument of
                              Left message -> do
                                               addFlashMsgText message
                                               return $ LinkMain
                              Right document -> do 
                                               postDocumentChangeAction document olddocumentstatus (Just signid)
                                               return $ LinkSigned docid signid
handleIssue :: DocumentID -> Kontra Response
handleIssue docid = do
  ctx <- get

  liftIO $ print $ ctxelegtransactions ctx

  nonceresponse <- generateChallenge

  -- should generate this somehow
  let tbs = "This is what you need to sign."
  case nonceresponse of
    Left (ImplStatus _a _b code msg) -> return $ toResponse $ toJSON [("status", JInt code)
                                                                     ,("msg", JString msg)]
    Right (nonce, transactionid) -> do
        encodetbsresponse <- encodeTBS tbs transactionid 
        case encodetbsresponse of
          Left (ImplStatus _a _b code msg) -> return $ toResponse $ toJSON [("status", JInt code)
                                                                           ,("msg", JString msg)]
          Right text -> do
                       -- store in session
                       addELegTransaction $ ELegTransaction 
                                              { transactionservertime = ctxtime ctx
                                              , transactiontransactionid = transactionid
                                              , transactiontbs = tbs
                                              , transactionencodedtbs = text
                                              , transactionsignatorylinkid = Nothing
                                              , transactiondocumentid = docid
                                              , transactionmagichash = Nothing
                                              , transactionnonce = nonce
                                              }
                       let MinutesTime time = ctxtime ctx
                       return $ toResponse $ toJSON [("status", JInt 0)
                                                    ,("servertime", JString $ show $ 60 * time)
                                                    ,("nonce", JString nonce)
                                                    ,("tbs", JString text)
                                                    ,("transactionid", JString transactionid)]


handleIssuePost :: DocumentID -> Kontra KontraLink
handleIssuePost docid = withUserPost $ do
  document <- queryOrFail $ GetDocumentByDocumentID $ docid
  ctx@Context { ctxmaybeuser = Just user, ctxelegtransactions } <- get
  failIfNotAuthor document user

  let allowedidtypes = documentallowedidtypes document
      allowsELeg = isJust $ find (== ELegitimationIdentification) allowedidtypes

  when (not allowsELeg) mzero


  signature <- getDataFnM $ look "signature"
  transactionid <- getDataFnM $ look "transactionid"

  let mtrans = find ((== transactionid) . transactiontransactionid) ctxelegtransactions
  case mtrans of
    Nothing -> mzero
    Just trans -> do
                -- validate transaction
                let ELegTransaction { transactiondocumentid = tdocid } = trans
                when (docid /= tdocid) mzero
                -- end validation

                done <- verifySignature (transactionencodedtbs trans)
                                        signature
                                        (transactionnonce trans)
                                        transactionid

                case done of
                  Left (ImplStatus _a _b code msg) -> do
                                      liftIO $ print $ toJSON [("status", JInt code)
                                                              ,("msg", JString msg)]
                                      -- change me! I should return back to the same page
                                      return $ LinkMain
                  Right (cert, attrs) -> do
                            let siginfo = SignatureInfo { signatureinfotext = transactiontbs trans
                                                        , signatureinfosignature = signature
                                                        , signatureinfocertificate = cert
                                                        , signatureinfoprovider = BankIDProvider
                                                        }
                            doc2 <- updateDocument ctx document $ Just siginfo
                            return $ LinkSignInvite docid


data JSONValue = JString String
               | JInt Int  

kvJson :: (String, JSONValue) -> String
kvJson (k, JInt v) = "\"" ++ k ++ "\" : " ++ show v  
kvJson (k, JString v) = "\"" ++ k ++ "\" : \"" ++ v ++ "\""

toJSON :: [(String, JSONValue)] -> String
toJSON kvs = "{ " ++ (concat $ intersperse ", " (map kvJson kvs)) ++ " }"

endpoint = "https://eidt.funktionstjanster.se:18898/osif"

data ImplStatus = ImplStatus Int String Int String



data GenerateChallengeRequest = GenerateChallengeRequest Int String

instance HTypeable (GenerateChallengeRequest) where
    toHType x = Defined "GenerateChallengeRequest" [] []
instance XmlContent (GenerateChallengeRequest) where
    toContents (GenerateChallengeRequest provider policy) =
        [CElem (Elem "generateChallengeRequest" 
                [mkAttr "xmlns" "urn:www.sll.se/wsdl/soap/osif"]
                [CElem (Elem "provider"
                                 [mkAttr "xmlns" ""]
                                 (toText $ show provider)) ()
                ,CElem (Elem "policy"
                                 [mkAttr "xmlns" ""]
                                 (toText policy)) ()
                ]) 
         ()]
    parseContents = error "Please do not parse GenerateChallengeRequest"

data GenerateChallengeResponse = GenerateChallengeResponse ImplStatus String String

instance HTypeable (GenerateChallengeResponse) where
    toHType x = Defined "GenerateChallengeResponse" [] []
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
                                               (maybe "" id errorGroupDescription)
                                               (read errorCode)
                                               (maybe "" id errorCodeDescription))
                          }

            ; challenge <- optional $ inElementNS "challenge" text
            ; transactionid <- optional $ inElementNS "transactionID" text
            ; return (GenerateChallengeResponse status
                                                (maybe "" id challenge)
                                                (maybe "" id transactionid))
            }
        } `adjustErr` ("in <GenerateChallengeResponse>, "++)

data EncodeTBSRequest = EncodeTBSRequest Int String String String

instance HTypeable (EncodeTBSRequest) where
    toHType x = Defined "EncodeTBSRequest" [] []
instance XmlContent (EncodeTBSRequest) where
    toContents (EncodeTBSRequest provider policy tbs transactionID) =
        [CElem (Elem "encodeTBSRequest" 
                [mkAttr "xmlns" "urn:www.sll.se/wsdl/soap/osif"]
                [CElem (Elem "provider"
                                 [mkAttr "xmlns" ""]
                                 (toText $ show provider)) ()
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
    toHType x = Defined "EncodeTBSResponse" [] []
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
                                               (maybe "" id errorGroupDescription)
                                               (read errorCode)
                                               (maybe "" id errorCodeDescription))
                          }

            ; text2 <- optional $ inElementNS "text" text
            ; transactionid <- optional $ inElementNS "transactionID" text
            ; return (EncodeTBSResponse status
                                                (maybe "" id text2)
                                                (maybe "" id transactionid))
            }
        } `adjustErr` ("in <EncodeTBSResponse>, "++)

data VerifySignatureRequest = VerifySignatureRequest Int String String String String String

instance HTypeable (VerifySignatureRequest) where
    toHType x = Defined "VerifySignatureRequest" [] []
instance XmlContent (VerifySignatureRequest) where
    toContents (VerifySignatureRequest provider policy tbs signature nonce transactionID) =
        [CElem (Elem "verifySignatureRequest" 
                [mkAttr "xmlns" "urn:www.sll.se/wsdl/soap/osif"]
                [CElem (Elem "provider"
                                 [mkAttr "xmlns" ""]
                                 (toText $ show provider)) ()
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
                ,CElem (Elem "nonce"
                                 [mkAttr "xmlns" ""]
                                 (toText nonce)) ()
                ]) 
         ()]
    parseContents = error "Please do not parse VerifySignatureRequest"

slurpAttributes = slurpAttributes' []

slurpAttributes' ls = do
  att <- optional $ inElementNS "attributes" $ do
                        name <- inElementNS "name" text
                        value <- inElementNS "value" text
                        return (name, value)
  case att of
    Nothing -> return ls
    Just nv -> slurpAttributes' $ nv : ls
  

data VerifySignatureResponse = VerifySignatureResponse ImplStatus [(String, String)] String

instance HTypeable (VerifySignatureResponse) where
    toHType x = Defined "VerifySignatureResponse" [] []
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
                                               (maybe "" id errorGroupDescription)
                                               (read errorCode)
                                               (maybe "" id errorCodeDescription))
                          }
            ; attributes <- slurpAttributes
            ; transactionid <- optional $ inElementNS "transactionID" text
            ; certificate <- optional $ inElementNS "certificate" text
            ; return (VerifySignatureResponse status attributes (maybe "" id certificate))
            }
        } `adjustErr` ("in <VerifySignatureResponse>, "++)

{-
generateChallenge :: Kontra (Either String (String, String))
generateChallenge = do
  let action = "GenerateChallenge"
  res <- liftIO $ soapPost endpoint action content
  if "<challenge>" `isInfixOf` res && "<transactionID>" `isInfixOf` res
   then return $ Right $ (parseChallenge res, parseTransactionID res)
   else return $ Left $ "Could not parse challenge response: " ++ res
-}
generateChallenge :: Kontra (Either ImplStatus (String, String))
generateChallenge = do
  eresponse <- liftIO $ makeSoapCallINSECURE endpoint "GenerateChallenge" $ GenerateChallengeRequest 6 "logtest004"
  case eresponse of
    Left msg -> do
      liftIO $ print msg
      error msg
    Right (GenerateChallengeResponse (ImplStatus a b status msg) challenge transactionid) -> do
                      if status == 0
                         then return $ Right (challenge, transactionid)
                         else return $ Left (ImplStatus a b status msg)
                 
encodeTBS :: String -> String -> Kontra (Either ImplStatus String)
encodeTBS tbs transactionID = do
  eresponse <- liftIO $ makeSoapCallINSECURE endpoint "EncodeTBS" $ EncodeTBSRequest 6 "logtest004" tbs transactionID
  case eresponse of
    Left msg -> do
      liftIO $ print msg
      error msg
    Right (EncodeTBSResponse (ImplStatus a b status msg) text c) -> do
                      if status == 0
                         then return $ Right text
                         else return $ Left (ImplStatus a b status msg)

verifySignature :: String -> String -> String -> String -> Kontra (Either ImplStatus (String, [(String, String)]))
verifySignature tbs signature nonce transactionID = do
  eresponse <- liftIO $ makeSoapCallINSECURE endpoint "VerifySignature" $ VerifySignatureRequest 6 "logtest004" tbs signature nonce transactionID
  case eresponse of
    Left msg -> do
      liftIO $ print msg
      error msg
    Right (VerifySignatureResponse (ImplStatus a b status msg) attrs certificate) -> do
                      if status == 0
                         then return $ Right (certificate, attrs)
                         else return $ Left (ImplStatus a b status msg)

parseChallenge = getBetween "<challenge>" "</challenge>"
parseTransactionID = getBetween "<transactionID>" "</transactionID>"

getBetween pre post = getBefore post . getAfter pre

getBefore post s =
    getBefore' post s []
    where getBefore' _ [] acc = reverse acc
          getBefore' post (s:ss) acc
              | isPrefixOf post (s:ss) = reverse acc
              | otherwise              = getBefore' post ss (s:acc)              

getAfter pre s
    | isPrefixOf pre s = drop (length pre) s
    | otherwise        = getAfter pre (tail s)    
                              

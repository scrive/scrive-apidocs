module EID.CGI.GRP.Control (grpRoutes) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Unjson
import Happstack.Server hiding (dir)
import Happstack.StaticRouting
import Log
import Text.StringTemplates.Templates
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.StringTemplates.Fields as F

import Chargeable.Model
import Company.Model
import DB hiding (InternalError)
import Doc.DocStateData
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model
import Doc.SignatoryLinkID
import Doc.Tokens.Model
import EID.Authentication.Model
import EID.CGI.GRP.Config
import EID.CGI.GRP.Data
import EID.CGI.GRP.Transaction.Model
import EID.Signature.Model
import EvidenceLog.Model
import Happstack.Fields
import Kontra hiding (InternalError)
import KontraLink
import KontraPrelude
import Network.SOAP.Call
import Network.SOAP.Transport.Curl
import Routing
import Session.Cookies
import Session.Data
import Session.Model
import Templates
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils

grpRoutes :: Route (Kontra Response)
grpRoutes = dir "cgi" . dir "grp" $ choice [
    dir "auth"    . hPost . toK2 $ handleAuthRequest
  , dir "sign"    . hPost . toK2 $ handleSignRequest
  , dir "collect" . hGet  . toK2 $ handleCollectRequest
  , dir "collectWithRedirect" . hGet  . toK2 $ handleCollectAndRedirectRequest
  ]

----------------------------------------

handleAuthRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleAuthRequest did slid = do
  CgiGrpConfig{..} <- ctxcgigrpconfig <$> getContext
  (doc,_) <- getDocumentAndSignatory did slid
  mcompany_display_name <- getCompanyDisplayName doc
  pn <- getField "personal_number" `onNothing` do
    logInfo_ "No personal number"
    respond404
  guardThatPersonalNumberMatches slid pn doc
  certErrorHandler <- mkCertErrorHandler
  debugFunction <- mkDebugFunction
  let transport = curlTransport SecureSSL (Just cgCertFile) cgGateway id certErrorHandler debugFunction
      req = AuthRequest {
        arqPolicy = cgServiceID
      , arqDisplayName = fromMaybe cgDisplayName mcompany_display_name
      , arqPersonalNumber = T.pack pn
      , arqProvider = "bankid"
      }
      parser = Right <$> xpAuthResponse <|> Left <$> xpGrpFault
  cgiResp <- soapCall transport "" () req parser >>= \case
    Left AlreadyInProgress -> do
      -- Action that is already in progress will get cancelled. In that case we do call again to get new transaction
      soapCall transport "" () req parser
    res -> return res
  case cgiResp of
    Left fault -> return $ unjsonToJSON unjsonDef fault
    Right sr@AuthResponse{..} -> do
        logInfo_ $ "SOAP response:" <+> show sr
        sess <- getCurrentSession
        dbUpdate $ MergeCgiGrpTransaction $ CgiGrpAuthTransaction slid arsTransactionID arsOrderRef (sesID sess)
        return $ unjsonToJSON unjsonDef (arsAutoStartToken, sessionCookieInfoFromSession sess)
----------------------------------------

handleSignRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleSignRequest did slid = do
  CgiGrpConfig{..} <- ctxcgigrpconfig <$> getContext
  (doc,_) <- getDocumentAndSignatory did slid
  mcompany_display_name <- getCompanyDisplayName doc
  tbs <- textToBeSigned doc
  pn <- getField "personal_number" `onNothing` do
    logInfo_ "No personal number"
    respond404
  guardThatPersonalNumberMatches slid pn doc
  certErrorHandler <- mkCertErrorHandler
  debugFunction <- mkDebugFunction
  let transport = curlTransport SecureSSL (Just cgCertFile) cgGateway id certErrorHandler debugFunction
      req = SignRequest {
        srqPolicy = cgServiceID
      , srqDisplayName = fromMaybe cgDisplayName mcompany_display_name
      , srqPersonalNumber = T.pack pn
      , srqUserVisibleData = T.decodeUtf8 . B64.encode . BSU.fromString $ tbs
      , srqProvider = "bankid"
      }
      parser = Right <$> xpSignResponse <|> Left <$> xpGrpFault
  cgiResp <- soapCall transport "" () req parser >>= \case
    Left AlreadyInProgress -> do
      -- Action that is already in progress will get cancelled. In that case we do call again to get new transaction
      soapCall transport "" () req parser
    res -> return res
  case cgiResp of
    Left fault -> return $ unjsonToJSON unjsonDef fault
    Right sr@SignResponse{..} -> do
      logInfo_ $ "SOAP response:" <+> show sr
      sess <- getCurrentSession
      dbUpdate $ MergeCgiGrpTransaction $ CgiGrpSignTransaction slid (T.pack tbs) srsTransactionID srsOrderRef (sesID sess)
      return $ unjsonToJSON unjsonDef (srsAutoStartToken,sessionCookieInfoFromSession sess)

handleCollectRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleCollectRequest did slid = do
  res <- collectRequest  did slid
  case res of
    Left e -> return $ unjsonToJSON unjsonDef e
    Right r -> return $ unjsonToJSON unjsonDef r

handleCollectAndRedirectRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleCollectAndRedirectRequest did slid = do
  (msession :: Maybe SessionCookieInfo) <- readField "session_id"
  case msession of
    Nothing -> internalError -- This should never happend
    Just sci -> unsafeSessionTakeover (cookieSessionID sci) (cookieSessionToken sci)
  _ <- collectRequest did slid -- There is no reason to process results of collect. We will redirect to link from param anyway
  murl <- getField "url"
  case murl of
    Nothing -> internalError -- This should never happend
    Just l -> do
      return $ LinkExternal l

collectRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m (Either GrpFault ProgressStatus)
collectRequest did slid = do
  CgiGrpConfig{..} <- ctxcgigrpconfig <$> getContext
  (doc,sl) <- getDocumentAndSignatory did slid
  mcompany_display_name <- getCompanyDisplayName doc
  ttype <- getField "type" >>= \t -> case t of
                                        Just "auth" -> return CgiGrpAuth
                                        Just "sign" -> return CgiGrpSign
                                        _ -> do
                                          logInfo_ "Collect request without type"
                                          respond404
  mcgiTransaction <- dbQuery (GetCgiGrpTransaction ttype slid)
  case mcgiTransaction of
    Nothing -> do
      sesid <- ctxsessionid <$> getContext
      success <- case ttype of
        CgiGrpAuth -> isJust <$> (dbQuery $ GetEAuthentication sesid slid)
        CgiGrpSign -> isJust <$> (dbQuery $ GetESignature slid)
      if (success)
        then return $ Right Complete
        else return $ Left ExpiredTransaction
    Just cgiTransaction -> do
      certErrorHandler <- mkCertErrorHandler
      debugFunction <- mkDebugFunction
      let transport = curlTransport SecureSSL (Just cgCertFile) cgGateway id certErrorHandler debugFunction
          req = CollectRequest {
            crqPolicy = cgServiceID
          , crqTransactionID = cgiTransactionID cgiTransaction
          , crqOrderRef = cgiOrderRef cgiTransaction
          , crqDisplayName = fromMaybe cgDisplayName mcompany_display_name
          }
          parser = Right <$> xpCollectResponse <|> Left <$> xpGrpFault

      soapCall transport "" () req parser >>= \case
        Left fault -> do
          logInfo_ $ "CGI Fault:" <+> show fault
          dbUpdate $ DeleteCgiGrpTransaction ttype slid
          return $ Left fault
        Right cr@CollectResponse{..} -> do
          logInfo_ $ "SOAP response:" <+> show cr
          when (crsProgressStatus == Complete) $ do
            dbUpdate $ DeleteCgiGrpTransaction ttype slid
            case cgiTransaction of
              (CgiGrpAuthTransaction _ _ _ session_id)   -> do
                -- all the required attributes are supposed to always
                -- be there, so bail out if this is not the case.
                let signatoryName = just_lookup "cert.subject.cn" crsAttributes
                let signatoryPersonalNumber = just_lookup "cert.subject.serialnumber" crsAttributes
                let signature = mk_binary $ fromMaybe (missing "signature") crsSignature
                let ocspResponse = mk_binary $ just_lookup "Validation.ocsp.response" crsAttributes

                dbUpdate $ MergeBankIDAuthentication session_id slid BankIDAuthentication {
                    bidaSignatoryName = signatoryName
                  , bidaSignatoryPersonalNumber = signatoryPersonalNumber
                  , bidaSignature = signature
                  , bidaOcspResponse = ocspResponse
                }
                ctx <- getContext
                let eventFields = do
                      F.value "signatory_name" signatoryName
                      F.value "signatory_personal_number" signatoryPersonalNumber
                      F.value "provider" ("Swedish BankID" :: String)
                      F.value "signature" $ B64.encode . unBinary $ signature
                      F.value "ocsp_response" $ B64.encode . unBinary $ ocspResponse
                withDocument doc $
                  void $ dbUpdate . InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence  (eventFields) (Just sl) Nothing =<< signatoryActor ctx sl
                dbUpdate $ ChargeCompanyForSEBankIDAuthentication did

              (CgiGrpSignTransaction _ tbs _ _ _)   -> do
                -- all the required attributes are supposed to always
                -- be there, so bail out if this is not the case.
                dbUpdate $ MergeBankIDSignature slid BankIDSignature {
                    bidsSignatoryName = just_lookup "cert.subject.cn" crsAttributes
                  , bidsSignatoryPersonalNumber = just_lookup "cert.subject.serialnumber" crsAttributes
                  , bidsSignedText = tbs
                  , bidsSignature = mk_binary $ fromMaybe (missing "signature") crsSignature
                  , bidsOcspResponse = mk_binary $ just_lookup "Validation.ocsp.response" crsAttributes
                }
                dbUpdate $ ChargeCompanyForSEBankIDSignature did

          return $ Right crsProgressStatus
  where
    missing name = $unexpectedError $ "missing" <+> T.unpack name
    just_lookup name = fromMaybe (missing name) . lookup name

    invalid_b64 txt = $unexpectedError $ "invalid base64:" <+> T.unpack txt
    mk_binary txt = either (invalid_b64 txt) Binary . B64.decode . T.encodeUtf8 $ txt

----------------------------------------

-- | Fetch the document for e-signing. Checks that the document
-- is in the correct state and the signatory hasn't signed yet.
getDocumentAndSignatory :: (MonadDB m, MonadLog m, KontraMonad m, MonadThrow m,MonadBase IO m)
            => DocumentID -> SignatoryLinkID -> m (Document,SignatoryLink)
getDocumentAndSignatory did slid = dbQuery (GetDocumentSessionToken slid) >>= \case
  Just mh -> do
    logInfo_ "Document token found"
    doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkIDMagicHash did slid mh
    when (documentstatus doc /= Pending) $ do
      logInfo_ $ "Document is" <+> show (documentstatus doc) <+> ", should be" <+> show Pending
      respond404
    -- this should always succeed as we already got the document
    let slink = $fromJust $ getSigLinkFor slid doc
    when (hasSigned slink) $ do
      logInfo_ "Signatory already signed the document"
      respond404
    return (doc,slink)
  Nothing -> do
    logInfo_ "No document token found"
    respond404

getCompanyDisplayName :: (MonadDB m, MonadThrow m) => Document -> m (Maybe T.Text)
getCompanyDisplayName doc = fmap T.pack . companycgidisplayname . companyinfo
  <$> dbQuery (GetCompanyByUserID $ $fromJust $ maybesignatory author)
  where
    author = $fromJust $ getSigLinkFor signatoryisauthor doc

-- | Generate text to be signed that represents contents of the document.
textToBeSigned :: TemplatesMonad m => Document -> m String
textToBeSigned doc@Document{..} = renderLocalTemplate doc "tbs" $ do
  F.value "document_title" $ documenttitle
  F.value "document_id"   $ show documentid



guardThatPersonalNumberMatches :: Kontrakcja m => SignatoryLinkID -> String -> Document -> m ()
guardThatPersonalNumberMatches slid pn doc = case getSigLinkFor slid doc of
    Nothing -> do
      logInfo_ "Can't find signatory for eleg operation"
      respond404
    Just sl -> do
      let withoutDashes = filter (not . (== '-'))
          slPersonalNumber = withoutDashes $ getPersonalNumber sl
          pn' = withoutDashes pn
      if (slPersonalNumber /= "" && slPersonalNumber /= pn' && "19" ++ slPersonalNumber /= pn'  && slPersonalNumber /= "19" ++ pn')
        then do
          logInfo_ "Personal number for eleg operation does not match and signatory personal number can't be changed"
          respond404
        else return ()

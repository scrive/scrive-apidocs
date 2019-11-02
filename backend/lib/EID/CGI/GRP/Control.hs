module EID.CGI.GRP.Control (
    grpRoutes
  , checkCGISignStatus
  , CGISignStatus(..)
  , guardThatPersonalNumberMatches
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Unjson
import Happstack.Server hiding (dir)
import Happstack.StaticRouting
import Log
import Text.StringTemplates.Templates
import qualified Data.Aeson as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.StringTemplates.Fields as F

import Chargeable.Model
import DB hiding (InternalError)
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocStateQuery
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.DocUtils
import Doc.Model.Query
import Doc.SignatoryLinkID
import EID.Authentication.Model
import EID.CGI.GRP.Config
import EID.CGI.GRP.Transaction.Model
import EID.CGI.GRP.Types
import EID.Signature.Model
import EvidenceLog.Model
import Happstack.Fields
import Kontra hiding (InternalError)
import KontraLink
import Log.Identifier
import Network.SOAP.Call
import Network.SOAP.Transport.Curl (curlTransport)
import Network.XMLCurl
  ( CurlAuth(..), SSL(..), mkCertErrorHandler, mkDebugFunction )

import Routing
import Session.Cookies
import Session.Model
import Session.Types
import Templates
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils
import Util.SignatoryLinkUtils

grpRoutes :: Route (Kontra Response)
grpRoutes = dir "cgi" . dir "grp" $ choice
  [ dir "auth" . hPost . toK2 $ handleAuthRequest
  , dir "sign" . hPost . toK2 $ handleSignRequest
  , dir "checkcgiauthstatus" . hGet . toK2 $ handleCheckCGIAuthStatus
  , dir "checkcgiauthstatuswithredirect"
  . hGet
  . toK2
  $ handleCheckCGIAuthStatusWithRedirect
  ]

----------------------------------------

handleAuthRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleAuthRequest did slid = do
  CgiGrpConfig {..} <- do
    ctx <- getContext
    case ctx ^. #cgiGrpConfig of
      Nothing -> noConfigurationError "CGI Group"
      Just cc -> return cc
  (doc, _)              <- getDocumentAndSignatoryForEIDAuth did slid
  mcompany_display_name <- getCompanyDisplayName doc
  mcompany_service_id   <- getCompanyServiceID doc
  pn                    <- getField "personal_number" >>= \case
    (Just pn) -> return pn
    _         -> do
      logInfo_ "No personal number"
      respond404
  guardThatPersonalNumberMatches slid pn doc
  certErrorHandler <- mkCertErrorHandler
  debugFunction    <- mkDebugFunction
  let transport = curlTransport SecureSSL
                                (CurlAuthCert cgCertFile)
                                cgGateway
                                certErrorHandler
                                debugFunction
      req = AuthRequest { arqPolicy         = fromMaybe cgServiceID mcompany_service_id
                        , arqDisplayName = fromMaybe cgDisplayName mcompany_display_name
                        , arqPersonalNumber = pn
                        , arqProvider       = "bankid"
                        }
      parser = Right <$> xpAuthResponse <|> Left <$> xpGrpFault
  cgiResp <- soapCall transport "" () req parser >>= \case
    Left AlreadyInProgress -> do
      -- Action that is already in progress will get cancelled. In that case we do call again to get new transaction
      soapCall transport "" () req parser
    res -> return res
  case cgiResp of
    Left  fault                -> return $ unjsonToJSON unjsonDef fault
    Right sr@AuthResponse {..} -> do
      logInfo "SOAP response returned" $ logObject_ sr
      sess <- getCurrentSession
      dbUpdate $ MergeCgiGrpTransaction $ CgiGrpAuthTransaction slid
                                                                arsTransactionID
                                                                arsOrderRef
                                                                (sesID sess)
      return
        $ unjsonToJSON unjsonDef (arsAutoStartToken, sessionCookieInfoFromSession sess)
----------------------------------------

handleSignRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleSignRequest _did slid = do
  CgiGrpConfig {..} <- do
    ctx <- getContext
    case ctx ^. #cgiGrpConfig of
      Nothing -> noConfigurationError "CGI Group"
      Just cc -> return cc
  (doc, _)              <- getDocumentAndSignatoryForEIDSign slid
  mcompany_display_name <- getCompanyDisplayName doc
  mcompany_service_id   <- getCompanyServiceID doc
  tbs                   <- textToBeSigned doc
  pn                    <- getField "personal_number" >>= \case
    (Just pn) -> return pn
    _         -> do
      logInfo_ "No personal number"
      respond404
  guardThatPersonalNumberMatches slid pn doc
  certErrorHandler <- mkCertErrorHandler
  debugFunction    <- mkDebugFunction
  let
    transport = curlTransport SecureSSL
                              (CurlAuthCert cgCertFile)
                              cgGateway
                              certErrorHandler
                              debugFunction
    req = SignRequest
      { srqPolicy          = fromMaybe cgServiceID mcompany_service_id
      , srqDisplayName     = fromMaybe cgDisplayName mcompany_display_name
      , srqPersonalNumber  = pn
      , srqUserVisibleData = TE.decodeUtf8 . B64.encode . BSU.fromString . T.unpack $ tbs
      , srqProvider        = "bankid"
      }
    parser = Right <$> xpSignResponse <|> Left <$> xpGrpFault
  cgiResp <- soapCall transport "" () req parser >>= \case
    Left AlreadyInProgress -> do
      -- Action that is already in progress will get cancelled. In that case we do call again to get new transaction
      soapCall transport "" () req parser
    res -> return res
  case cgiResp of
    Left  fault                -> return $ unjsonToJSON unjsonDef fault
    Right sr@SignResponse {..} -> do
      logInfo "SOAP response returned" $ logObject_ sr
      sess <- getCurrentSession
      dbUpdate $ MergeCgiGrpTransaction $ CgiGrpSignTransaction slid
                                                                tbs
                                                                srsTransactionID
                                                                srsOrderRef
                                                                (sesID sess)
      return
        $ unjsonToJSON unjsonDef (srsAutoStartToken, sessionCookieInfoFromSession sess)

handleCheckCGIAuthStatus :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleCheckCGIAuthStatus did slid = do
  res <- checkCGIAuthStatus did slid
  case res of
    Left  e -> return $ unjsonToJSON unjsonDef e
    Right r -> return $ unjsonToJSON unjsonDef r

handleCheckCGIAuthStatusWithRedirect
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m KontraLink
handleCheckCGIAuthStatusWithRedirect did slid = do
  (msci :: Maybe SessionCookieInfo) <- readField "session_id"
  case msci of
    Nothing  -> internalError -- This should never happend
    Just sci -> do
      void $ guardJustM $ unsafeSessionTakeover sci
      return ()
  void $ checkCGIAuthStatus did slid -- There is no reason to process results of collect. We will redirect to link from param anyway
  murl <- getField "url"
  case murl of
    Nothing -> internalError -- This should never happend
    Just l  -> do
      return $ LinkExternal l


data CGISignStatus = CGISignStatusSuccess | CGISignStatusInProgress ProgressStatus | CGISignStatusFailed GrpFault | CGISignStatusAlreadySigned deriving Show

checkCGISignStatus
  :: (MonadDB m, MonadThrow m, MonadMask m, MonadLog m, MonadBaseControl IO m)
  => CgiGrpConfig
  -> DocumentID
  -> SignatoryLinkID
  -> m CGISignStatus
checkCGISignStatus CgiGrpConfig {..} did slid = do
  doc <- dbQuery $ GetDocumentByDocumentID did
  if (not (isPending doc) || isSignatoryAndHasSigned (getSigLinkFor slid doc))
    then return CGISignStatusAlreadySigned
    else do
      logInfo_ "Fetching signature"
      esignature <- dbQuery $ GetESignature slid
      if (isJust esignature)
        then return CGISignStatusSuccess
        else do
          mcompany_display_name <- getCompanyDisplayName doc
          mcompany_service_id   <- getCompanyServiceID doc
          mcgiTransaction       <- dbQuery (GetCgiGrpTransaction CgiGrpSign slid)
          logInfo_ "Getting transaction"

          case mcgiTransaction of
            Nothing             -> return $ CGISignStatusFailed ExpiredTransaction
            Just cgiTransaction -> do
              logInfo_ "Transaction fetch"
              certErrorHandler <- mkCertErrorHandler
              debugFunction    <- mkDebugFunction
              let transport = curlTransport SecureSSL
                                            (CurlAuthCert cgCertFile)
                                            cgGateway
                                            certErrorHandler
                                            debugFunction
                  req = CollectRequest
                    { crqPolicy        = fromMaybe cgServiceID mcompany_service_id
                    , crqTransactionID = cgiTransactionID cgiTransaction
                    , crqOrderRef      = cgiOrderRef cgiTransaction
                    , crqDisplayName   = fromMaybe cgDisplayName mcompany_display_name
                    }
                  parser = Right <$> xpCollectResponse <|> Left <$> xpGrpFault

              soapCall transport "" () req parser >>= \case
                Left fault -> do
                  logInfo "SOAP fault returned" $ object ["fault" .= show fault]
                  dbUpdate $ DeleteCgiGrpTransaction CgiGrpSign slid
                  return $ CGISignStatusFailed fault
                Right cr@CollectResponse {..} -> do
                  logInfo "SOAP response returned" $ object ["response" .= show cr]
                  case crsProgressStatus of
                    Complete -> do
                      dbUpdate $ DeleteCgiGrpTransaction CgiGrpSign slid
                      case cgiTransaction of
                        (CgiGrpSignTransaction _ tbs _ _ _) -> do
                          -- all the required attributes are supposed to always
                          -- be there, so bail out if this is not the case.
                          dbUpdate $ MergeCGISEBankIDSignature
                            slid
                            CGISEBankIDSignature
                              { cgisebidsSignatoryName = just_lookup "cert.subject.cn"
                                                                     crsAttributes
                              , cgisebidsSignatoryPersonalNumber =
                                just_lookup "cert.subject.serialnumber" crsAttributes
                              , cgisebidsSignatoryIP = just_lookup "ipAddress"
                                                                   crsAttributes
                              , cgisebidsSignedText = tbs
                              , cgisebidsSignature = mk_binary $ fromMaybe
                                                       (missing "signature")
                                                       crsSignature
                              , cgisebidsOcspResponse = mk_binary $ just_lookup
                                                          "Validation.ocsp.response"
                                                          crsAttributes
                              }
                          dbUpdate $ ChargeUserGroupForSEBankIDSignature did
                          return CGISignStatusSuccess
                        (CgiGrpAuthTransaction _ _ _ _) ->
                          unexpectedError
                            "Fetched CgiGrpAuthTransaction while expecting CgiGrpSignTransaction"
                    _ -> return $ CGISignStatusInProgress crsProgressStatus
  where
    missing name = unexpectedError $ "missing" <+> name
    just_lookup name = fromMaybe (missing name) . lookup name

    invalid_b64 txt = unexpectedError $ "invalid base64:" <+> txt
    mk_binary txt = either (invalid_b64 txt) identity . B64.decode . TE.encodeUtf8 $ txt

checkCGIAuthStatus
  :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m (Either GrpFault ProgressStatus)
checkCGIAuthStatus did slid = do
  CgiGrpConfig {..} <- do
    ctx <- getContext
    case ctx ^. #cgiGrpConfig of
      Nothing -> noConfigurationError "CGI Group"
      Just cc -> return cc
  (doc, sl)             <- getDocumentAndSignatoryForEIDAuth did slid
  mcompany_display_name <- getCompanyDisplayName doc
  mcompany_service_id   <- getCompanyServiceID doc
  mcgiTransaction       <- dbQuery (GetCgiGrpTransaction CgiGrpAuth slid)
  case mcgiTransaction of
    Nothing -> do
      sesid   <- view #sessionID <$> getContext
      success <- isJust <$> (dbQuery $ GetEAuthentication (mkAuthKind doc) sesid slid)
      if (success) then return $ Right Complete else return $ Left ExpiredTransaction
    Just cgiTransaction -> do
      certErrorHandler <- mkCertErrorHandler
      debugFunction    <- mkDebugFunction
      let transport = curlTransport SecureSSL
                                    (CurlAuthCert cgCertFile)
                                    cgGateway
                                    certErrorHandler
                                    debugFunction
          req = CollectRequest
            { crqPolicy        = fromMaybe cgServiceID mcompany_service_id
            , crqTransactionID = cgiTransactionID cgiTransaction
            , crqOrderRef      = cgiOrderRef cgiTransaction
            , crqDisplayName   = fromMaybe cgDisplayName mcompany_display_name
            }
          parser = Right <$> xpCollectResponse <|> Left <$> xpGrpFault

      soapCall transport "" () req parser >>= \case
        Left fault -> do
          logInfo "SOAP fault returned" $ object ["fault" .= show fault]
          dbUpdate $ DeleteCgiGrpTransaction CgiGrpAuth slid
          return $ Left fault
        Right cr@CollectResponse {..} -> do
          logInfo "SOAP response returned" $ object ["response" .= show cr]
          if (crsProgressStatus == Complete)
            then do
              dbUpdate $ DeleteCgiGrpTransaction CgiGrpAuth slid
              case cgiTransaction of
                (CgiGrpAuthTransaction _ _ _ session_id) -> do
                  -- all the required attributes are supposed to always
                  -- be there, so bail out if this is not the case.
                  let signatoryName = just_lookup "cert.subject.cn" crsAttributes
                  -- Sometimes cert.subject.serialnumber is missing, then we use Subject.SerialNumber (Case 3320)
                  let subjectSerialNumber =
                        just_lookup "Subject.SerialNumber" crsAttributes
                  let signatoryPersonalNumber = fromMaybe subjectSerialNumber
                        $ lookup "cert.subject.serialnumber" crsAttributes
                  let signature =
                        mk_binary $ fromMaybe (missing "signature") crsSignature
                  let ocspResponse =
                        mk_binary $ just_lookup "Validation.ocsp.response" crsAttributes
                  let ipAddress = just_lookup "ipAddress" crsAttributes

                  dbUpdate $ MergeCGISEBankIDAuthentication
                    (mkAuthKind doc)
                    session_id
                    slid
                    CGISEBankIDAuthentication
                      { cgisebidaSignatoryName           = signatoryName
                      , cgisebidaSignatoryPersonalNumber = signatoryPersonalNumber
                      , cgisebidaSignatoryIP             = ipAddress
                      , cgisebidaSignature               = signature
                      , cgisebidaOcspResponse            = ocspResponse
                      }
                  ctx <- getContext
                  -- Do not update evidence log if document is closed
                  -- (authentication to view archived case).
                  unless (isClosed doc) . withDocument doc $ do
                    let eventFields = do
                          F.value "hide_pn" $ signatorylinkhidepn sl
                          F.value "signatory_name" signatoryName
                          F.value "signatory_personal_number" signatoryPersonalNumber
                          F.value "signatory_ip" ipAddress
                          F.value "provider_sebankid" True
                          F.value "signature" $ B64.encode signature
                          F.value "ocsp_response" $ B64.encode ocspResponse
                    void
                      $   dbUpdate
                      .   InsertEvidenceEventWithAffectedSignatoryAndMsg
                            AuthenticatedToViewEvidence
                            (eventFields)
                            (Just sl)
                            Nothing
                      =<< signatoryActor ctx sl
                  dbUpdate $ ChargeUserGroupForSEBankIDAuthentication did
                  return $ Right Complete
                (CgiGrpSignTransaction _ _ _ _ _) ->
                  unexpectedError
                    "Fetched CgiGrpSignTransaction while expecting CgiGrpAuthTransaction"
            else return $ Right crsProgressStatus

  where
    missing name = unexpectedError $ "missing" <+> name
    just_lookup name = fromMaybe (missing name) . lookup name

    invalid_b64 txt = unexpectedError $ "invalid base64:" <+> txt
    mk_binary txt = either (invalid_b64 txt) identity . B64.decode . TE.encodeUtf8 $ txt

----------------------------------------

getCompanyDisplayName :: (MonadDB m, MonadThrow m) => Document -> m (Maybe Text)
getCompanyDisplayName doc = view #cgiDisplayName . ugwpSettings <$> dbQuery
  (UserGroupGetWithParentsByUserID $ fromJust $ maybesignatory author)
  where author = fromJust $ getSigLinkFor signatoryisauthor doc

getCompanyServiceID :: (MonadDB m, MonadThrow m) => Document -> m (Maybe Text)
getCompanyServiceID doc = view #cgiServiceID . ugwpSettings <$> dbQuery
  (UserGroupGetWithParentsByUserID $ fromJust $ maybesignatory author)
  where author = fromJust $ getSigLinkFor signatoryisauthor doc

-- | Generate text to be signed that represents contents of the document.
textToBeSigned :: TemplatesMonad m => Document -> m Text
textToBeSigned doc@Document {..} = renderLocalTemplate doc "tbs" $ do
  F.value "document_title" $ documenttitle
  F.value "document_id" $ show documentid

guardThatPersonalNumberMatches
  :: Kontrakcja m => SignatoryLinkID -> Text -> Document -> m ()
guardThatPersonalNumberMatches slid pn doc = case getSigLinkFor slid doc of
  Nothing -> do
    logInfo "Can't find signatory for eleg operation"
      $ object [identifier $ documentid doc, identifier slid]
    respond404
  Just sl -> do
    let withoutDashes    = T.filter (not . (`elem` ['-', '+']))
        slPersonalNumber = withoutDashes $ getPersonalNumber sl
        pn'              = withoutDashes pn
    if (  slPersonalNumber
       /= ""
       && slPersonalNumber
       /= pn'
       && "19"
       <> slPersonalNumber
       /= pn'
       && slPersonalNumber
       /= "19"
       <> pn'
       && slPersonalNumber
       /= "20"
       <> pn'
       && "20"
       <> slPersonalNumber
       /= pn'
       )
    then
      do
        logInfo
            "Personal number for eleg operation does not match and signatory personal number can't be changed"
          $ object [identifier $ documentid doc, identifier slid]
        respond404
    else
      return ()

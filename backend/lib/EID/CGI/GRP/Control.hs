module EID.CGI.GRP.Control (
    grpRoutes
  , checkCGISignStatus
  , CGISignStatus(..)
  , handleAuthRequest  -- for testing purposes
  , handleSignRequest  -- for testing purposes
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

import AccessControl.Check
import AccessControl.Model
import AccessControl.Types
import Chargeable
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
import EventStream.Class
import EvidenceLog.Model
import Happstack.Fields
import Kontra hiding (InternalError)
import KontraLink
import Log.Identifier
import Network.SOAP.Call
import Network.SOAP.Transport.Curl (curlTransport)
import Network.XMLCurl
  ( CurlAuth(..), SSL(..), mkCertErrorHandler, mkDebugFunction
  )
import Routing
import Session.Cookies
import Session.Model
import Session.Types
import Templates
import User.Model
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
  , (dir "checkcgiauthstatuswithredirect" . hGet . toK2)
    handleCheckCGIAuthStatusWithRedirect
  ]

----------------------------------------

handleAuthRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleAuthRequest did slid = do
  ctx                    <- getContext
  cgiGrpConfig@CgiGrpConfig {..} <- getCgiGrpConfig
  (doc     , _         ) <- getDocumentAndSignatoryForEIDAuth did slid
  (authorid, ugidforeid) <- getAuthorAndUserGroupForEID doc
  author <- fromMaybe (unexpectedError "Impossible happened: author doesn't exist.")
    <$> dbQuery (GetUserByID authorid)
  guardUserMayImpersonateUserGroupForEid author doc
  (display_name, service_id) <- getCompanyDisplayNameAndServiceID cgiGrpConfig ugidforeid
  pn <- getField "personal_number" >>= \case
    (Just pn) -> return pn
    _         -> do
      logInfo_ "No personal number"
      respond404
  guardThatSwedishPersonalNumberMatches slid pn doc
  certErrorHandler <- mkCertErrorHandler
  debugFunction    <- mkDebugFunction
  let transport = curlTransport SecureSSL
                                (CurlAuthCert cgCertFile)
                                cgGateway
                                certErrorHandler
                                debugFunction
      req = AuthRequest { arqPolicy         = service_id
                        , arqDisplayName    = display_name
                        , arqPersonalNumber = pn
                        , arqProvider       = "bankid"
                        , arqClientIP       = showt (ctx ^. #ipAddr)
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
      dbUpdate . MergeCgiGrpTransaction $ CgiGrpAuthTransaction slid
                                                                arsTransactionID
                                                                arsOrderRef
                                                                (sesID sess)
      return
        $ unjsonToJSON unjsonDef (arsAutoStartToken, sessionCookieInfoFromSession sess)
----------------------------------------

handleSignRequest :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m A.Value
handleSignRequest _did slid = do
  ctx                    <- getContext
  cgiGrpConfig@CgiGrpConfig {..} <- getCgiGrpConfig
  (doc     , _         ) <- getDocumentAndSignatoryForEIDSign slid
  (authorid, ugidforeid) <- getAuthorAndUserGroupForEID doc
  author <- fromMaybe (unexpectedError "Impossible happened: author doesn't exist.")
    <$> dbQuery (GetUserByID authorid)
  guardUserMayImpersonateUserGroupForEid author doc
  (display_name, service_id) <- getCompanyDisplayNameAndServiceID cgiGrpConfig ugidforeid
  tbs <- textToBeSigned doc
  pn  <- getField "personal_number" >>= \case
    (Just pn) -> return pn
    _         -> do
      logInfo_ "No personal number"
      respond404
  guardThatSwedishPersonalNumberMatches slid pn doc
  certErrorHandler <- mkCertErrorHandler
  debugFunction    <- mkDebugFunction
  let transport = curlTransport SecureSSL
                                (CurlAuthCert cgCertFile)
                                cgGateway
                                certErrorHandler
                                debugFunction
      req = SignRequest
        { srqPolicy          = service_id
        , srqDisplayName     = display_name
        , srqPersonalNumber  = pn
        , srqUserVisibleData = TE.decodeUtf8
                               . B64.encode
                               . BSU.fromString
                               . T.unpack
                               $ tbs
        , srqProvider        = "bankid"
        , srqClientIP        = showt (ctx ^. #ipAddr)
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
      dbUpdate . MergeCgiGrpTransaction $ CgiGrpSignTransaction slid
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
    Just sci -> void . guardJustM $ unsafeSessionTakeover sci
  void $ checkCGIAuthStatus did slid -- There is no reason to process results of collect. We will redirect to link from param anyway
  murl <- getField "url"
  case murl of
    Nothing -> internalError -- This should never happend
    Just l  -> do
      return $ LinkExternal l


data CGISignStatus = CGISignStatusSuccess | CGISignStatusInProgress ProgressStatus | CGISignStatusFailed GrpFault | CGISignStatusAlreadySigned deriving Show

checkCGISignStatus
  :: ( MonadDB m
     , MonadThrow m
     , MonadMask m
     , MonadLog m
     , MonadBaseControl IO m
     , MonadEventStream m
     )
  => CgiGrpConfig
  -> DocumentID
  -> SignatoryLinkID
  -> m CGISignStatus
checkCGISignStatus cgiGrpConfig@CgiGrpConfig {..} did slid = do
  doc <- dbQuery $ GetDocumentByDocumentIDSignatoryLinkID did slid
  if not (isPending doc) || isSignatoryAndHasSigned (getSigLinkFor slid doc)
    then return CGISignStatusAlreadySigned
    else do
      logInfo_ "Fetching signature"
      esignature <- dbQuery $ GetESignature slid
      if isJust esignature
        then return CGISignStatusSuccess
        else do
          (_           , ugidforeid) <- getAuthorAndUserGroupForEID doc
          (display_name, service_id) <- getCompanyDisplayNameAndServiceID cgiGrpConfig
                                                                          ugidforeid
          mcgiTransaction <- dbQuery (GetCgiGrpTransaction CgiGrpSign slid)
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
                    { crqPolicy        = service_id
                    , crqTransactionID = cgiTransactionID cgiTransaction
                    , crqOrderRef      = cgiOrderRef cgiTransaction
                    , crqDisplayName   = display_name
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
                          chargeForItemSpecificUserGroup CISEBankIDSignature
                                                         did
                                                         ugidforeid
                                                         1
                          return CGISignStatusSuccess
                        CgiGrpAuthTransaction{} ->
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
  cgiGrpConfig@CgiGrpConfig {..} <- getCgiGrpConfig
  (doc         , sl        )     <- getDocumentAndSignatoryForEIDAuth did slid
  (_           , ugidforeid)     <- getAuthorAndUserGroupForEID doc
  (display_name, service_id) <- getCompanyDisplayNameAndServiceID cgiGrpConfig ugidforeid
  mcgiTransaction                <- dbQuery (GetCgiGrpTransaction CgiGrpAuth slid)
  case mcgiTransaction of
    Nothing -> do
      sesid   <- view #sessionID <$> getContext
      success <- isJust <$> dbQuery (GetEAuthentication (mkAuthKind doc) sesid slid)
      if success then return $ Right Complete else return $ Left ExpiredTransaction
    Just cgiTransaction -> do
      certErrorHandler <- mkCertErrorHandler
      debugFunction    <- mkDebugFunction
      let transport = curlTransport SecureSSL
                                    (CurlAuthCert cgCertFile)
                                    cgGateway
                                    certErrorHandler
                                    debugFunction
          req = CollectRequest { crqPolicy        = service_id
                               , crqTransactionID = cgiTransactionID cgiTransaction
                               , crqOrderRef      = cgiOrderRef cgiTransaction
                               , crqDisplayName   = display_name
                               }
          parser = Right <$> xpCollectResponse <|> Left <$> xpGrpFault

      soapCall transport "" () req parser >>= \case
        Left fault -> do
          logInfo "SOAP fault returned" $ object ["fault" .= show fault]
          dbUpdate $ DeleteCgiGrpTransaction CgiGrpAuth slid
          return $ Left fault
        Right cr@CollectResponse {..} -> do
          logInfo "SOAP response returned" $ object ["response" .= show cr]
          if crsProgressStatus == Complete
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
                            eventFields
                            (Just sl)
                            Nothing
                      =<< signatoryActor ctx sl
                  chargeForItemSpecificUserGroup CISEBankIDAuthentication did ugidforeid 1
                  return $ Right Complete
                CgiGrpSignTransaction{} ->
                  unexpectedError
                    "Fetched CgiGrpSignTransaction while expecting CgiGrpAuthTransaction"
            else return $ Right crsProgressStatus

  where
    missing name = unexpectedError $ "missing" <+> name
    just_lookup name = fromMaybe (missing name) . lookup name

    invalid_b64 txt = unexpectedError $ "invalid base64:" <+> txt
    mk_binary txt = either (invalid_b64 txt) identity . B64.decode . TE.encodeUtf8 $ txt

----------------------------------------

-- | For a given document compute the author as well as the user group whose
-- identity is to be used for the transaction. The latter affects display name
-- (see getCompanyDisplayNameAndServiceID) as well as billing.
getAuthorAndUserGroupForEID
  :: (MonadDB m, MonadThrow m) => Document -> m (UserID, UserGroupID)
getAuthorAndUserGroupForEID doc = do
  let mauthorid = (maybesignatory =<<) $ getAuthorSigLink doc
      authorid  = fromMaybe
        (unexpectedError "Impossible happened: document doesn't have an author.")
        mauthorid
  author <- fromMaybe (unexpectedError "Impossible happened: author does not exist.")
    <$> dbQuery (GetUserByID authorid)
  -- If the 'user_group_to_impersonate_for_eid' field is set, use that user
  -- group, otherwise use the author's user group.
  let ugidforeid | Just ugid <- documentusergroupforeid doc = ugid
                 | otherwise = author ^. #groupID
  return (authorid, ugidforeid)

-- | Helper to ensure consistent error messages.
getCgiGrpConfig :: Kontrakcja m => m CgiGrpConfig
getCgiGrpConfig = do
  ctx <- getContext
  case ctx ^. #cgiGrpConfig of
    Just cfg -> return cfg
    Nothing  -> noConfigurationError "CGI Group"

-- | For a given user group compute the display name (i.e. the name displayed to
-- the user on BankID) and service id.
getCompanyDisplayNameAndServiceID
  :: (MonadDB m, MonadThrow m) => CgiGrpConfig -> UserGroupID -> m (Text, Text)
getCompanyDisplayNameAndServiceID CgiGrpConfig {..} ugid = do
  ugwp <- fromMaybe (unexpectedError "Impossible happened: user group does not exist.")
    <$> dbQuery (UserGroupGetWithParents ugid)
  let ugsettings   = ugwpSettings ugwp
  let mdisplayname = view #cgiDisplayName ugsettings
  let mserviceid   = view #cgiServiceID ugsettings
  -- If no display name and/or service ID is set, use Scrive's.
  let displayname  = fromMaybe cgDisplayName mdisplayname
  let serviceid    = fromMaybe cgServiceID mserviceid
  return (displayname, serviceid)

-- | Generate text to be signed that represents contents of the document.
textToBeSigned :: TemplatesMonad m => Document -> m Text
textToBeSigned doc@Document {..} = renderLocalTemplate doc "tbs" $ do
  F.value "document_title" documenttitle
  F.value "document_id" $ show documentid

guardThatSwedishPersonalNumberMatches
  :: Kontrakcja m => SignatoryLinkID -> Text -> Document -> m ()
guardThatSwedishPersonalNumberMatches slid pn doc = case getSigLinkFor slid doc of
  Nothing -> do
    logInfo "Can't find signatory for Swedish BankID operation"
      $ object [identifier $ documentid doc, identifier slid]
    respond404
  Just sl -> do
    let withoutDashes    = T.filter (not . (`elem` ['-', '+']))
        slPersonalNumber = withoutDashes $ getPersonalNumber sl
        pn'              = withoutDashes pn

        pnInSignatoryLinkIsEmpty = slPersonalNumber == ""
        pnsMatchPossiblyWithPrefix = or
          [ prefix1 <> slPersonalNumber == prefix2 <> pn'
          | prefix1 <- prefixes
          , prefix2 <- prefixes
          ]
          where prefixes = ["", "19", "20"]

    if pnInSignatoryLinkIsEmpty || pnsMatchPossiblyWithPrefix
      then return ()
      else do
        logInfo
            "Personal number for Swedish BankID operation does not match and signatory personal number can't be changed"
          $ object [identifier $ documentid doc, identifier slid]
        respond404

guardUserMayImpersonateUserGroupForEid :: Kontrakcja m => User -> Document -> m ()
guardUserMayImpersonateUserGroupForEid user doc
  | Just ugid <- documentusergroupforeid doc = do
    roles <- dbQuery . GetRoles $ user
    requiredPerm <- alternativePermissionCondition $ canDo ReadA $ EidIdentityR ugid
    let
      action = do
        logInfo
            "Someone tried to use Swedish BankID on a document without\
            \ sufficient permissions to use the specified display name\
            \ (via user_group_to_impersonate_for_eid)."
          $ object
              [identifier $ documentid doc, identifier $ user ^. #id, identifier ugid]
        respond404
    accessControl roles requiredPerm action $ return ()
guardUserMayImpersonateUserGroupForEid _ _ = return ()

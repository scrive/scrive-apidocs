module EID.EIDService.Provider.DKNemID (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  , dateOfBirthFromDKPersonalNumber
  , completeEIDServiceSignTransaction
  , DKNemIDEIDServiceAuthCompletionData(..)
  , DKNemIDEIDServiceSignCompletionData(..)
  ) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Log
import Text.StringTemplates.Templates
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.StringTemplates.Fields as F

import Chargeable
import DB
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.DocUtils
import EID.Authentication.Model
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Model
import EID.EIDService.Types
import EvidenceLog.Model
import FlashMessage
import Happstack.Fields
import Kontra
import Session.Model
import Templates
import User.Lang
import Util.Actor
import Util.HasSomeUserInfo
import qualified InputValidation as IV

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderDKNemID

data DKNemIDEIDServiceProviderAuthParams = DKNemIDEIDServiceProviderAuthParams {
    cdkestUILocale :: Lang
  , cdkestLimitedClientMode :: Bool
  , cdkestRequestCPR :: Bool
  , cdkestMethod :: EIDServiceDKNemIDInternalProvider
  }

instance ToJSON DKNemIDEIDServiceProviderAuthParams where
  toJSON req = object
    [ "limitedClientMode" .= cdkestLimitedClientMode req
    , "uiLocale" .= localeText
    , "requestCPR" .= cdkestRequestCPR req
    , "method" .= cdkestMethod req
    ]
    where
      localeText :: Text
      localeText = case cdkestUILocale req of
        LANG_SV -> "sv-SE"
        LANG_NO -> "nb-NO"
        LANG_DA -> "da-DK"
        _       -> "en-GB"

newtype StartDKNemIDEIDServiceTransactionResponse = StartDKNemIDEIDServiceTransactionResponse {
    sdkestAuthURL :: Text
  }

instance FromJSON StartDKNemIDEIDServiceTransactionResponse where
  parseJSON outer =
    StartDKNemIDEIDServiceTransactionResponse
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: eidServiceFieldName)
          >>= withObject "object" (.: "authUrl")
          )
    where eidServiceFieldName = toEIDServiceProviderName provider <> "Auth"

data DKNemIDAuthMethod = DKNemIDAuthCPR | DKNemIDAuthPID | DKNemIDAuthCVR deriving (Show, Eq)

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf (EIDServiceAuthToView authToViewKind) doc sl =
  beginAuthTransaction conf authToViewKind doc sl
beginEIDServiceTransaction conf EIDServiceAuthToSign doc sl =
  beginSignTransaction conf doc sl

beginAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> AuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginAuthTransaction conf authToViewKind doc sl = do
  ctx             <- getContext
  mkontraRedirect <- getField "redirect"
  nemIDMethod     <- getField "nemid_method" >>= resolveDKNemIDAuthMethod
  let redirectUrl = UnifiedRedirectUrl
        { redDomain          = ctx ^. #brandedDomain % #url
        , redProvider        = provider
        , redDocumentID      = documentid doc
        , redAuthKind        = EIDServiceAuthToView authToViewKind
        , redSignatoryLinkID = signatorylinkid sl
        , redPostRedirectUrl = mkontraRedirect
        }
  let requestCPR = nemIDMethod == EIDServiceNemIDPersonalKeyCard
      createReq  = CreateEIDServiceTransactionRequest
        { cestProvider           = provider
        , cestMethod             = EIDServiceAuthMethod
        , cestRedirectUrl        = showt redirectUrl
        , cestProviderParameters = Just . toJSON $ DKNemIDEIDServiceProviderAuthParams
                                     { cdkestUILocale          = documentlang doc
                                     , cdkestLimitedClientMode = True
                                     , cdkestRequestCPR        = requestCPR
                                     , cdkestMethod            = nemIDMethod
                                     }
        }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- sdkestAuthURL <$> startTransactionWithEIDService conf provider tid
  chargeForItemSingle CIDKNemIDAuthenticationStarted $ documentid doc
  return (tid, object ["accessUrl" .= turl], EIDServiceTransactionStatusStarted)
  where
    resolveDKNemIDAuthMethod = \case
      Nothing             -> return EIDServiceNemIDPersonalKeyCard
      Just "dk_nemid_cpr" -> return EIDServiceNemIDPersonalKeyCard
      Just "dk_nemid_pid" -> return EIDServiceNemIDPersonalKeyCard
      Just "dk_nemid_cvr_keyfile" -> return EIDServiceNemIDEmployeeKeyFile
      Just "dk_nemid_cvr_keycard" -> return EIDServiceNemIDEmployeeKeyCard
      Just s              -> do
        logAttention_ $ "No such DK NemID auth method" <> s
        internalError

data DKNemIDEIDServiceAuthCompletionData = DKNemIDEIDServiceAuthCompletionData
  { eidnidInternalProvider :: EIDServiceDKNemIDInternalProvider
  , eidnidBirthDate :: Maybe T.Text
  , eidnidCertificate :: T.Text
  , eidnidDistinguishedName :: T.Text
  , eidnidPID :: Maybe T.Text
  , eidnidCPR :: Maybe T.Text
  , eidnidOrganisationData :: DKNemIDEIDServiceAuthCompletionOrganisationData
  } deriving (Eq, Ord, Show)

data DKNemIDEIDServiceAuthCompletionOrganisationData = DKNemIDEIDServiceAuthCompletionOrganisationData
  {
    eidnidoOrganisationName :: Maybe T.Text
  , eidnidoOrganisationNumber :: Maybe T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON DKNemIDEIDServiceAuthCompletionData where
  parseJSON outer = do
    providerParams <-
      withObject "object" (.: "providerParameters") outer
      >>= withObject "object" (.: "auth")
      >>= withObject "object" (.: providerName)
    ip <- withObject "object" (.:? "method") providerParams
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: providerAuth)
      >>= withObject "object" (.: "completionData")
      >>= withObject
            "object"
            (\o -> do
              pid              <- o .:? "pid"
              cpr              <- o .:? "ssn"
              organisationData <- o .: "organisationData" >>= withObject
                "organisationData"
                (\od ->
                  DKNemIDEIDServiceAuthCompletionOrganisationData
                    <$> od
                    .:  "organisationName"
                    <*> od
                    .:  "organisationNumber"
                )
              let dob = dateOfBirthFromDKPersonalNumber <$> cpr
              (cer, dn) <- o .: "certificateData" >>= withObject
                "object"
                (\cd -> (,) <$> cd .: "certificate" <*> cd .: "distinguishedName")
              return DKNemIDEIDServiceAuthCompletionData
                { eidnidInternalProvider  = fromMaybe EIDServiceNemIDPersonalKeyCard ip
                , eidnidCPR               = cpr
                , eidnidBirthDate         = dob
                , eidnidCertificate       = cer
                , eidnidDistinguishedName = dn
                , eidnidPID               = pid
                , eidnidOrganisationData  = organisationData
                }
            )
    where
      providerName = toEIDServiceProviderName provider
      providerAuth = providerName <> "Auth"

completeEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> Document
  -> SignatoryLink
  -> m (Maybe EIDServiceTransactionStatus)
completeEIDServiceAuthTransaction conf doc sl = do
  sessionID <- getNonTempSessionID
  let authKind = EIDServiceAuthToView $ mkAuthKind doc
  runMaybeT $ do
    Just estDB <- dbQuery
      $ GetEIDServiceTransactionGuardSessionID sessionID (signatorylinkid sl) authKind
    Just trans <- getTransactionFromEIDService conf provider (estID estDB)
    let eidServiceStatus = estRespStatus trans
        dbStatus         = estStatus estDB
    if eidServiceStatus == dbStatus
      then return eidServiceStatus
      else finaliseTransaction doc sl estDB trans

getDKNemIDAuthMethod
  :: (MonadFail m) => EIDServiceAuthenticationKind -> SignatoryLink -> m DKNemIDAuthMethod
getDKNemIDAuthMethod kind sl = case kind of
  EIDServiceAuthToView authToViewKind -> do
    case getAuthToViewMethod authToViewKind of
      DKNemIDCPRAuthenticationToView -> return DKNemIDAuthCPR
      DKNemIDPIDAuthenticationToView -> return DKNemIDAuthPID
      DKNemIDCVRAuthenticationToView -> return DKNemIDAuthCVR
      LegacyDKNemIDAuthenticationToView -> return DKNemIDAuthCPR
      auth -> fail $ "Not supported auth" <> show auth
    where
      getAuthToViewMethod k = case k of
        AuthenticationToView -> signatorylinkauthenticationtoviewmethod sl
        AuthenticationToViewArchived ->
          signatorylinkauthenticationtoviewarchivedmethod sl
  EIDServiceAuthToSign -> fail "DKNemID to sign not supported yet"

finaliseTransaction
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceTransactionFromDB
  -> EIDServiceTransactionResponse DKNemIDEIDServiceAuthCompletionData
  -> m EIDServiceTransactionStatus
finaliseTransaction doc sl estDB trans = validateAuthCompletionData sl trans >>= \case
  Nothing -> do
    let status = EIDServiceTransactionStatusCompleteAndFailed
    mergeEIDServiceTransactionWithStatus status
    return status
  Just cd -> do
    let status   = EIDServiceTransactionStatusCompleteAndSuccess
        authKind = estAuthKind estDB
    mergeEIDServiceTransactionWithStatus status
    updateDBTransactionWithAuthCompletionData doc sl authKind cd
    updateEvidenceLogWithAuth doc sl authKind cd
    chargeForItemSingle CIDKNemIDAuthenticationFinished $ documentid doc
    return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }

validateAuthCompletionData
  :: Kontrakcja m
  => SignatoryLink
  -> EIDServiceTransactionResponse DKNemIDEIDServiceAuthCompletionData
  -> m (Maybe DKNemIDEIDServiceAuthCompletionData)
validateAuthCompletionData sl trans = case estRespCompletionData trans of
  Nothing -> return Nothing
  Just cd -> do
    case eidnidInternalProvider cd of
      EIDServiceNemIDPersonalKeyCard -> validateCPR cd
      EIDServiceNemIDEmployeeKeyFile -> validateCVR cd
      -- TODO this one is actually not supported right now
      EIDServiceNemIDEmployeeKeyCard -> return $ Just cd
  where
    validateCPR
      :: Kontrakcja m
      => DKNemIDEIDServiceAuthCompletionData
      -> m (Maybe DKNemIDEIDServiceAuthCompletionData)
    validateCPR cd = do
      let cprFromEIDService    = normalizeCPR <$> eidnidCPR cd
          cprFromSignatoryLink = normalizeCPR $ getPersonalNumber sl
      if isJust cprFromEIDService && cprFromEIDService /= Just cprFromSignatoryLink
        then do
          logAttention "SSN from NETS does not match SSN from SignatoryLink." $ object
            [ "ssn_sl" .= cprFromSignatoryLink
            , "ssn_eidhub" .= cprFromEIDService
            , "provider" .= ("dk_nemid_cpr" :: Text)
            ]
          flashMessageUserHasIdentifiedWithDifferentSSN
            >>= addFlashCookie
            .   toCookieValue
          return Nothing
        else return $ Just cd
    validateCVR
      :: Kontrakcja m
      => DKNemIDEIDServiceAuthCompletionData
      -> m (Maybe DKNemIDEIDServiceAuthCompletionData)
    validateCVR cd = do
      let cvrFromEIDService    = eidnidoOrganisationNumber . eidnidOrganisationData $ cd
          cvrFromSignatoryLink = Just . getPersonalNumber $ sl
      if cvrFromEIDService /= cvrFromSignatoryLink
        then do
          logAttention
              "CVR number from Eid service differs from the one in signatory link"
            $ object
                [ "cvr_sl" .= cvrFromSignatoryLink
                , "cvr_eidhub" .= cvrFromEIDService
                , "provider" .= ("dk_nemid_cvr" :: Text)
                ]
          flashMessageUserHasIdentifiedWithDifferentSSN
            >>= addFlashCookie
            .   toCookieValue
          return Nothing
        else return $ Just cd
    normalizeCPR :: Text -> Text
    normalizeCPR = T.filter (/= '-')
    flashMessageUserHasIdentifiedWithDifferentSSN :: TemplatesMonad m => m FlashMessage
    flashMessageUserHasIdentifiedWithDifferentSSN = toFlashMsg OperationFailed
      <$> renderTemplate_ "flashMessageUserHasIdentifiedWithDifferentSSN"

updateDBTransactionWithAuthCompletionData
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceAuthenticationKind
  -> DKNemIDEIDServiceAuthCompletionData
  -> m ()
updateDBTransactionWithAuthCompletionData doc sl authKind cd = do
  authMethod <- getDKNemIDAuthMethod authKind sl
  let signatoryName            = cnFromDN $ eidnidDistinguishedName cd
      birthDate                = eidnidBirthDate cd
      certificate              = decodeCertificate $ eidnidCertificate cd
      internalProvider         = eidnidInternalProvider cd
      personalOrEmployeeNumber = if authMethod == DKNemIDAuthPID
        then fromMaybe "" $ eidnidPID cd
        else getPersonalNumber sl
      auth = EIDServiceDKNemIDAuthentication
        { eidServiceNemIDInternalProvider             = internalProvider
        , eidServiceNemIDSignatoryPersonalOrCVRNumber = personalOrEmployeeNumber
        , eidServiceNemIDSignatoryName                = signatoryName
        , eidServiceNemIDDateOfBirth                  = birthDate
        , eidServiceNemIDCertificate                  = certificate
        }
  sessionID <- getNonTempSessionID
  dbUpdate
    . MergeDocumentEidAuthentication (mkAuthKind doc) sessionID (signatorylinkid sl)
    $ EIDServiceNemIDAuthentication_ auth

updateEvidenceLogWithAuth
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceAuthenticationKind
  -> DKNemIDEIDServiceAuthCompletionData
  -> m ()
updateEvidenceLogWithAuth doc sl authKind cd = do
  ctx        <- getContext
  authMethod <- getDKNemIDAuthMethod authKind sl
  let
    pid           = eidnidPID cd
    signatoryName = cnFromDN $ eidnidDistinguishedName cd
    birthDate     = eidnidBirthDate cd
    certificate   = decodeCertificate $ eidnidCertificate cd
    eventFields   = do
      F.value "signatory_name" signatoryName
      when (authMethod == DKNemIDAuthCPR)
        . F.value "signatory_personal_number"
        $ eidnidCPR cd
      when (authMethod == DKNemIDAuthCVR)
        . F.value "signatory_org_number"
        . eidnidoOrganisationNumber
        $ eidnidOrganisationData cd
      F.value "provider_dknemid" True
      F.value "signatory_dob" birthDate
      F.value "signatory_pid" pid
      F.value "signature" $ B64.encode certificate
  withDocument doc
    .   when (mkAuthKind doc == AuthenticationToView)
    .   void
    $   dbUpdate
    .   InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence
                                                       eventFields
                                                       (Just sl)
                                                       Nothing
    =<< signatoryActor ctx sl

cnFromDN :: Text -> Text
cnFromDN dn = fromMaybe parseError . lookup "CN" $ fmap
  parsePair
  (concatMap (T.splitOn " + ") $ T.splitOn ", " dn)
  where
    parsePair s = case T.splitOn "=" s of
      (name : values) -> (name, T.intercalate "=" values)
      _               -> unexpectedError $ "Cannot parse DN value: " <> dn
    parseError = unexpectedError $ "Cannot parse DN value: " <> dn

decodeCertificate :: Text -> BSC8.ByteString
decodeCertificate =
  either (unexpectedError "invalid base64 of NemID certificate") identity
    . B64.decode
    . T.encodeUtf8

dateOfBirthFromDKPersonalNumber :: Text -> Text
dateOfBirthFromDKPersonalNumber personalnumber =
  case T.chunksOf 2 $ T.take 6 personalnumber of
    [day, month, year] ->
      let
        yearWithoutCentury = read year
        firstDigitOfSequenceNumber = T.index personalnumber 7
        century = showt $ resolveCentury yearWithoutCentury firstDigitOfSequenceNumber
      in
        day <> "." <> month <> "." <> century <> year
    _ ->
      unexpectedError
        $  "This personal number cannot be formatted to date: "
        <> personalnumber
  where
    resolveCentury :: Int -> Char -> Int
    resolveCentury yearWithoutCentury firstDigitOfSequenceNumber
      | firstDigitOfSequenceNumber < '4'
      = 19
      | firstDigitOfSequenceNumber == '4'
      = if yearWithoutCentury < 37 then 20 else 19
      | firstDigitOfSequenceNumber > '4' && firstDigitOfSequenceNumber < '9'
      = if yearWithoutCentury < 58 then 20 else 18
      | otherwise
      = if yearWithoutCentury < 37 then 20 else 19

data DKNemIDEIDServiceProviderSignParams = DKNemIDEIDServiceProviderSignParams
  {
     cdkestsMethod :: EIDServiceDKNemIDInternalProvider
   , cdkestsSignTitle :: Text
   , cdkestsSignDescription :: Text
   , cdkestsSignText :: Text
   , cdkestsPersonalNumber :: Maybe Text
  }

instance ToJSON DKNemIDEIDServiceProviderSignParams where
  toJSON req = object
    [ "method" .= cdkestsMethod req
    , "signText" .= cdkestsSignText req
    , "signTitle" .= cdkestsSignTitle req
    , "signDescription" .= cdkestsSignDescription req
    , "personalNumber" .= cdkestsPersonalNumber req
    ]

beginSignTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginSignTransaction conf doc sl = do
  userVisibleData          <- textToBeSigned
  nemIDMethodRaw           <- getField "nemid_method"
  mPersonalNumberFromParam <- getField "personal_number"
  nemIDMethod              <- resolveDKNemIDSignMethod nemIDMethodRaw
  mPersonalNumber          <- case nemIDMethodRaw of
    Just "dk_nemid_pid" -> return Nothing
    _ ->
      case
          resolvePersonalNumberFromSL nemIDMethod
            <|> resolvePersonalNumberFromParam nemIDMethod mPersonalNumberFromParam
        of
          Just pn -> return $ Just pn
          Nothing -> do
            logAttention_
              $  "Tried to start sign NemID CPR/CVR transaction without"
              <> "mandatory personal number"
            internalError
  redirectUrl <- do
    ctx <- getContext
    return $ showt UnifiedRedirectUrl { redDomain          = ctx ^. #brandedDomain % #url
                                      , redProvider        = provider
                                      , redAuthKind        = EIDServiceAuthToSign
                                      , redDocumentID      = documentid doc
                                      , redSignatoryLinkID = signatorylinkid sl
                                      , redPostRedirectUrl = Nothing
                                      }
  let createReq = CreateEIDServiceTransactionRequest
        { cestProvider           = provider
        , cestMethod             = EIDServiceSignMethod
        , cestRedirectUrl        = redirectUrl
        , cestProviderParameters = Just . toJSON $ DKNemIDEIDServiceProviderSignParams
                                     { cdkestsMethod          = nemIDMethod
                                     , cdkestsSignTitle       = "Scrive document"
                                     , cdkestsSignDescription =
                                       "Scrive document to be signed"
                                     , cdkestsSignText        = userVisibleData
                                     , cdkestsPersonalNumber  = mPersonalNumber
                                     }
        }
  tid <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  startTransactionWithEIDServiceWithStatus conf provider tid >>= \case
    Right response -> do
      chargeForItemSingle CIDKNemIDSignatureStarted $ documentid doc
      return
        ( tid
        , object ["accessUrl" .= signUrl response]
        , EIDServiceTransactionStatusStarted
        )
    Left (HttpErrorCode 409) -> return
      ( tid
      , object ["grp_fault" .= ("already_in_progress" :: Text)]
      , EIDServiceTransactionStatusFailed
      )
    Left err -> do
      logInfo_ $ "Transaction retrieval error " <> showt err
      internalError

  where
    textToBeSigned :: TemplatesMonad m => m Text
    textToBeSigned = renderLocalTemplate doc "tbs" $ do
      F.value "document_title" $ documenttitle doc
      F.value "document_id" . show $ documentid doc
    resolveDKNemIDSignMethod = \case
      Nothing             -> return EIDServiceNemIDPersonalKeyCard
      Just "dk_nemid_cpr" -> return EIDServiceNemIDPersonalKeyCard
      Just "dk_nemid_pid" -> return EIDServiceNemIDPersonalKeyCard
      Just "dk_nemid_cvr_keyfile" -> return EIDServiceNemIDEmployeeKeyFile
      Just "dk_nemid_cvr_keycard" -> return EIDServiceNemIDEmployeeKeyCard
      Just s              -> do
        logAttention_ $ "No such DK NemID sign method" <> s
        internalError
    resolvePersonalNumberFromSL :: EIDServiceDKNemIDInternalProvider -> Maybe Text
    resolvePersonalNumberFromSL m = do
      pnField  <- getFieldByIdentity PersonalNumberFI (signatoryfields sl)
      pnFromSL <- fieldTextValue pnField
      normalisePersonalNumber m pnFromSL
    resolvePersonalNumberFromParam
      :: EIDServiceDKNemIDInternalProvider -> Maybe Text -> Maybe Text
    resolvePersonalNumberFromParam m mParam = mParam >>= normalisePersonalNumber m
    normalisePersonalNumber :: EIDServiceDKNemIDInternalProvider -> Text -> Maybe Text
    normalisePersonalNumber method rawPN =
      let validate = case method of
            EIDServiceNemIDPersonalKeyCard -> IV.asValidDanishSSN
            EIDServiceNemIDEmployeeKeyCard -> IV.asValidDanishCVR
            EIDServiceNemIDEmployeeKeyFile -> IV.asValidDanishCVR
      in  case validate rawPN of
            IV.Good pn -> Just pn
            _          -> Nothing

newtype StartDKNemIDSignTransactionResponse = StartDKNemIDSignTransactionResponse {
    signUrl :: Text
  }

instance FromJSON StartDKNemIDSignTransactionResponse where
  parseJSON outer =
    StartDKNemIDSignTransactionResponse
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: (toEIDServiceProviderName provider <> "Sign"))
          >>= withObject "object" (.: "signUrl")
          )

completeEIDServiceSignTransaction
  :: Kontrakcja m => EIDServiceConf -> SignatoryLink -> m Bool
completeEIDServiceSignTransaction conf sl = do
  sessionID <- getNonTempSessionID
  mTrans    <- getTransactionFromSession sessionID
  return . maybe False isSuccessfulTransaction $ mTrans
  where
    getTransactionFromSession sessionID = runMaybeT $ do
      Just estDB <- dbQuery $ GetEIDServiceTransactionGuardSessionID
        sessionID
        (signatorylinkid sl)
        EIDServiceAuthToSign
      Just trans <- getTransactionFromEIDService conf provider (estID estDB)
      return (trans :: EIDServiceTransactionResponse DKNemIDEIDServiceSignCompletionData)
    isSuccessfulTransaction trans =
      estRespStatus trans == EIDServiceTransactionStatusCompleteAndSuccess

data DKNemIDEIDServiceSignCompletionData = DKNemIDEIDServiceSignCompletionData
  {
    eidnidsSignedText :: Text
  , eidnidsB64SDO       :: Text
  , eidnidsSignatoryName :: Text
  , eidnidsSignatorySSN :: Maybe Text
  , eidnidsSignatoryPID :: Maybe Text
  , eidnidsSignatoryIP :: Maybe Text
  } deriving Show

instance FromJSON DKNemIDEIDServiceSignCompletionData where
  parseJSON outer = do
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: providerAuth)
      >>= withObject "object" (.: "completionData")
      >>= withObject
            "object"
            (\o -> do
              signedText    <- o .: "signedText"
              base64SDO     <- o .: "base64SDO"
              signatoryName <- o .: "signatoryName"
              signatorySSN  <- o .:? "signatorySSN"
              signatoryPID  <- o .:? "signatoryPID"
              signatoryIP   <- o .:? "signatoryIP"
              return DKNemIDEIDServiceSignCompletionData
                { eidnidsSignedText    = signedText
                , eidnidsB64SDO        = base64SDO
                , eidnidsSignatoryName = signatoryName
                , eidnidsSignatorySSN  = signatorySSN
                , eidnidsSignatoryPID  = signatoryPID
                , eidnidsSignatoryIP   = signatoryIP
                }
            )
    where
      providerName = toEIDServiceProviderName provider
      providerAuth = providerName <> "Sign"

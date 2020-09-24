module EID.EIDService.Provider.DKNemID (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  , dateOfBirthFromDKPersonalNumber
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
import User.Lang
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderDKNemID

data DKNemIDEIDServiceProviderParams = DKNemIDEIDServiceProviderParams {
    cdkestUILocale :: Lang
  , cdkestLimitedClientMode :: Bool
  , cdkestRequestCPR :: Bool
  , cdkestMethod :: Text
  }

instance ToJSON DKNemIDEIDServiceProviderParams where
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

resolveDKNemIDAuthMethod :: Kontrakcja m => Maybe Text -> m DKNemIDAuthMethod
resolveDKNemIDAuthMethod = \case
  Nothing             -> return DKNemIDAuthPID
  Just "dk_nemid_cpr" -> return DKNemIDAuthCPR
  Just "dk_nemid_pid" -> return DKNemIDAuthPID
  Just "dk_nemid_cvr" -> return DKNemIDAuthCVR
  Just s              -> do
    logAttention_ $ "No such DK NemID auth method" <> s
    internalError

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind doc sl = do
  ctx             <- getContext
  mkontraRedirect <- case authKind of
    EIDServiceAuthToView _ -> Just <$> guardJustM (getField "redirect")
    EIDServiceAuthToSign   -> return Nothing
  nemIDMethod <- getField "nemid_method" >>= resolveDKNemIDAuthMethod
  let redirectUrl = UnifiedRedirectUrl { redDomain          = ctx ^. #brandedDomain % #url
                                       , redProvider        = provider
                                       , redAuthKind        = authKind
                                       , redDocumentID      = documentid doc
                                       , redSignatoryLinkID = signatorylinkid sl
                                       , redPostRedirectUrl = mkontraRedirect
                                       }
  let requestCPR     = nemIDMethod == DKNemIDAuthCPR || nemIDMethod == DKNemIDAuthPID
      resolvedMethod = case nemIDMethod of
        DKNemIDAuthCPR -> "PersonalKeycard"
        DKNemIDAuthPID -> "PersonalKeycard"
        DKNemIDAuthCVR -> "EmployeeKeyfile"
      createReq = CreateEIDServiceTransactionRequest
        { cestProvider           = provider
        , cestMethod             = EIDServiceAuthMethod
        , cestRedirectUrl        = showt redirectUrl
        , cestProviderParameters = Just . toJSON $ DKNemIDEIDServiceProviderParams
                                     { cdkestUILocale          = documentlang doc
                                     , cdkestLimitedClientMode = True
                                     , cdkestRequestCPR        = requestCPR
                                     , cdkestMethod            = resolvedMethod
                                     }
        }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- sdkestAuthURL <$> startTransactionWithEIDService conf provider tid
  chargeForItemSingle CIDKNemIDAuthenticationStarted $ documentid doc
  return (tid, object ["accessUrl" .= turl], EIDServiceTransactionStatusStarted)

data DKNemIDEIDServiceCompletionData = DKNemIDEIDServiceCompletionData
  { eidnidInternalProvider :: EIDServiceDKNemIDInternalProvider
  , eidnidBirthDate :: Maybe T.Text
  , eidnidCertificate :: T.Text
  , eidnidDistinguishedName :: T.Text
  , eidnidPID :: Maybe T.Text
  , eidnidCPR :: Maybe T.Text
  , eidnidOrganisationData :: DKNemIDEIDServiceCompletionOrganisationData
  } deriving (Eq, Ord, Show)

data DKNemIDEIDServiceCompletionOrganisationData = DKNemIDEIDServiceCompletionOrganisationData
  {
    eidnidoOrganisationName :: Maybe T.Text
  , eidnidoOrganisationNumber :: Maybe T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON DKNemIDEIDServiceCompletionData where
  parseJSON outer = do
    providerParams <-
      withObject "object" (.: "providerParameters") outer
      >>= withObject "object" (.: "auth")
      >>= withObject "object" (.: providerName)
    method <- withObject "object" (.: "method") providerParams
    ip     <- resolveInternalProvider method
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
                  DKNemIDEIDServiceCompletionOrganisationData
                    <$> od
                    .:  "organisationName"
                    <*> od
                    .:  "organisationNumber"
                )
              let dob = dateOfBirthFromDKPersonalNumber <$> cpr
              (cer, dn) <- o .: "certificateData" >>= withObject
                "object"
                (\cd -> (,) <$> cd .: "certificate" <*> cd .: "distinguishedName")
              return DKNemIDEIDServiceCompletionData
                { eidnidInternalProvider  = ip
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
      resolveInternalProvider mt = case mt of
        Nothing                -> return EIDServiceNemIDPersonalKeyCard
        Just "PersonalKeycard" -> return EIDServiceNemIDPersonalKeyCard
        Just "EmployeeKeycard" -> return EIDServiceNemIDEmployeeKeyCard
        Just "EmployeeKeyfile" -> return EIDServiceNemIDEmployeeKeyFile
        Just t -> fail $ "Unknown internal provider returned from EID service: " <> t

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
  -> EIDServiceTransactionResponse DKNemIDEIDServiceCompletionData
  -> m EIDServiceTransactionStatus
finaliseTransaction doc sl estDB trans = validateCompletionData sl trans >>= \case
  Nothing -> do
    let status = EIDServiceTransactionStatusCompleteAndFailed
    mergeEIDServiceTransactionWithStatus status
    return status
  Just cd -> do
    let status   = EIDServiceTransactionStatusCompleteAndSuccess
        authKind = estAuthKind estDB
    mergeEIDServiceTransactionWithStatus status
    updateDBTransactionWithCompletionData doc sl authKind cd
    updateEvidenceLog doc sl authKind cd
    chargeForItemSingle CIDKNemIDAuthenticationFinished $ documentid doc
    return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }

validateCompletionData
  :: Kontrakcja m
  => SignatoryLink
  -> EIDServiceTransactionResponse DKNemIDEIDServiceCompletionData
  -> m (Maybe DKNemIDEIDServiceCompletionData)
validateCompletionData sl trans = case estRespCompletionData trans of
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
      => DKNemIDEIDServiceCompletionData
      -> m (Maybe DKNemIDEIDServiceCompletionData)
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
      => DKNemIDEIDServiceCompletionData
      -> m (Maybe DKNemIDEIDServiceCompletionData)
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

updateDBTransactionWithCompletionData
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceAuthenticationKind
  -> DKNemIDEIDServiceCompletionData
  -> m ()
updateDBTransactionWithCompletionData doc sl authKind cd = do
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

updateEvidenceLog
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceAuthenticationKind
  -> DKNemIDEIDServiceCompletionData
  -> m ()
updateEvidenceLog doc sl authKind cd = do
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

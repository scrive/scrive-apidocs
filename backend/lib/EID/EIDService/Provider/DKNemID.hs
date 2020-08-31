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
  }

instance ToJSON DKNemIDEIDServiceProviderParams where
  toJSON req = object
    ["limitedClientMode" .= cdkestLimitedClientMode req, "uiLocale" .= localeText]
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
  let redirectUrl = UnifiedRedirectUrl { redDomain          = ctx ^. #brandedDomain % #url
                                       , redProvider        = provider
                                       , redAuthKind        = authKind
                                       , redDocumentID      = documentid doc
                                       , redSignatoryLinkID = signatorylinkid sl
                                       , redPostRedirectUrl = mkontraRedirect
                                       }
  let createReq = CreateEIDServiceTransactionRequest
        { cestProvider           = provider
        , cestMethod             = EIDServiceAuthMethod
        , cestRedirectUrl        = showt redirectUrl
        , cestProviderParameters = Just . toJSON $ DKNemIDEIDServiceProviderParams
                                     { cdkestUILocale          = documentlang doc
                                     , cdkestLimitedClientMode = True
                                     }
        }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- sdkestAuthURL <$> startTransactionWithEIDService conf provider tid
  chargeForItemSingle CIDKNemIDAuthenticationStarted $ documentid doc
  return (tid, object ["accessUrl" .= turl], EIDServiceTransactionStatusStarted)

data DKNemIDEIDServiceCompletionData = DKNemIDEIDServiceCompletionData
  { eidnidInternalProvider :: EIDServiceDKNemIDInternalProvider
  , eidnidSSN :: T.Text
  , eidnidBirthDate :: T.Text
  , eidnidCertificate :: T.Text
  , eidnidDistinguishedName :: T.Text
  , eidnidPid :: T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON DKNemIDEIDServiceCompletionData where
  parseJSON outer = do
    ip <-
      withObject "object" (.: "providerParameters") outer
      >>= withObject "object" (.: "auth")
      >>= withObject "object" (.: providerName)
      >>= withObject "object" (.: "method")
      >>= resolveInternalProvider
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: providerAuth)
      >>= withObject "object" (.: "completionData")
      >>= withObject
            "object"
            (\o -> do
              pid <- o .: "pid"
              ssn <- o .: "ssn"
              let dob = dateOfBirthFromDKPersonalNumber ssn
              (cer, dn) <- o .: "certificateData" >>= withObject
                "object"
                (\cd -> (,) <$> cd .: "certificate" <*> cd .: "distinguishedName")
              return DKNemIDEIDServiceCompletionData { eidnidInternalProvider  = ip
                                                     , eidnidSSN               = ssn
                                                     , eidnidBirthDate         = dob
                                                     , eidnidCertificate       = cer
                                                     , eidnidDistinguishedName = dn
                                                     , eidnidPid               = pid
                                                     }
            )
    where
      providerName = toEIDServiceProviderName provider
      providerAuth = providerName <> "Auth"
      resolveInternalProvider mt = case mt of
        -- TODO: decide if we want to distinguish between personal keycard and employee
        -- keycard - this would break backward data compability, but maybe doesn't hurt us
        -- that much? - KJ
        Nothing                -> return EIDServiceNemIDKeyCard
        Just "PersonalKeycard" -> return EIDServiceNemIDKeyCard
        Just "EmployeeKeycard" -> return EIDServiceNemIDKeyCard
        Just "EmployeeKeyFile" -> return EIDServiceNemIDKeyFile
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
    let status = EIDServiceTransactionStatusCompleteAndSuccess
    mergeEIDServiceTransactionWithStatus status
    updateDBTransactionWithCompletionData doc sl cd
    updateEvidenceLog doc sl cd
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
    let ssnFromEIDService    = normalizeSSN $ eidnidSSN cd
        ssnFromSignatoryLink = normalizeSSN $ getPersonalNumber sl
    if ssnFromEIDService /= ssnFromSignatoryLink
      then do
        logAttention "SSN from NETS does not match SSN from SignatoryLink." $ object
          [ "ssn_sl" .= ssnFromSignatoryLink
          , "ssn_eidhub" .= ssnFromEIDService
          , "provider" .= ("dk_nemid" :: Text)
          ]
        flashMessageUserHasIdentifiedWithDifferentSSN >>= addFlashCookie . toCookieValue
        return Nothing
      else return $ Just cd
  where
    normalizeSSN :: Text -> Text
    normalizeSSN = T.filter (/= '-')
    flashMessageUserHasIdentifiedWithDifferentSSN :: TemplatesMonad m => m FlashMessage
    flashMessageUserHasIdentifiedWithDifferentSSN = toFlashMsg OperationFailed
      <$> renderTemplate_ "flashMessageUserHasIdentifiedWithDifferentSSN"

updateDBTransactionWithCompletionData
  :: Kontrakcja m => Document -> SignatoryLink -> DKNemIDEIDServiceCompletionData -> m ()
updateDBTransactionWithCompletionData doc sl cd = do
  let signatoryName = cnFromDN $ eidnidDistinguishedName cd
      birthDate     = eidnidBirthDate cd
      certificate   = decodeCertificate $ eidnidCertificate cd
      auth          = EIDServiceDKNemIDAuthentication
        { eidServiceNemIDInternalProvider = eidnidInternalProvider cd
        , eidServiceNemIDSignatoryName    = signatoryName
        , eidServiceNemIDDateOfBirth      = birthDate
        , eidServiceNemIDCertificate      = certificate
        }
  sessionID <- getNonTempSessionID
  dbUpdate $ MergeEIDServiceNemIDAuthentication (mkAuthKind doc)
                                                sessionID
                                                (signatorylinkid sl)
                                                auth

updateEvidenceLog
  :: Kontrakcja m => Document -> SignatoryLink -> DKNemIDEIDServiceCompletionData -> m ()
updateEvidenceLog doc sl cd = do
  ctx <- getContext
  let pid           = eidnidPid cd
      signatoryName = cnFromDN $ eidnidDistinguishedName cd
      birthDate     = eidnidBirthDate cd
      certificate   = decodeCertificate $ eidnidCertificate cd
      eventFields   = do
        F.value "signatory_name" signatoryName
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

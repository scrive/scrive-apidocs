module EID.EIDService.Provider.NOBankID (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
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
import InputValidation (asValidPhoneForNorwegianBankID, resultToMaybe)
import Kontra hiding (InternalError)
import Session.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderNOBankID

data NOBankIDEIDServiceProviderParams = NOBankIDEIDServiceProviderParams {
    cnoestPhoneNumber :: Maybe Text
  , cnoestPersonalNumber :: Text
  }

instance ToJSON NOBankIDEIDServiceProviderParams where
  toJSON req = object $ ["personalNumber" .= cnoestPersonalNumber req] <> phoneField
    where phoneField = maybe [] (\pn -> ["phoneNumber" .= pn]) $ cnoestPhoneNumber req

newtype StartNOBankIDEIDServiceTransactionResponse = StartNOBankIDEIDServiceTransactionResponse {
    snoestAuthURL :: Text
  }

instance FromJSON StartNOBankIDEIDServiceTransactionResponse where
  parseJSON outer =
    StartNOBankIDEIDServiceTransactionResponse
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
  -> m (EIDServiceTransactionID, Text, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind doc sl = do
  personalNumberField <-
    guardJust . getFieldByIdentity PersonalNumberFI . signatoryfields $ sl
  ssn <- guardJust . fieldTextValue $ personalNumberField
  let mNonEmptyNOPhone = case getMobile sl of
        "" -> Nothing
        p  -> resultToMaybe . asValidPhoneForNorwegianBankID $ p
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
        , cestProviderParameters = Just . toJSON $ NOBankIDEIDServiceProviderParams
                                     { cnoestPhoneNumber    = mNonEmptyNOPhone
                                     , cnoestPersonalNumber = ssn
                                     }
        }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- snoestAuthURL <$> startTransactionWithEIDService conf provider tid
  return (tid, turl, EIDServiceTransactionStatusStarted)

data NOBankIDEIDServiceCompletionData = NOBankIDEIDServiceCompletionData
  { eidnobidInternalProvider :: !EIDServiceNOBankIDInternalProvider
  , eidnobidBirthDate :: !(Maybe T.Text)
  , eidnobidCertificate :: !(Maybe T.Text)
  , eidnobidDistinguishedName :: !T.Text
  , eidnobidIssuerDistinguishedName :: !T.Text
  , eidnobidName :: !(Maybe Text)
  , eidnobidPhoneNumber :: !(Maybe T.Text)
  , eidnobidPid :: !T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON NOBankIDEIDServiceCompletionData where
  parseJSON outer =
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: providerAuth)
      >>= withObject "object" (.: "completionData")
      >>= withObject
            "object"
            (\o -> do
              pid              <- o .: "pid"
              usedMobileBankID <- o .: "usedMobileBankID"
              mphoneNumber     <- o .:? "phoneNumber"
              idn              <- o .: "issuerDN"
              (mdob, mname)    <- o .: "profileData" >>= withObject
                "object"
                (\cd -> (,) <$> cd .:? "birthdate" <*> cd .:? "name")
              (mcer, dn) <- o .: "certificateData" >>= withObject
                "object"
                (\cd -> (,) <$> cd .:? "certificate" <*> cd .: "distinguishedName")
              return NOBankIDEIDServiceCompletionData
                { eidnobidInternalProvider        = if usedMobileBankID
                                                      then EIDServiceNOBankIDMobile
                                                      else EIDServiceNOBankIDStandard
                , eidnobidName                    = mname
                , eidnobidBirthDate               = mdob
                , eidnobidDistinguishedName       = dn
                , eidnobidIssuerDistinguishedName = idn
                , eidnobidCertificate             = mcer
                , eidnobidPhoneNumber             = mphoneNumber
                , eidnobidPid                     = pid
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

finaliseTransaction
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceTransactionFromDB
  -> EIDServiceTransactionResponse NOBankIDEIDServiceCompletionData
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
    chargeForItemSingle CINOBankIDAuthentication $ documentid doc
    return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }

validateCompletionData
  :: Kontrakcja m
  => SignatoryLink
  -> EIDServiceTransactionResponse NOBankIDEIDServiceCompletionData
  -> m (Maybe NOBankIDEIDServiceCompletionData)
validateCompletionData sl trans = case estRespCompletionData trans of
  Nothing -> return Nothing
  Just cd -> do
    let mDobFromEIDService   = eidnobidBirthDate cd
        dobFromSignatoryLink = resolveDateOfBirthFromSSN $ getPersonalNumber sl
        mNameFromEIDService  = eidnobidName cd
    if isNothing mDobFromEIDService
      then do
        logAttention_ "Distinguished name not provided by EIDService."
        flashErrWithMessage =<< renderTemplate_ "flashMessageNoDOBProvidedByEIDService"
        return Nothing
      else if isNothing mNameFromEIDService
        then do
          logAttention_ "Name not provided by EIDService."
          flashErrWithMessage =<< renderTemplate_ "flashMessageNoNameProvidedByEIDService"
          return Nothing
        else if mDobFromEIDService /= Just dobFromSignatoryLink
          then do
            logAttention
                "Date of birth from EIDService does not match the one from SignatoryLink."
              $ object
                  [ "dob_sl" .= dobFromSignatoryLink
                  , "dob_eidhub" .= mDobFromEIDService
                  , "provider" .= ("no_nobankid" :: Text)
                  ]
            flashErrWithMessage
              =<< renderTemplate_ "flashMessageUserHasIdentifiedWithDifferentSSN"
            return Nothing
          else return $ Just cd
  where
    flashErrWithMessage :: Kontrakcja m => String -> m ()
    flashErrWithMessage = addFlashCookie . toCookieValue . toFlashMsg OperationFailed

updateDBTransactionWithCompletionData
  :: Kontrakcja m => Document -> SignatoryLink -> NOBankIDEIDServiceCompletionData -> m ()
updateDBTransactionWithCompletionData doc sl cd = do
  let mDobFromEIDService = eidnobidBirthDate cd
  signatoryName <- guardJust $ eidnobidName cd
  birthDate     <- guardJust mDobFromEIDService
  let certificate      = decodeCertificate <$> eidnobidCertificate cd
      phoneNumber      = eidnobidPhoneNumber cd
      internalProvider = eidnobidInternalProvider cd
      auth             = EIDServiceNOBankIDAuthentication
        { eidServiceNOBankIDInternalProvider = internalProvider
        , eidServiceNOBankIDSignatoryName    = signatoryName
        , eidServiceNOBankIDPhoneNumber      = phoneNumber
        , eidServiceNOBankIDDateOfBirth      = birthDate
        , eidServiceNOBankIDCertificate      = certificate
        }
  sessionID <- getNonTempSessionID
  dbUpdate $ MergeEIDServiceNOBankIDAuthentication (mkAuthKind doc)
                                                   sessionID
                                                   (signatorylinkid sl)
                                                   auth

updateEvidenceLog
  :: Kontrakcja m => Document -> SignatoryLink -> NOBankIDEIDServiceCompletionData -> m ()
updateEvidenceLog doc sl cd = do
  ctx           <- getContext
  -- TODO: These `guardJust`s should be handled at the parsing phase
  signatoryName <- guardJust $ eidnobidName cd
  birthDate     <- guardJust $ eidnobidBirthDate cd
  let certificate = decodeCertificate <$> eidnobidCertificate cd
      phoneNumber = eidnobidPhoneNumber cd
      pid         = eidnobidPid cd
      signatoryDN = eidnobidDistinguishedName cd
      issuerDN    = eidnobidIssuerDistinguishedName cd
      eventFields = do
        F.value "signatory_name" signatoryName
        F.value "signatory_mobile" phoneNumber
        F.value "provider_nobankid_eidservice" True
        F.value "signatory_dob" birthDate
        F.value "signatory_pid" pid
        F.value "signatory_distinguished_name" signatoryDN
        F.value "issuer_distinguished_name" issuerDN
        F.value "signature" $ B64.encode <$> certificate
  withDocument doc $ do
    actor <- signatoryActor ctx sl
    when (mkAuthKind doc == AuthenticationToView) $ do
      void
        . dbUpdate
        . InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence
                                                         eventFields
                                                         (Just sl)
                                                         Nothing
        $ actor

decodeCertificate :: Text -> BSC8.ByteString
decodeCertificate =
  either (unexpectedError "invalid base64 of NOBankID certificate") identity
    . B64.decode
    . T.encodeUtf8

resolveDateOfBirthFromSSN :: Text -> Text
resolveDateOfBirthFromSSN personalnumber = case T.chunksOf 2 (T.take 6 personalnumber) of
  [day, month, year] ->
    let yearWithoutCentury = read year
        sequenceNumber     = read . T.take 3 . T.drop 6 $ personalnumber
        century            = showt $ resolveCentury yearWithoutCentury sequenceNumber
    in  century <> year <> "-" <> month <> "-" <> day
  _ ->
    unexpectedError
      $  "This personal number cannot be formatted to date: "
      <> personalnumber
  where
    resolveCentury :: Int -> Int -> Int
    resolveCentury yearWithoutCentury sequenceNumber
      | yearWithoutCentury > 53 && sequenceNumber > 500 && sequenceNumber < 750 = 18
      | yearWithoutCentury > 40 && sequenceNumber > 899 = 19
      | yearWithoutCentury < 40 && sequenceNumber > 499 = 20
      | otherwise = 19

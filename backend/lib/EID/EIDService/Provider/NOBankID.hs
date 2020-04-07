module EID.EIDService.Provider.NOBankID (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  , completeEIDServiceSignTransaction
  , NOBankIDEIDServiceCompletionData(..)
) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types (Parser)
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
import Templates
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderNOBankID

data NOBankIDEIDServiceProviderAuthParams = NOBankIDEIDServiceProviderAuthParams {
    cnoestPhoneNumber :: Maybe Text
  , cnoestPersonalNumber :: Text
  }

instance ToJSON NOBankIDEIDServiceProviderAuthParams where
  toJSON req = object $ ["personalNumber" .= cnoestPersonalNumber req] <> phoneField
    where phoneField = maybe [] (\pn -> ["phoneNumber" .= pn]) $ cnoestPhoneNumber req

-- BC: sorry, I had no idea what cnoest stands for
data NOBankIDEIDServiceProviderSignParams = NOBankIDEIDServiceProviderSignParams {
    cnoest2PhoneNumber :: Maybe Text
  , cnoest2PersonalNumber :: Maybe Text
  , cnoest2SignText :: Text
  , cnoest2SignDescription :: Text
  , cnoest2UseMobile :: Bool
  }

instance ToJSON NOBankIDEIDServiceProviderSignParams where
  toJSON req =
    object
      $  personalNumberField
      <> phoneField
      <> signField
      <> descriptionField
      <> useMobileField
    where
      phoneField = maybe [] (\pn -> ["phoneNumber" .= pn]) $ cnoest2PhoneNumber req
      personalNumberField =
        maybe [] (\pn -> ["personalNumber" .= pn]) $ cnoest2PersonalNumber req
      signField        = ["signText" .= cnoest2SignText req]
      descriptionField = ["signDescription" .= cnoest2SignDescription req]
      useMobileField   = ["useMobileBankID" .= cnoest2UseMobile req]

withAlternativeObjects
  :: String -> (Object -> Parser a) -> (Object -> Parser a) -> Value -> Parser a
withAlternativeObjects name p1 p2 v = withObject name p1 v <|> withObject name p2 v

newtype StartNOBankIDEIDServiceTransactionResponse = StartNOBankIDEIDServiceTransactionResponse {
    snoestURL :: Text
  }

instance FromJSON StartNOBankIDEIDServiceTransactionResponse where
  parseJSON outer =
    StartNOBankIDEIDServiceTransactionResponse
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withAlternativeObjects "object" (.: providerAuth) (.: providerSign)
          >>= withAlternativeObjects "object" (.: "signUrl")    (.: "authUrl")
          )
    where
      providerName = toEIDServiceProviderName provider
      providerAuth = providerName <> "Auth"
      providerSign = providerName <> "Sign"

totalSignTextLength :: Int
totalSignTextLength = 118

properSignTextChars :: String
properSignTextChars =
  ['0' .. '9']
    ++ ['a' .. 'z']
    ++ ['A' .. 'Z']
    ++ ['(' .. '?']
    ++ "æøåÆØÅ #$%&@¡£¤¥§¿ÄÇÉÑÖÜßàäèéìñòöù"

createSignText :: Kontrakcja m => Document -> m Text
createSignText doc = do
  let
    title   = documenttitle doc
    sdid    = show $ documentid doc
    unquote = T.replace "\"" ""
    replaceBadChar c | c `elem` properSignTextChars = c
                     | otherwise                    = '?'
    replaceBadChars = T.map replaceBadChar
    render t = unquote <$> renderLocalTemplate
      doc
      "tbs"
      (do
        F.value "document_title" t
        F.value "document_id" sdid
      )
    boundedRender s incomplete = do
      let s' = if incomplete then s <> "..." else s
      res <- render s'
      if T.length res <= totalSignTextLength
        then return res
        else boundedRender (T.init s) True
  replaceBadChars <$> boundedRender title False

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind doc sl = do
  let mssn' =
        fieldTextValue =<< (getFieldByIdentity PersonalNumberFI . signatoryfields $ sl)
      mssn = case mssn' of
        Just "" -> Nothing
        _       -> mssn'
  let mNonEmptyNOPhone = case getMobile sl of
        "" -> Nothing
        p  -> resultToMaybe . asValidPhoneForNorwegianBankID $ p
  ctx             <- getContext
  signText        <- createSignText doc
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
      method = case authKind of
        EIDServiceAuthToView _ -> EIDServiceAuthMethod
        EIDServiceAuthToSign   -> EIDServiceSignMethod
  params <- do
    case authKind of
      EIDServiceAuthToView _ -> do
        ssn <- guardJust mssn
        return . toJSON $ NOBankIDEIDServiceProviderAuthParams
          { cnoestPhoneNumber    = mNonEmptyNOPhone
          , cnoestPersonalNumber = ssn
          }
      EIDServiceAuthToSign -> do
        useMobile' <- guardJustM $ getField "useMobile"
        let useMobile = useMobile' == "true"
        return . toJSON $ NOBankIDEIDServiceProviderSignParams
          { cnoest2PhoneNumber     = mNonEmptyNOPhone
          , cnoest2PersonalNumber  = mssn
          , cnoest2UseMobile       = useMobile
                   -- should these 2 text be translated? they never were for NETS
          , cnoest2SignText        = signText
          , cnoest2SignDescription = "Scrive document"
          }
  let createReq = CreateEIDServiceTransactionRequest
        { cestProvider           = provider
        , cestMethod             = method
        , cestRedirectUrl        = showt redirectUrl
        , cestProviderParameters = Just params
        }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- snoestURL <$> startTransactionWithEIDService conf provider tid
  chargeForItemSingle CINOBankIDAuthenticationStarted $ documentid doc
  return (tid, object ["accessUrl" .= turl], EIDServiceTransactionStatusStarted)

data NOBankIDEIDServiceCompletionData = NOBankIDEIDServiceCompletionData
  { eidnobidInternalProvider :: EIDServiceNOBankIDInternalProvider
  , eidnobidBirthDate :: Maybe T.Text
  , eidnobidCertificate :: Maybe T.Text
  , eidnobidDistinguishedName :: T.Text
  , eidnobidIssuerDistinguishedName :: T.Text
  , eidnobidName :: Maybe Text
  , eidnobidPhoneNumber :: Maybe T.Text
  , eidnobidPersonalNumber :: Maybe T.Text
  , eidnobidSignText :: Maybe T.Text
  , eidnobidPid :: T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON NOBankIDEIDServiceCompletionData where
  parseJSON outer = do
    msigntext <-
      withObject "object" (.: "providerParameters") outer >>= withObjectOrNothing
        "object"
        (.: "sign")
        (withObject "object" (.: providerName) >=> (.: "signText"))
    withObject "object" (.: "providerInfo") outer
      >>= withAlternativeObjects "object" (.: providerAuth) (.: providerSign)
      >>= withObject "object" (.: "completionData")
      >>= withObject
            "object"
            (\o -> do
              pid  <- o .: "pid"
              mssn <- o .:? "ssn"
              msdo <- o .:? "sdo"
              let usedMobileBankID = False -- TODO this should work somehow
              mphoneNumber  <- o .:? "phoneNumber"
              idn           <- o .: "issuerDN"
              (mdob, mname) <- o .: "profileData" >>= withObject
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
                , eidnobidCertificate             = mcer <|> msdo
                , eidnobidPhoneNumber             = mphoneNumber
                , eidnobidSignText                = msigntext
                , eidnobidPid                     = pid
                , eidnobidPersonalNumber          = mssn
                }
            )
    where
      providerName = toEIDServiceProviderName provider
      providerAuth = providerName <> "Auth"
      providerSign = providerName <> "Sign"
      withObjectOrNothing name p rest v =
        (withObject name p v >>= rest) <|> return Nothing

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

completeEIDServiceSignTransaction
  :: Kontrakcja m => EIDServiceConf -> SignatoryLink -> m Bool
completeEIDServiceSignTransaction conf sl = do
  sessionID <- getNonTempSessionID
  let authKind = EIDServiceAuthToSign
  mtrans <- runMaybeT $ do
    Just estDB <- dbQuery
      $ GetEIDServiceTransactionGuardSessionID sessionID (signatorylinkid sl) authKind
    Just trans <- getTransactionFromEIDService conf provider (estID estDB)
    return (trans :: EIDServiceTransactionResponse NOBankIDEIDServiceCompletionData)
  case mtrans of
    Nothing    -> return False
    Just trans -> return $ isSuccessFullTransaction trans
  where
    isSuccessFullTransaction trans =
      estRespStatus trans == EIDServiceTransactionStatusCompleteAndSuccess

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
    chargeForItemSingle CINOBankIDAuthenticationFinished $ documentid doc
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

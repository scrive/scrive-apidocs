module EID.EIDService.Provider.SEBankID (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
  , SEBankIDEIDServiceSignCompletionData(..)
  , SEBankIDEIDServiceSignTransactionData(..)
  , normalisePersonalNumber
  , completeEIDServiceSignTransaction
  , useEIDHubForSEBankIDSign
) where

import Control.Monad.Trans.Maybe
import Data.Aeson hiding (Result)
import Data.Aeson.Types hiding (Result)
import Data.ByteString (ByteString)
import Data.Either
import Data.Time (toGregorian, utctDay)
import Log
import Text.Read
import Text.StringTemplates.Templates
import qualified Data.ByteString.Base64 as B64
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
import InputValidation
import Kontra hiding (InternalError)
import Session.Cookies
import Session.Model
import Templates
import UserGroup.Types (SEBankIDSigningProviderOverride(..), UserGroupSettings)
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderSEBankID

useEIDHubForSEBankIDSign :: Context -> UserGroupSettings -> Bool
useEIDHubForSEBankIDSign ctx ugs
  | Just override <- ugs ^. #seBankIDSigningOverride = case override of
    ForceCGIForSEBankIDSigning    -> False
    ForceEIDHubForSEBankIDSigning -> True
  | otherwise = fromMaybe False $ ctx ^? #eidServiceConf % _Just % #eidUseForSESign

-- | Returns a 12 digit personal number (without separators).
normalisePersonalNumber :: UTCTime -> Text -> Result Text
normalisePersonalNumber now rawSSN = do
  clean <- asValidSwedishSSN rawSSN  -- returns 10 or 12 digits, no separator

  if T.length clean == 12
    then return clean
    else do
      -- We need to prefix 10 digit personal numbers with "19" or "20".
      let centenarian         = '+' `elem` T.unpack rawSSN
          -- -^ See https://en.wikipedia.org/wiki/Personal_identity_number_(Sweden)#Format.
          (currentYear, _, _) = toGregorian $ utctDay now

      yy      <- maybe Bad return (readMaybe . T.unpack $ T.take 2 clean)
      century <-
        case
          [ century
          | century <- [19, 20]
          , let age = currentYear - (century * 100 + yy)
          , age > 0
          , if centenarian then age >= 100 else age < 100
          ]
        of
          [century] -> return century
          _         -> Bad

      return $ showt century <> clean

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind = case authKind of
  EIDServiceAuthToView _ -> beginAuthTransaction conf authKind
  EIDServiceAuthToSign   -> beginSignTransaction conf


{- 1. Authentication -}

data SEBankIDEIDServiceProviderAuthParams = SEBankIDEIDServiceProviderAuthParams {
    cseeatPersonalNumber :: Maybe Text
  -- -^ If requireAutoStartToken == False, then a personal number needs to be
  -- provided.
  , cseeatRequireAutoStartToken :: Bool
  }

data SEBankIDEIDServiceProviderSignParams = SEBankIDEIDServiceProviderSignParams {
    cseestPersonalNumber :: Maybe Text
  -- -^ If requireAutoStartToken == False, then a personal number needs to be
  -- provided.
  , cseestRequireAutoStartToken :: Bool
  , cseestUserVisibleData :: Text  -- The text signed by the user.
  }

instance ToJSON SEBankIDEIDServiceProviderAuthParams where
  toJSON req = object
    [ "personalNumber" .= cseeatPersonalNumber req
    , "requireAutoStartToken" .= cseeatRequireAutoStartToken req
    ]

instance ToJSON SEBankIDEIDServiceProviderSignParams where
  toJSON req = object
    [ "personalNumber" .= cseestPersonalNumber req
    , "requireAutoStartToken" .= cseestRequireAutoStartToken req
    , "userVisibleData" .= cseestUserVisibleData req
    ]

beginAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginAuthTransaction conf authKind doc sl = do
  personalNumber <- do
    personalNumberField <-
      guardJust . getFieldByIdentity PersonalNumberFI . signatoryfields $ sl
    personalNumberRaw <- guardJust . fieldTextValue $ personalNumberField
    now               <- currentTime
    case normalisePersonalNumber now personalNumberRaw of
      Good pn -> return pn
      _ ->
        unexpectedError
          $  "Tried to authenticate with SEBankID with invalid personal number "
          <> showt personalNumberRaw

  redirectUrl <- do
    ctx               <- getContext
    sessionCookieInfo <- sessionCookieInfoFromSession <$> getCurrentSession
    postredirect      <- guardJustM (getField "redirect")
    return
      $  showt UnifiedRedirectUrl { redDomain          = ctx ^. #brandedDomain % #url
                                  , redProvider        = provider
                                  , redAuthKind        = authKind
                                  , redDocumentID      = documentid doc
                                  , redSignatoryLinkID = signatorylinkid sl
                                  , redPostRedirectUrl = Just postredirect
                                  }
      <> "&session="
      <> showt sessionCookieInfo

  let createReq = CreateEIDServiceTransactionRequest
        { cestProvider           = provider
        , cestMethod             = EIDServiceAuthMethod
        , cestRedirectUrl        = redirectUrl
        , cestProviderParameters = Just . toJSON $ SEBankIDEIDServiceProviderAuthParams
                                     { cseeatPersonalNumber        = Just personalNumber
                                     , cseeatRequireAutoStartToken = False
                                     }
        }
  trans <- createTransactionWithEIDService conf createReq  -- We are using the eID Hub frontend.
  chargeForItemSingle CISEBankIDAuthenticationStarted $ documentid doc
  return
    ( cestRespTransactionID trans
    , object ["accessUrl" .= cestRespAccessUrl trans]
    , EIDServiceTransactionStatusStarted
    )

-- same fields as in CGISEBankIDAuthentication
data SEBankIDEIDServiceAuthCompletionData = SEBankIDEIDServiceAuthCompletionData
  { eidsebidaSignatoryName           :: !Text
  , eidsebidaSignatoryPersonalNumber :: !Text
  , eidsebidaSignatoryIP             :: !Text
  , eidsebidaSignature               :: !ByteString
  , eidsebidaOcspResponse            :: !ByteString
  } deriving (Eq, Ord, Show)

-- Note for the future: this shouldn't be a FromJSON instance, since the
-- decoding isn't trivial (or even obvious).
instance FromJSON SEBankIDEIDServiceAuthCompletionData where
  parseJSON outer =
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: "seBankID")
      >>= withObject "object" (.: "completionData")
      >>= withObject
            "object"
            (\o -> do
              (name, personalNumber) <- o .: "user" >>= withObject
                "user"
                (\o' -> do
                  name           <- o' .: "name"
                  personalNumber <- o' .: "personalNumber"
                  return (name, personalNumber)
                )
              ipAddress    <- o .: "device" >>= withObject "device" (.: "ipAddress")
              signature    <- o .: "signature"
              ocspResponse <- o .: "ocspResponse"
              return SEBankIDEIDServiceAuthCompletionData
                { eidsebidaSignatoryName           = name
                , eidsebidaSignatoryPersonalNumber = personalNumber
                , eidsebidaSignatoryIP             = ipAddress
                , eidsebidaSignature               = decodeBase64 signature
                , eidsebidaOcspResponse            = decodeBase64 ocspResponse
                }
            )
    where
      decodeBase64 =
        fromRight (unexpectedError "invalid base64 in SEBankID auth completion data")
          . B64.decode
          . T.encodeUtf8

completeEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> Document
  -> SignatoryLink
  -> m (Maybe EIDServiceTransactionStatus)
completeEIDServiceAuthTransaction conf doc sl = do
  (session :: SessionCookieInfo) <- guardJustM $ readField "session"
  void . guardJustM $ unsafeSessionTakeover session
  -- To address AUTH-201 we include the session id as part of the redirect
  -- url, and restore the session from there. For posterity: on iOS the redirect
  -- may under certain circumstances land us in another browser, where we don't
  -- have an active session.
  let authKind = EIDServiceAuthToView $ mkAuthKind doc
  runMaybeT $ do
    Just estDB <- dbQuery $ GetEIDServiceTransactionGuardSessionID
      (cookieSessionID session)
      (signatorylinkid sl)
      authKind
    Just trans <- getTransactionFromEIDService conf provider (estID estDB)
    let eidServiceStatus = estRespStatus trans
        dbStatus         = estStatus estDB
    if eidServiceStatus == dbStatus
      then do
        return eidServiceStatus
      else do
        finaliseTransaction doc sl estDB trans

finaliseTransaction
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceTransactionFromDB
  -> EIDServiceTransactionResponse SEBankIDEIDServiceAuthCompletionData
  -> m EIDServiceTransactionStatus
finaliseTransaction doc sl estDB trans = validateAuthCompletionData sl trans >>= \case
  Nothing -> do
    let status = EIDServiceTransactionStatusCompleteAndFailed
    mergeEIDServiceTransactionWithStatus status
    return status
  Just cd -> do
    let status = EIDServiceTransactionStatusCompleteAndSuccess
    mergeEIDServiceTransactionWithStatus status
    updateDBTransactionWithCompletionData doc sl cd
    updateEvidenceLog doc sl cd
    chargeForItemSingle CISEBankIDAuthenticationFinished $ documentid doc
    return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }

-- | We make sure that we authenticate the correct person.
validateAuthCompletionData
  :: Kontrakcja m
  => SignatoryLink
  -> EIDServiceTransactionResponse SEBankIDEIDServiceAuthCompletionData
  -> m (Maybe SEBankIDEIDServiceAuthCompletionData)
validateAuthCompletionData sl trans = case estRespCompletionData trans of
  Nothing -> return Nothing
  Just cd -> do
    now <- currentTime

    let personalNumbersDontMatch = do
          addFlashCookie . toCookieValue $ toFlashMsg
            OperationFailed
            "flashMessageUserHasIdentifiedWithDifferentSSN"
          return Nothing

    case normalisePersonalNumber now $ getPersonalNumber sl of
      Empty -> return $ Just cd

      Good pnFromSigLink
        | eidsebidaSignatoryPersonalNumber cd == pnFromSigLink -> return $ Just cd
        | otherwise -> do
          logAttention
              "SEBankID personal number from EID Service does not match the\
              \ one in the signatory link."
            $ object
                [ "pn_from_siglink" .= pnFromSigLink
                , "pn_from_eid" .= eidsebidaSignatoryPersonalNumber cd
                , "provider" .= ("sebankid" :: Text)
                ]
          personalNumbersDontMatch

      Bad -> do
        logAttention
            "Invalid Swedish personal number in signatory link after\
          \ authenticating with SEBankID (using EID Hub)."
          $ object
              [ "personal_number_from_signatory_link" .= getPersonalNumber sl
              , "provider" .= ("sebankid" :: Text)
              ]
        personalNumbersDontMatch

updateDBTransactionWithCompletionData
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> SEBankIDEIDServiceAuthCompletionData
  -> m ()
updateDBTransactionWithCompletionData doc sl SEBankIDEIDServiceAuthCompletionData {..} =
  do
    let auth = EIDServiceSEBankIDAuthentication
          { eidServiceSEBankIDSignatoryName           = eidsebidaSignatoryName
          , eidServiceSEBankIDSignatoryPersonalNumber = eidsebidaSignatoryPersonalNumber
          , eidServiceSEBankIDSignatoryIP             = eidsebidaSignatoryIP
          , eidServiceSEBankIDSignature               = eidsebidaSignature
          , eidServiceSEBankIDOcspResponse            = eidsebidaOcspResponse
          }
    sessionID <- getNonTempSessionID
    dbUpdate $ MergeEIDServiceSEBankIDAuthentication (mkAuthKind doc)
                                                     sessionID
                                                     (signatorylinkid sl)
                                                     auth

updateEvidenceLog
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> SEBankIDEIDServiceAuthCompletionData
  -> m ()
updateEvidenceLog doc sl SEBankIDEIDServiceAuthCompletionData {..} = do
  ctx <- getContext
  let eventFields = do
        F.value "signatory_name" eidsebidaSignatoryName
        F.value "signatory_personal_number" eidsebidaSignatoryPersonalNumber
        F.value "hide_pn" $ signatorylinkhidepn sl
        F.value "provider_sebankid_eidservice" True
        F.value "signatory_ip" eidsebidaSignatoryIP
        F.value "signature" $ B64.encode eidsebidaSignature
        F.value "ocsp_response" $ B64.encode eidsebidaOcspResponse
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


{- 2. Signing -}

-- | Generate text to be signed that represents contents of the document.
textToBeSigned :: TemplatesMonad m => Document -> m Text
textToBeSigned doc@Document {..} = renderLocalTemplate doc "tbs" $ do
  F.value "document_title" documenttitle
  F.value "document_id" $ show documentid

beginSignTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginSignTransaction conf doc sl = do
  let mPersonalNumberField = getFieldByIdentity PersonalNumberFI . signatoryfields $ sl
  personalNumberRaw <- case mPersonalNumberField of
    Nothing ->
      unexpectedError "Tried to sign with SEBankID without personal number field"
    Just pnf -> case fieldTextValue pnf of
      Just s | not (T.null s) -> return s
             | otherwise      -> guardJustM $ getField "personal_number"
      Nothing -> unexpectedError "Tried to sign with SEBankID without personal number"
  personalNumber <- do
    now <- currentTime
    case normalisePersonalNumber now personalNumberRaw of
      Good pn -> return pn
      _ ->
        unexpectedError
          $  "Tried to sign with SEBankID with invalid personal number "
          <> showt personalNumberRaw

  redirectUrl <- do
    ctx <- getContext
    return $ showt UnifiedRedirectUrl { redDomain          = ctx ^. #brandedDomain % #url
                                      , redProvider        = provider
                                      , redAuthKind        = EIDServiceAuthToSign
                                      , redDocumentID      = documentid doc
                                      , redSignatoryLinkID = signatorylinkid sl
                                      , redPostRedirectUrl = Nothing
                                      }

  userVisibleData <- textToBeSigned doc

  let createReq = CreateEIDServiceTransactionRequest
        { cestProvider           = provider
        , cestMethod             = EIDServiceSignMethod
        , cestRedirectUrl        = redirectUrl
        , cestProviderParameters = Just . toJSON $ SEBankIDEIDServiceProviderSignParams
                                     { cseestPersonalNumber        = Just personalNumber
                                     , cseestRequireAutoStartToken = False
                                     , cseestUserVisibleData       = userVisibleData
                                     }
        }
  tid <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  startTransactionWithEIDServiceWithStatus conf provider tid >>= \case
    Right response -> do
      chargeForItemSingle CISEBankIDSignatureStarted $ documentid doc

      sessionCookieInfo <- sessionCookieInfoFromSession <$> getCurrentSession
      return
        ( tid
        , object
          [ "auto_start_token" .= respAutoStartToken response
          , "session_id" .= showt sessionCookieInfo
          ]
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

newtype StartSEBankIDSignTransactionResponse = StartSEBankIDSignTransactionResponse {
    respAutoStartToken :: Text
  }

instance FromJSON StartSEBankIDSignTransactionResponse where
  parseJSON outer =
    StartSEBankIDSignTransactionResponse
      <$> (   withObject "object" (.: "providerInfo") outer
          >>= withObject "object" (.: toEIDServiceProviderName provider)
          >>= withObject "object" (.: "autoStartToken")
          )

data SEBankIDEIDServiceSignTransactionData = SEBankIDEIDServiceSignTransactionData
  { eidsebidsProcessStatusInfo :: !(Maybe Text)
  , eidsebidsCompletionData :: !(Maybe SEBankIDEIDServiceSignCompletionData)
  } deriving (Eq, Ord, Show)

-- same fields as in CGISEBankIDSignature
data SEBankIDEIDServiceSignCompletionData = SEBankIDEIDServiceSignCompletionData
  { eidsebidsSignatoryName           :: !Text
  , eidsebidsSignatoryPersonalNumber :: !Text
  , eidsebidsSignatoryIP             :: !Text
  , eidsebidsSignedText              :: !Text
  , eidsebidsSignature               :: !ByteString
  , eidsebidsOcspResponse            :: !ByteString
  } deriving (Eq, Ord, Show)

instance FromJSON SEBankIDEIDServiceSignTransactionData where
  parseJSON = withObject "" $ \outer -> do
    completionData <- parseCompletionData outer
    statusInfo <- outer .: "providerInfo" >>= (.: "seBankID") >>= (.: "processStatusInfo")
    return $ SEBankIDEIDServiceSignTransactionData
      { eidsebidsCompletionData    = completionData
      , eidsebidsProcessStatusInfo = statusInfo
      }
    where
      parseCompletionData :: Object -> Parser (Maybe SEBankIDEIDServiceSignCompletionData)
      parseCompletionData outer = do
        providerInfo <- outer .: "providerInfo" >>= (.: "seBankID")
        providerInfo .: "completionData" >>= \case
          Just completionData -> do
            name            <- completionData .: "user" >>= (.: "name")
            personalNumber  <- completionData .: "user" >>= (.: "personalNumber")
            ipAddress       <- completionData .: "device" >>= (.: "ipAddress")
            signature       <- completionData .: "signature"
            ocspResponse    <- completionData .: "ocspResponse"
            userVisibleData <-
              outer
              .:  "providerParameters"
              >>= (.: "sign")
              >>= (.: "seBankID")
              >>= (.: "userVisibleData")
            return . Just $ SEBankIDEIDServiceSignCompletionData
              { eidsebidsSignatoryName           = name
              , eidsebidsSignatoryPersonalNumber = personalNumber
              , eidsebidsSignatoryIP             = ipAddress
              , eidsebidsSignedText              = userVisibleData
              , eidsebidsSignature               = decodeBase64 signature
              , eidsebidsOcspResponse            = decodeBase64 ocspResponse
              }
          Nothing -> return Nothing
      decodeBase64 =
        fromRight (unexpectedError "invalid base64 in SEBankID sign completion data")
          . B64.decode
          . T.encodeUtf8

completeEIDServiceSignTransaction
  :: Kontrakcja m => EIDServiceConf -> SignatoryLink -> m Bool
completeEIDServiceSignTransaction conf sl = do
  (session :: SessionCookieInfo) <- guardJustM $ readField "session"
  void . guardJustM $ unsafeSessionTakeover session
  -- -^ To address AUTH-201 we include the session id as part of the redirect
  -- url, and restore the session from there. For posterity: on iOS the redirect
  -- may under certain circumstances land us in another browser, where we don't
  -- have an active session.
  let authKind = EIDServiceAuthToSign
  mtrans <- runMaybeT $ do
    Just estDB <- dbQuery $ GetEIDServiceTransactionGuardSessionID
      (cookieSessionID session)
      (signatorylinkid sl)
      authKind
    Just trans <- getTransactionFromEIDService conf provider (estID estDB)
    return trans
  case mtrans of
    Nothing    -> return False
    Just trans -> do
      validatedCompletionData <- validateSignCompletionData sl trans
      return $ isSuccessFullTransaction trans && isJust validatedCompletionData
  where
    isSuccessFullTransaction trans =
      estRespStatus trans == EIDServiceTransactionStatusCompleteAndSuccess

-- | We make sure that we authenticate the correct person.
validateSignCompletionData
  :: Kontrakcja m
  => SignatoryLink
  -> EIDServiceTransactionResponse SEBankIDEIDServiceSignTransactionData
  -> m (Maybe SEBankIDEIDServiceSignCompletionData)
validateSignCompletionData sl trans = case estRespCompletionData trans of
  Nothing              -> return Nothing
  Just transactionData -> case eidsebidsCompletionData transactionData of
    Nothing             -> return Nothing
    Just completionData -> do
      now <- currentTime
      let personalNumbersDontMatch = do
            addFlashCookie . toCookieValue $ toFlashMsg
              OperationFailed
              "flashMessageUserHasIdentifiedWithDifferentSSN"
            return Nothing

      case normalisePersonalNumber now $ getPersonalNumber sl of
        Empty -> return $ Just completionData

        Good pnFromSigLink
          | eidsebidsSignatoryPersonalNumber completionData == pnFromSigLink -> return
          $ Just completionData
          | otherwise -> do
            logAttention
                "SEBankID personal number from EID Service does not match the\
                  \ one in the signatory link."
              $ object
                  [ "pn_from_siglink" .= pnFromSigLink
                  , "pn_from_eid" .= eidsebidsSignatoryPersonalNumber completionData
                  , "provider" .= ("sebankid" :: Text)
                  ]
            personalNumbersDontMatch

        Bad -> do
          logAttention
              "Invalid Swedish personal number in signatory link after\
              \ authenticating with SEBankID (using EID Hub)."
            $ object
                [ "personal_number_from_signatory_link" .= getPersonalNumber sl
                , "provider" .= ("sebankid" :: Text)
                ]
          personalNumbersDontMatch

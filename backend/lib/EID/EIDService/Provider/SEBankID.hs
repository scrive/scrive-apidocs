module EID.EIDService.Provider.SEBankID (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
) where

import Control.Monad.Trans.Maybe
import Data.Aeson hiding (Result)
import Data.ByteString (ByteString)
import Data.Either
import Data.Time (toGregorian, utctDay)
import Log
import Text.Read
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
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderSEBankID

data SEBankIDEIDServiceProviderParams = SEBankIDEIDServiceProviderParams {
    cseestPersonalNumber :: Maybe Text
  -- ^ If requireAutoStartToken == False, then a personal number needs to be
  -- provided.
  , cseestRequireAutoStartToken :: Bool
  }

instance ToJSON SEBankIDEIDServiceProviderParams where
  toJSON req = object
    [ "personalNumber" .= cseestPersonalNumber req
    , "requireAutoStartToken" .= cseestRequireAutoStartToken req
    ]

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> EIDServiceAuthenticationKind
  -> Document
  -> SignatoryLink
  -> m (EIDServiceTransactionID, Text, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind doc sl = do
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
  sess <- getCurrentSession  -- getNonTempSessionID? Someone explain to me what's going on here.
  let createReq = CreateEIDServiceTransactionRequest
        { cestProvider           = provider
        , cestMethod             = EIDServiceAuthMethod
        , cestRedirectUrl        = showt redirectUrl <> "&session=" <> showt
                                     (sessionCookieInfoFromSession sess)
        , cestProviderParameters = Just . toJSON $ SEBankIDEIDServiceProviderParams
                                     { cseestPersonalNumber        = Just personalNumber
                                     , cseestRequireAutoStartToken = False
                                     }
        }
  -- We are using the eID Hub frontend for the time being.
  trans <- createTransactionWithEIDService conf createReq
  return
    ( cestRespTransactionID trans
    , cestRespAccessUrl trans
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
        fromRight (unexpectedError "invalid base64 in SEBankID completion data")
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
  -- ^ To address AUTH-201 we include the session id as part of the redirect
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
    chargeForItemSingle CISEBankIDAuthenticationFinished $ documentid doc
    return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }

-- | We make sure that we authenticate the correct person.
validateCompletionData
  :: Kontrakcja m
  => SignatoryLink
  -> EIDServiceTransactionResponse SEBankIDEIDServiceAuthCompletionData
  -> m (Maybe SEBankIDEIDServiceAuthCompletionData)
validateCompletionData sl trans = case estRespCompletionData trans of
  Nothing -> return Nothing
  Just cd -> do
    now <- currentTime
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
  where
    personalNumbersDontMatch = do
      addFlashCookie . toCookieValue $ toFlashMsg
        OperationFailed
        "flashMessageUserHasIdentifiedWithDifferentSSN"
      return Nothing


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
        F.value "provider_sebankid_eidservice" True
        F.value "signatory_ip" eidsebidaSignatoryIP
        F.value "signature" eidsebidaSignature
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

-- | Returns a 12 digit personal number (without separators).
normalisePersonalNumber :: UTCTime -> Text -> Result Text
normalisePersonalNumber now rawSSN = do
  clean <- asValidSwedishSSN rawSSN  -- returns 10 or 12 digits, no separator

  if T.length clean == 12
    then return clean
    else do
      -- We need to prefix 10 digit personal numbers with "19" or "20".
      let centenarian         = '+' `elem` T.unpack rawSSN
          -- ^ See https://en.wikipedia.org/wiki/Personal_identity_number_(Sweden)#Format.
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

module EID.EIDService.Provider.FITupas (
    beginEIDServiceTransaction
  , FITupasEIDServiceCompletionData(..)
  , completeEIDServiceAuthTransaction
  , completeEIDServiceSignTransaction
 ) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.Text as T
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
import Happstack.Fields
import Kontra hiding (InternalError)
import Session.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderFITupas

newtype StartFITupasEIDServiceTransactionResponse = StartFITupasEIDServiceTransactionResponse {
    sfiestAuthURL :: Text
  }

instance FromJSON StartFITupasEIDServiceTransactionResponse where
  parseJSON outer =
    StartFITupasEIDServiceTransactionResponse
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
        , cestProviderParameters = Nothing
        }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- sfiestAuthURL <$> startTransactionWithEIDService conf provider tid
  case authKind of
    EIDServiceAuthToView _ ->
      chargeForItemSingle CIFITupasAuthenticationStarted $ documentid doc
    EIDServiceAuthToSign ->
      chargeForItemSingle CIFITupasSignatureStarted $ documentid doc
  return (tid, object ["accessUrl" .= turl], EIDServiceTransactionStatusStarted)

data FITupasEIDServiceCompletionData = FITupasEIDServiceCompletionData
  { eidtupasName :: Text
  , eidtupasBirthDate :: Maybe Text
  , eidtupasDistinguishedName :: Text  -- may contain the personal number
  , eidtupasBank :: Maybe Text  -- absent when using Mobile ID
  , eidtupasPid :: Maybe Text
  -- ^ 'A fixed identifier for the user set in the E-Ident / FTN service.' (from
  -- the Nets documentation)
  , eidtupasSSN :: Maybe Text  -- seems to be absent for 'legal persons'
  } deriving (Eq, Ord, Show)

instance FromJSON FITupasEIDServiceCompletionData where
  parseJSON outer = do
    let providerAuth =
          toEIDServiceProviderName EIDServiceTransactionProviderFITupas <> "Auth"
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: providerAuth)
      >>= withObject "object" (.: "completionData")
      >>= withObject
            "object"
            (\o -> do
              mbank       <- o .:? "bank"
              mpid        <- o .:? "pid"
              mssn        <- o .:? "ssn"
              (dob, name) <- o .: "profileData" >>= withObject
                "object"
                (\cd -> (,) <$> cd .: "birthdate" <*> cd .: "name")
              dn <- o .: "certificateData" >>= withObject "object"
                                                          (.: "distinguishedName")
              return FITupasEIDServiceCompletionData { eidtupasName              = name
                                                     , eidtupasBirthDate         = dob
                                                     , eidtupasDistinguishedName = dn
                                                     , eidtupasBank              = mbank
                                                     , eidtupasPid               = mpid
                                                     , eidtupasSSN               = mssn
                                                     }
            )

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
  -> EIDServiceTransactionResponse FITupasEIDServiceCompletionData
  -> m EIDServiceTransactionStatus
finaliseTransaction doc sl estDB trans = case validateCompletionData sl trans of
  Nothing -> do
    let status = EIDServiceTransactionStatusCompleteAndFailed
    mergeEIDServiceTransactionWithStatus status
    return status
  Just cd -> do
    let status = EIDServiceTransactionStatusCompleteAndSuccess
    mergeEIDServiceTransactionWithStatus status
    updateDBTransactionWithCompletionData doc sl cd
    updateEvidenceLog doc sl cd
    chargeForItemSingle CIFITupasAuthenticationFinished $ documentid doc
    return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }

updateDBTransactionWithCompletionData
  :: Kontrakcja m => Document -> SignatoryLink -> FITupasEIDServiceCompletionData -> m ()
updateDBTransactionWithCompletionData doc sl FITupasEIDServiceCompletionData {..} = do
  let auth = EIDServiceFITupasAuthentication
        { eidServiceFITupasSignatoryName  = eidtupasName
        , eidServiceFITupasPersonalNumber = eidtupasSSN
        , eidServiceFITupasDateOfBirth    = eidtupasBirthDate
        }
  sessionID <- getNonTempSessionID
  dbUpdate $ MergeEIDServiceFITupasAuthentication (mkAuthKind doc)
                                                  sessionID
                                                  (signatorylinkid sl)
                                                  auth

updateEvidenceLog
  :: Kontrakcja m => Document -> SignatoryLink -> FITupasEIDServiceCompletionData -> m ()
updateEvidenceLog doc sl FITupasEIDServiceCompletionData {..} = do
  ctx <- getContext
  let eventFields = do
        F.value "provider_fitupas" True
        F.value "signatory_name" eidtupasName
        F.value "signatory_dob" eidtupasBirthDate
        F.value "signatory_personal_number" eidtupasSSN
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

completeEIDServiceSignTransaction
  :: Kontrakcja m => EIDServiceConf -> SignatoryLink -> m Bool
completeEIDServiceSignTransaction conf sl = do
  sessionID <- getNonTempSessionID
  let authKind = EIDServiceAuthToSign
  mtrans <- runMaybeT $ do
    Just estDB <- dbQuery
      $ GetEIDServiceTransactionGuardSessionID sessionID (signatorylinkid sl) authKind
    Just trans <- getTransactionFromEIDService conf provider (estID estDB)
    return (trans :: EIDServiceTransactionResponse FITupasEIDServiceCompletionData)
  case mtrans of
    Nothing -> return False
    Just trans ->
      return $ isSuccessFullTransaction trans && isJust (validateCompletionData sl trans)
  where
    isSuccessFullTransaction trans =
      estRespStatus trans == EIDServiceTransactionStatusCompleteAndSuccess

validateCompletionData
  :: SignatoryLink
  -> EIDServiceTransactionResponse FITupasEIDServiceCompletionData
  -> Maybe FITupasEIDServiceCompletionData
validateCompletionData sl trans = case estRespCompletionData trans of
  Nothing -> Nothing
  Just cd ->
    if T.null (getPersonalNumber sl)
         || isNothing (eidtupasSSN cd)
         || (getPersonalNumber sl == fromMaybe "" (eidtupasSSN cd))
      then Just cd
      else Nothing

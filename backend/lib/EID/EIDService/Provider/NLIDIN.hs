module EID.EIDService.Provider.NLIDIN (
    beginEIDServiceTransaction
  , NLIDINEIDServiceCompletionData(..)
  , completeEIDServiceAuthTransaction
  , completeEIDServiceSignTransaction
  ) where

import Control.Monad.Trans.Maybe
import Data.Aeson
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
import Util.MonadUtils

provider :: EIDServiceTransactionProvider
provider = EIDServiceTransactionProviderNLIDIN

newtype NLIDINEIDServiceProviderParams = NLIDINEIDServiceProviderParams {
    cnlestRequestBirthdate :: Bool
  }

instance ToJSON NLIDINEIDServiceProviderParams where
  toJSON _ = Null
  toEncoding = pairs . ("requestBirthdate" .=) . cnlestRequestBirthdate

newtype StartNLIDINEIDServiceTransactionResponse = StartNLIDINEIDServiceTransactionResponse {
    snlestAuthURL :: Text
  }

instance FromJSON StartNLIDINEIDServiceTransactionResponse where
  parseJSON outer =
    StartNLIDINEIDServiceTransactionResponse
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
  ctx             <- getContext
  mkontraRedirect <- case authKind of
    EIDServiceAuthToView _ -> Just <$> guardJustM (getField "redirect")
    EIDServiceAuthToSign   -> return Nothing
  let createReq = CreateEIDServiceTransactionRequest
        { cestDomain             = ctx ^. #brandedDomain % #url
        , cestEIDServiceProvider = provider
        , cestDocumentID         = documentid doc
        , cestSignatoryLinkID    = signatorylinkid sl
        , cestAuthKind           = authKind
        , cestKontraRedirectUrl  = mkontraRedirect
        , cestmProviderParams    = Just $ NLIDINEIDServiceProviderParams
                                     { cnlestRequestBirthdate = True
                                     }
        }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- snlestAuthURL <$> startTransactionWithEIDService conf provider tid
  return (tid, turl, EIDServiceTransactionStatusStarted)

data NLIDINEIDServiceCompletionData = NLIDINEIDServiceCompletionData
  { eiditdName :: Text
  , eiditdBirthDate :: Text
  , eiditdCustomerID :: Text
  } deriving (Eq, Ord, Show)

-- BMW do not want us to ever fail if we don't receive a particular item of data from iDIN
-- so we just save an empty string for any data that is missing.
instance FromJSON NLIDINEIDServiceCompletionData where
  parseJSON outer =
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: providerAuth)
      >>= withObject "object" (.: "completionData")
      >>= withObject
            "object"
            (\o -> do
              initials       <- o .:? "initials" .!= ""
              mTussenvoegsel <- o .:? "legalLastNamePrefix"
              surname        <- o .:? "legalLastName" .!= ""
              dob            <- o .:? "birthDate" .!= ""
              customerID     <- o .:? "customerId" .!= ""
              let name = case mTussenvoegsel of
                    Just tussenvoegsel ->
                      initials <> " " <> tussenvoegsel <> " " <> surname
                    Nothing -> initials <> " " <> surname
              return NLIDINEIDServiceCompletionData { eiditdName       = name
                                                    , eiditdBirthDate  = dob
                                                    , eiditdCustomerID = customerID
                                                    }
            )
    where providerAuth = toEIDServiceProviderName provider <> "Auth"

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
    return (trans :: EIDServiceTransactionResponse NLIDINEIDServiceCompletionData)
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
  -> EIDServiceTransactionResponse NLIDINEIDServiceCompletionData
  -> m EIDServiceTransactionStatus
finaliseTransaction doc sl estDB trans = case estRespCompletionData trans of
  Nothing -> do
    let status = EIDServiceTransactionStatusCompleteAndFailed
    mergeEIDServiceTransactionWithStatus status
    return status
  Just cd -> do
    let status = EIDServiceTransactionStatusCompleteAndSuccess
    mergeEIDServiceTransactionWithStatus status
    updateDBTransactionWithCompletionData doc sl cd
    updateEvidenceLog doc sl cd
    chargeForItemSingle CIIDINAuthentication $ documentid doc
    return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }

updateDBTransactionWithCompletionData
  :: Kontrakcja m => Document -> SignatoryLink -> NLIDINEIDServiceCompletionData -> m ()
updateDBTransactionWithCompletionData doc sl NLIDINEIDServiceCompletionData {..} = do
  let auth = EIDServiceNLIDINAuthentication
        { eidServiceIDINName          = eiditdName
        , eidServiceIDINVerifiedPhone = Nothing
        , eidServiceIDINBirthDate     = Just eiditdBirthDate
        , eidServiceIDINCustomerID    = Just eiditdCustomerID
        }
  sessionID <- getNonTempSessionID
  dbUpdate $ MergeEIDServiceIDINAuthentication (mkAuthKind doc)
                                               sessionID
                                               (signatorylinkid sl)
                                               auth

updateEvidenceLog
  :: Kontrakcja m => Document -> SignatoryLink -> NLIDINEIDServiceCompletionData -> m ()
updateEvidenceLog doc sl NLIDINEIDServiceCompletionData {..} = do
  ctx <- getContext
  let eventFields = do
        F.value "signatory_name" eiditdName
        F.value "signatory_dob" eiditdBirthDate
        F.value "provider_customer_id" eiditdCustomerID
        F.value "provider_idin" True
  withDocument doc
    .   void
    $   dbUpdate
    .   InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence
                                                       eventFields
                                                       (Just sl)
                                                       Nothing
    =<< signatoryActor ctx sl

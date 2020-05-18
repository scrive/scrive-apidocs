module EID.EIDService.Provider.Verimi (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
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
provider = EIDServiceTransactionProviderVerimi

newtype StartVerimiEIDServiceTransactionResponse = StartVerimiEIDServiceTransactionResponse {
    svestAuthURL :: Text
  }

instance FromJSON StartVerimiEIDServiceTransactionResponse where
  parseJSON outer =
    StartVerimiEIDServiceTransactionResponse
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
        , cestmProviderParams    = Nothing :: Maybe ()
        }
  tid  <- cestRespTransactionID <$> createTransactionWithEIDService conf createReq
  turl <- svestAuthURL <$> startTransactionWithEIDService conf provider tid
  return (tid, turl, EIDServiceTransactionStatusStarted)

data VerimiEIDServiceCompletionData = VerimiEIDServiceCompletionData
  { eidvtdName :: T.Text
  , eidvtdVerifiedEmail :: T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON VerimiEIDServiceCompletionData where
  parseJSON outer =
    withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: providerAuth)
      >>= withObject "object" (.: "completionData")
      >>= withObject
            "object"
            (\o -> do
              emailVerified <- o .: "emailVerified"
              unless emailVerified $ fail "Email is not verified"
              VerimiEIDServiceCompletionData <$> (o .: "name") <*> (o .: "email")
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

finaliseTransaction
  :: Kontrakcja m
  => Document
  -> SignatoryLink
  -> EIDServiceTransactionFromDB
  -> EIDServiceTransactionResponse VerimiEIDServiceCompletionData
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
    chargeForItemSingle CIVerimiAuthentication $ documentid doc
    return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }

validateCompletionData
  :: SignatoryLink
  -> EIDServiceTransactionResponse VerimiEIDServiceCompletionData
  -> Maybe VerimiEIDServiceCompletionData
validateCompletionData sl trans = case estRespCompletionData trans of
  Nothing -> Nothing
  Just cd -> if eidvtdVerifiedEmail cd == getEmail sl then Just cd else Nothing

updateDBTransactionWithCompletionData
  :: Kontrakcja m => Document -> SignatoryLink -> VerimiEIDServiceCompletionData -> m ()
updateDBTransactionWithCompletionData doc sl cd = do
  let auth = EIDServiceVerimiAuthentication
        { eidServiceVerimiName          = eidvtdName cd
        , eidServiceVerimiVerifiedEmail = Just $ eidvtdVerifiedEmail cd
        , eidServiceVerimiVerifiedPhone = Nothing
        }
  sessionID <- getNonTempSessionID
  dbUpdate $ MergeEIDServiceVerimiAuthentication (mkAuthKind doc)
                                                 sessionID
                                                 (signatorylinkid sl)
                                                 auth

updateEvidenceLog
  :: Kontrakcja m => Document -> SignatoryLink -> VerimiEIDServiceCompletionData -> m ()
updateEvidenceLog doc sl cd = do
  ctx <- getContext
  let eventFields = do
        F.value "signatory_name" $ eidvtdName cd
        F.value "provider_verimi" True
        F.value "provider_email" $ eidvtdVerifiedEmail cd
  withDocument doc
    .   void
    $   dbUpdate
    .   InsertEvidenceEventWithAffectedSignatoryAndMsg AuthenticatedToViewEvidence
                                                       eventFields
                                                       (Just sl)
                                                       Nothing
    =<< signatoryActor ctx sl

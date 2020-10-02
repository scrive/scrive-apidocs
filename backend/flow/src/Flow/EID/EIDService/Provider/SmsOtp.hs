module Flow.EID.EIDService.Provider.SmsOtp (
    beginEIDServiceTransaction
  , completeEIDServiceAuthTransaction
 ) where

import Control.Monad.Trans.Maybe
import Data.Aeson
import Log

import Chargeable
import DB
import Doc.DocStateData
import Doc.Model.Query
import EID.Authentication.Model
import EID.EIDService.Communication
import EID.EIDService.Conf
import EID.EIDService.Types hiding
  ( EIDServiceTransactionFromDB(..), UnifiedRedirectUrl(..)
  )
import Flow.ActionConsumers
import Flow.EID.Authentication
import Flow.EID.EIDService.Model
import Flow.EID.EIDService.Provider.Common
import Flow.EID.EIDService.Types
import Flow.Id
import Flow.Names
import Flow.Utils
import Happstack.Fields
import Kontra hiding (InternalError)
import Log.Identifier
import Session.Model
import Util.HasSomeUserInfo
import Util.MonadUtils
import qualified Flow.CallbackPayload as CB
import qualified Flow.CallbackPayload as Callback

eidProvider :: EIDServiceTransactionProvider
eidProvider = EIDServiceTransactionProviderSmsOtp

data SmsOtpParams = SmsOtpParams {
    phoneNumber :: Text
  , numberOfAttempts :: Int
  , message :: Text
  , originator :: Text
  }

instance ToJSON SmsOtpParams where
  toJSON SmsOtpParams {..} = object
    [ "msisdn" .= phoneNumber
    , "maxNumberOfAttempts" .= numberOfAttempts
    , "messageText" .= message
    , "originator" .= originator
    ]

-- TODO make this configurable
smsOtpParams :: Text -> SmsOtpParams
smsOtpParams phoneNumber = SmsOtpParams { .. }
  where
    numberOfAttempts = 5
    message          = "Your authentication code: "
    originator       = "Scrive"

beginEIDServiceTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> AuthenticationKind
  -> InstanceId
  -> UserName
  -> m (EIDServiceTransactionID, Value, EIDServiceTransactionStatus)
beginEIDServiceTransaction conf authKind instanceId userName = do
  ctx            <- getContext
  kontraRedirect <- guardJustM (getField "redirect")
  let redirectUrl = UnifiedRedirectUrl { redDomain          = ctx ^. #brandedDomain % #url
                                       , redProvider        = eidProvider
                                       , redAuthKind = EIDServiceAuthToView authKind
                                       , redInstanceId      = instanceId
                                       , redUserName        = userName
                                       , redPostRedirectUrl = Just kontraRedirect
                                       }

  (did, slid) <- findFirstSignatoryLink instanceId userName
  sl          <- dbQuery $ GetSignatoryLinkByID did slid
  let createReq = CreateEIDServiceTransactionRequest
        { cestProvider           = eidProvider
        , cestMethod             = EIDServiceAuthMethod
        , cestRedirectUrl        = showt redirectUrl
        , cestProviderParameters = Just . toJSON . smsOtpParams $ getMobile sl
        }
  trans <- createTransactionWithEIDService conf createReq
  chargeForItemSingle CISmsOtpAuthenticationStarted did
  let tid  = cestRespTransactionID trans
      turl = cestRespAccessUrl trans
  return (tid, object ["accessUrl" .= turl], EIDServiceTransactionStatusNew)

completeEIDServiceAuthTransaction
  :: Kontrakcja m
  => EIDServiceConf
  -> InstanceId
  -> UserName
  -> m (Maybe EIDServiceTransactionStatus)
completeEIDServiceAuthTransaction conf instanceId userName = runMaybeT $ do
  Just (authKind, estDB, trans) <- getTransactionForUser conf
                                                         eidProvider
                                                         instanceId
                                                         userName
  if estRespStatus trans == estStatus estDB
    then return $ estRespStatus trans
    else finaliseTransaction instanceId userName authKind estDB trans

newtype SmsOtpCompletionData = SmsOtpCompletionData
  { phoneNumber :: Text
  } deriving (Show)

instance FromJSON SmsOtpCompletionData where
  parseJSON outer =
    do
        withObject "object" (.: "providerInfo") outer
      >>= withObject "object" (.: "smsOtpAuth")
      >>= \o -> do
            phoneNumber <- o .: "msisdn"
            pure $ SmsOtpCompletionData phoneNumber

finaliseTransaction
  :: Kontrakcja m
  => InstanceId
  -> UserName
  -> AuthenticationKind
  -> EIDServiceTransactionFromDB
  -> EIDServiceTransactionResponse SmsOtpCompletionData
  -> m EIDServiceTransactionStatus
finaliseTransaction instanceId userName authKind estDB trans =
  case validateCompletionData trans of
    Nothing -> do
      let status = EIDServiceTransactionStatusCompleteAndFailed
      mergeEIDServiceTransactionWithStatus status
      insertAuthenticationFailure instanceId userName authKind
      case estRespCompletionData trans of
        Just _cd -> sendCallback CB.Failure
        Nothing  -> do
          logInfo "Finalizing transaction which is not finished"
            $ object [logPair_ instanceId]
          return ()
      return status
    Just cd -> do
      let status = EIDServiceTransactionStatusCompleteAndSuccess
      mergeEIDServiceTransactionWithStatus status
      updateDBTransactionWithCompletionData instanceId userName authKind cd
      sendCallback CB.Success
      return status
  where
    mergeEIDServiceTransactionWithStatus newstatus =
      dbUpdate . MergeEIDServiceTransaction $ estDB { estStatus = newstatus }
    sendCallback result =
      sendEventCallback instanceId
        . CB.AuthenticationAttempted
        $ CB.AuthenticationAttemptedEvent { userName = userName
                                          , result   = result
                                          , provider = Callback.SmsOtp
                                          }

validateCompletionData
  :: EIDServiceTransactionResponse SmsOtpCompletionData -> Maybe SmsOtpCompletionData
validateCompletionData trans = case (estRespStatus trans, estRespCompletionData trans) of
  (EIDServiceTransactionStatusCompleteAndSuccess, Just cd) -> Just cd
  (_, _      ) -> Nothing

updateDBTransactionWithCompletionData
  :: Kontrakcja m
  => InstanceId
  -> UserName
  -> AuthenticationKind
  -> SmsOtpCompletionData
  -> m ()
updateDBTransactionWithCompletionData instanceId userName authKind SmsOtpCompletionData {..}
  = do
    sessionID <- getNonTempSessionID
    updateFlowEidAuthentication instanceId userName authKind sessionID
      $ EIDServiceSmsOtpAuthentication_ phoneNumber

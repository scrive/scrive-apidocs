{-# LANGUAGE DeriveAnyClass #-}
module Flow.ActionConsumers
  ( ConsumableAction(..)
  , ActionsError(..)
  , NotifyUser(..)
  , CloseAllAction(..)
  , NotifyAction(..)
  , RejectAction(..)
  , toConsumableAction
  , toConsumableRejectAction
  , notifyAction
  , consumeFlowAction
  , consumeNotifyAction
  , consumeRejectionAction
  , sendEventCallback
  ) where

import Control.Monad.Catch
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG
import Data.Aeson
import Data.Aeson.Casing
import Database.PostgreSQL.PQTypes
import GHC.Generics
import Log
import Optics (At(at))
import Text.StringTemplates.Templates (TemplatesMonad)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.Model
import Branding.Adler32
import Callback.Model
import Callback.Types
import DB hiding (JSON(..))
import Doc.Action
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model.Query (GetDocumentByDocumentID(..))
import Doc.Model.Update
import Doc.Signing.Model ()
import Doc.Types.Document
import Doc.Types.DocumentStatus
import Doc.Types.SignatoryLink
import EventStream.Class
import File.Storage
import Flow.CallbackPayload
import Flow.Core.Type.Callback
import Flow.Core.Type.Url
import Flow.Id
import Flow.Machinize (EventInfo(..), getRejectMessageFromEvent)
import Flow.Message.Internal
import Flow.Model
import Flow.Model.InstanceSession
import Flow.Model.Types
import Flow.Model.Types.FlowUserId
import Flow.Names
import Flow.Server.Utils
import GuardTime (GuardTimeConfMonad)
import MailContext
import Mails.SendMail
import SMS.SMS
import SMS.Types
import Theme.Model
import User.Lang
import User.UserID
import UserGroup.Model
import UserGroup.Types
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Color
import qualified Flow.CallbackPayload as Callback
import qualified Flow.HighTongue as HighTongue
import qualified Flow.Machinize as Machinize

type NotifyPhoneNumber = Text
type NotifyEmail = Text

data ConsumableAction
  = Notify NotifyAction
  | CloseAll CloseAllAction
  | Reject RejectAction
  | Fail FailAction
  deriving (Show, Generic)

data NotifyAction = NotifyAction
  { users :: [NotifyUser]
  , emailMessage :: Maybe Message
  , smsMessage :: Maybe Message
  }
  deriving (Show, Generic, ToJSON)

data CloseAllAction = CloseAllAction
  { instanceId :: InstanceId
  , documentIds :: [DocumentID]
  }
  deriving (Show, Generic, ToJSON)

-- A rejection action can be performed by a flow participant.
-- When triggered, all pending documents that involve the signatory
-- is cancelled, and the flow fails.
data RejectAction = RejectAction
  { rejector :: UserName
  , instanceId :: InstanceId

  -- documents to be cancelled
  , documentIds :: [DocumentID]

  -- The name of the document that triggers the rejection action,
  -- if it is triggered from rejecting a document instead of a flow instance.
  , documentName :: Maybe DocumentName

  , rejectMessage :: Maybe Text
  }
  deriving (Show, Generic, ToJSON)

newtype FailAction = FailAction
  { instanceId :: InstanceId
  }
  deriving (Show, Generic)
  deriving newtype ToJSON

data ActionsError
    = UserNotFound HighTongue.UserName
    | AssociatedSignatoryNotFound FlowUserId
    | MessageNotFound HighTongue.MessageName
    | AccessLinkNotFound HighTongue.UserName
    | NoInstance InstanceId
  deriving (Show, Exception)

aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = snakeCase }

data NotifyUser = NotifyUser
    { email :: Maybe NotifyEmail
    , phoneNumber :: Maybe NotifyPhoneNumber
    , docToChargeSMS :: DocumentID
    , authorUserId :: UserID
    , accessLink :: Url
    }
  deriving (Show, Generic)

instance ToJSON NotifyUser where
  toEncoding = genericToEncoding aesonOptions

-- For logging
instance ToJSON ConsumableAction where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = snakeCase }

notifyAction
  :: (MonadThrow m, MonadDB m, MonadLog m)
  => Url
  -> InstanceId
  -> [UserName]
  -> HighTongue.SystemActionMethods
  -> m ([NotifyUser], Maybe Message, Maybe Message)
notifyAction baseUrl instanceId userNames methods = do
  keyValues   <- selectInstanceKeyValues instanceId
  documentIds <- selectDocumentIdsByInstanceId instanceId
  documents   <- mapM (dbQuery . GetDocumentByDocumentID) documentIds
  -- TODO: This is hacky, we need to a charge a document for SMSs, so we pick the first.
  let docToChargeSMS = head documentIds
  -- TODO: This is hacky, we need the Branded Domain, so we find the author's siglink
  -- TODO: since we don't have access to Kontrakcja's context. We can get all info needed
  -- TODO: from the DB later if we have the author's user ID.
  let authorUserId =
        fromJust . maybesignatory . fromJust . getAuthorSigLink $ head documents

  let sls = mconcat $ fmap documentsignatorylinks documents

  accessTokens <- selectInstanceAccessTokens instanceId
  let usersWithHashes = fmap (\act -> (act ^. #userName, act ^. #hash)) accessTokens
  let accessLinks     = mkAccessLinks (fromUrl baseUrl) instanceId usersWithHashes

  -- TODO get rid of these throws, these things cannot fail due to how Flows are constructed.
  -- Even if they could, it seems bad to block the process just because notifications
  -- could not be sent out.
  notifyUsers <- forM userNames $ \userName -> do
    user <- throwIfNothing (UserNotFound userName) $ keyValues ^. #users % at userName
    signatoryLink <- throwIfNothing (AssociatedSignatoryNotFound user)
      $ find (compareUserAndSignatory user) sls
    accessLink <- throwIfNothing (AccessLinkNotFound userName)
      $ Map.lookup userName accessLinks

    let email       = textToMaybe $ getEmail signatoryLink
    let phoneNumber = textToMaybe $ getMobile signatoryLink
    pure $ NotifyUser { .. }

  memailMessage <- mapM (findMessageByName keyValues) (HighTongue.email methods)
  msmsMessage   <- mapM (findMessageByName keyValues) (HighTongue.sms methods)
  pure (notifyUsers, memailMessage, msmsMessage)
  where
    throwIfNothing :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
    throwIfNothing exception = maybe (throwM exception) pure

    textToMaybe :: Text -> Maybe Text
    textToMaybe t = if T.null $ T.strip t then Nothing else Just t

    findMessageByName :: MonadThrow m => InstanceKeyValues -> MessageName -> m Message
    findMessageByName keyValues messageName =
      throwIfNothing (MessageNotFound messageName)
        $  keyValues
        ^. #messages
        %  at messageName

toConsumableAction
  :: (MonadThrow m, MonadDB m, MonadLog m)
  => Url
  -> InstanceId
  -> EventInfo
  -> Machinize.LowAction
  -> m ConsumableAction
toConsumableAction baseUrl instanceId eventInfo = \case
  Machinize.CloseAll -> do
    docIds <- selectDocumentIdsByInstanceId instanceId
    return . CloseAll $ CloseAllAction instanceId docIds

  Machinize.Action (HighTongue.Notify userNames methods) -> do
    (users, emailMessage, smsMessage) <- notifyAction baseUrl instanceId userNames methods
    return . Notify $ NotifyAction { users, emailMessage, smsMessage }

  Machinize.Reject -> Reject <$> toConsumableRejectAction
    instanceId
    eventInfo
    (getRejectMessageFromEvent eventInfo)

  Machinize.Fail -> pure . Fail $ FailAction instanceId

toConsumableRejectAction
  :: (MonadThrow m, MonadDB m) => InstanceId -> EventInfo -> Maybe Text -> m RejectAction
toConsumableRejectAction instanceId EventInfo {..} rejectMessage = do
  signatoryMapping <- selectSignatoryInfo instanceId

  docIds           <- mapMaybeM
    (\(username2, _, docId) -> if username2 == eventInfoUser
      then do
        doc <- dbQuery $ GetDocumentByDocumentID docId
        if documentstatus doc == Pending
          then return $ Just (documentid doc)
          else return Nothing
      else return Nothing
    )
    signatoryMapping

  return $ RejectAction { rejector      = eventInfoUser
                        , instanceId
                        , documentName  = eventInfoDocument
                        , documentIds   = docIds
                        , rejectMessage = rejectMessage
                        }

sendEmailNotification
  :: ( CryptoRNG m
     , MonadLog m
     , MonadThrow m
     , MonadCatch m
     , MonadDB m
     , MonadEventStream m
     , TemplatesMonad m
     )
  => NotifyUser
  -> Message
  -> m ()
sendEmailNotification nu@NotifyUser {..} message = do
  logInfo "Sending Email notification" nu
  case email of
    Nothing           -> logAttention "User does not have email address" nu
    Just emailAddress -> notifyEmail authorUserId message accessLink emailAddress

sendSMSNotification
  :: ( CryptoRNG m
     , MonadLog m
     , MonadThrow m
     , MonadCatch m
     , MonadDB m
     , MonadEventStream m
     , TemplatesMonad m
     )
  => NotifyUser
  -> Message
  -> m ()
sendSMSNotification nu@NotifyUser {..} message = do
  logInfo "Sending SMS notification" nu
  case phoneNumber of
    Nothing        -> logAttention "User does not have phone number for SMS" nu
    Just numForSMS -> notifySMS docToChargeSMS message accessLink numForSMS

notifyEmail
  :: (CryptoRNG m, MonadLog m, MonadThrow m, MonadDB m, TemplatesMonad m)
  => UserID
  -> Message
  -> Url
  -> NotifyEmail
  -> m ()
notifyEmail authorUserId (Message msg) accessLink email = do
  bd    <- dbQuery $ GetBrandedDomainByUserID authorUserId
  ugwp  <- dbQuery $ UserGroupGetWithParentsByUserID authorUserId
  theme <- dbQuery . GetTheme $ fromMaybe (bd ^. #mailTheme) (ugwpUI ugwp ^. #mailTheme)

  logInfo "notifyEmail: Sending email" msg
  -- TODO Perhaps: we should reuse the helpers in DocMailView better
  mail <- kontramaillocal "noreply@scrive.com" bd theme LANG_EN "flowNotifyEmail" $ do
    F.value "creatorname" ("Scrive Flow" :: Text)
    F.value "flowNotifyMessage" msg
    F.value "flowAccessLink" (fromUrl accessLink)
    F.value "brandingdomainid" . show $ bd ^. #id
    F.value "brandinguserid" . show $ authorUserId
    F.value "logoAdler32" . imageAdler32 $ themeLogo theme
    F.value "brandcolor" . ensureHexRGB' $ themeBrandColor theme
    F.value "brandtextcolor" . ensureHexRGB' $ themeBrandTextColor theme
    F.value "actioncolor" . ensureHexRGB' $ themeActionColor theme
    F.value "actiontextcolor" . ensureHexRGB' $ themeActionTextColor theme
    F.value "font" $ themeFont theme
  scheduleEmailSendout
    $ mail { Mails.SendMail.to = [MailAddress { fullname = "", email = email }] }
  where ensureHexRGB' s = fromMaybe s . ensureHexRGB $ T.unpack s

notifySMS
  :: (MonadLog m, MonadDB m, MonadThrow m, MonadCatch m, MonadEventStream m)
  => DocumentID
  -> Message
  -> Url
  -> NotifyPhoneNumber
  -> m ()
notifySMS docToChargeSMS (Message message) accessLink phoneNum = do
  let sms = SMS { smsMSISDN        = phoneNum
                , kontraInfoForSMS = Nothing
                , smsBody = message <> " - Access documents: " <> fromUrl accessLink
                , smsOriginator    = "Scrive Flow"
                , smsProvider      = SMSDefault
                }
  -- TODO: Need to work out how to do this in a way that makes sense
  doc <- dbQuery $ GetDocumentByDocumentID docToChargeSMS
  logInfo "Flow notifySMS: Sending SMS" sms
  scheduleSMS doc sms

compareUserAndSignatory :: FlowUserId -> SignatoryLink -> Bool
compareUserAndSignatory (Email email) signatoryLink = getEmail signatoryLink == email
compareUserAndSignatory (PhoneNumber phoneNumber) signatoryLink =
  getMobile signatoryLink == phoneNumber
compareUserAndSignatory (UserId userId) signatoryLink =
  maybe False (userId ==) $ maybesignatory signatoryLink

consumeFlowAction
  :: ( CryptoRNG m
     , MonadDB m
     , GuardTimeConfMonad m
     , MailContextMonad m
     , MonadBaseControl IO m
     , MonadEventStream m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , TemplatesMonad m
     )
  => ConsumableAction
  -> m ()
consumeFlowAction action = do
  logInfo "Consuming Flow action" action
  case action of
    CloseAll CloseAllAction { instanceId, documentIds } -> forM_ documentIds $ \docId ->
      do
        doc <- dbQuery $ GetDocumentByDocumentID docId
        withDocument doc $ commonDocumentClosingActions doc
        -- TODO Flow: This event have to be generated after document sealing.
        sendEventCallback instanceId Completed

    Notify (NotifyAction users memailMsg msmsMsg) ->
      consumeNotifyAction users memailMsg msmsMsg

    Reject rejectAction            -> consumeRejectionAction rejectAction

    Fail   (FailAction instanceId) -> do
      logInfo "Flow process failed" action
      sendEventCallback instanceId Failed

sendEventCallback
  :: (MonadThrow m, MonadDB m, MonadTime m) => InstanceId -> FlowCallbackEventV1 -> m ()
sendEventCallback instanceId event = do
  fullInstance <- fromMaybeM (throwM $ NoInstance instanceId)
    $ selectFullInstance instanceId
  now <- currentTime
  let maybeCallback = fullInstance ^. #flowInstance % #callback
  whenJust maybeCallback $ \Callback {..} -> do
    let payload = case version of
          V1 -> FlowCallbackEventV1Envelope now instanceId event
    void $ scheduleNewCallback (fromUrl url) NoAuth payload

consumeNotifyAction
  :: ( MonadLog m
     , CryptoRNG m
     , MonadThrow m
     , MonadCatch m
     , MonadDB m
     , MonadEventStream m
     , TemplatesMonad m
     )
  => [NotifyUser]
  -> Maybe Message
  -> Maybe Message
  -> m ()
consumeNotifyAction users memailMsg msmsMsg = do
  logInfo "Consuming Flow Notify action" . Notify $ NotifyAction users memailMsg msmsMsg
  forM_ users $ \user -> do
    whenJust memailMsg $ sendEmailNotification user
    whenJust msmsMsg $ sendSMSNotification user

consumeRejectionAction
  :: (MonadDB m, MonadIO m, MonadLog m, MonadMask m, TemplatesMonad m)
  => RejectAction
  -> m ()
consumeRejectionAction RejectAction {..} = do
  logInfo "Cancelling documents for rejection of flow instance, and send event callback"
    $ object
        [ "document_ids" .= documentIds
        , "instance_id" .= instanceId
        , "document_name" .= documentName
        , "reject_message" .= rejectMessage
        ]

  forM_ documentIds $ \docId -> do
    -- Use the system actor for now as otherwise it requires
    -- additional monad constraints
    actor <- systemActor <$> currentTime
    withDocumentID docId . dbUpdate $ CancelDocument actor

  let event = RejectedEvent { userName = rejector, rejectMessage, documentName }

  sendEventCallback instanceId $ Callback.Rejected event

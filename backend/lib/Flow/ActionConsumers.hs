{-# LANGUAGE DeriveAnyClass #-}
module Flow.ActionConsumers
  ( ConsumableAction(..)
  , ActionsError(..)
  , NotifyUser(..)
  , toConsumableAction
  , notifyAction
  , consumeFlowAction
  , consumeNotifyAction
  ) where

import Control.Monad.Catch
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
import DB hiding (JSON(..))
import Doc.Action (commonDocumentClosingActions)
import Doc.DocumentID
import Doc.DocumentMonad
import Doc.Model.Query (GetDocumentByDocumentID(..))
import Doc.Signing.Model ()
import Doc.Types.Document
import Doc.Types.SignatoryLink
import EventStream.Class
import File.Storage
import Flow.Id
import Flow.Message.Internal
import Flow.Model
import Flow.Model.InstanceSession
import Flow.Model.Types.FlowUserId
import Flow.Names
import Flow.Routes.Types
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
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Color
import qualified Flow.HighTongue as HighTongue
import qualified Flow.Machinize as Machinize

type NotifyPhoneNumber = Text
type NotifyEmail = Text

data ConsumableAction
  = Notify
      { users :: [NotifyUser]
      , message :: Message
      }
  | CloseAll
      { documentIds :: [DocumentID]
      }
  | Fail
  deriving (Show, Generic)

data ActionsError
    = UserNotFound HighTongue.UserName
    | AssociatedSignatoryNotFound FlowUserId
    | MessageNotFound HighTongue.MessageName
    | AccessLinkNotFound HighTongue.UserName
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
  -> MessageName
  -> m ([NotifyUser], Message)
notifyAction baseUrl instanceId userNames messageName = do
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

  message <-
    throwIfNothing (MessageNotFound messageName) $ keyValues ^. #messages % at messageName
  pure (notifyUsers, message)
  where
    throwIfNothing :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
    throwIfNothing exception = maybe (throwM exception) pure

    textToMaybe :: Text -> Maybe Text
    textToMaybe t = if T.null $ T.strip t then Nothing else Just t

toConsumableAction
  :: (MonadThrow m, MonadDB m, MonadLog m)
  => Url
  -> InstanceId
  -> Machinize.LowAction
  -> m ConsumableAction
toConsumableAction baseUrl instanceId = \case
  Machinize.CloseAll -> CloseAll <$> selectDocumentIdsByInstanceId instanceId
  Machinize.Action (HighTongue.Notify userNames messageName) ->
    uncurry Notify <$> notifyAction baseUrl instanceId userNames messageName
  Machinize.Fail -> pure Fail

sendNotification
  :: ( CryptoRNG m
     , MonadLog m
     , MonadThrow m
     , MonadCatch m
     , MonadDB m
     , MonadEventStream m
     , TemplatesMonad m
     )
  => Message
  -> NotifyUser
  -> m ()
sendNotification message nu@NotifyUser {..} = do
  logInfo "Sending notification" nu
  when (isJust email) . notifyEmail authorUserId message accessLink $ fromJust email
  when (isJust phoneNumber) . notifySMS docToChargeSMS message accessLink $ fromJust
    phoneNumber

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
     , DocumentMonad m
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
    CloseAll documentIds -> forM_ documentIds $ \docId -> do
      doc <- dbQuery $ GetDocumentByDocumentID docId
      withDocument doc $ commonDocumentClosingActions doc
    Notify users message -> forM_ users $ sendNotification message
    Fail                 -> logInfo "Flow process failed" action

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
  -> Message
  -> m ()
consumeNotifyAction users message = do
  logInfo "Consuming Flow Notify action" $ Notify users message
  forM_ users $ sendNotification message

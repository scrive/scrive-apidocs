-----------------------------------------------------------------------------
-- |
-- Module      :  Mails.Events
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Sendgrid events interface. 'processEvents' is used when sendgrid contacts us.
-- mailinfo param is set when we are sending mails.
-----------------------------------------------------------------------------
module Mails.Events (
    processEvents
  , mailDeliveredInvitation
  , mailDeferredInvitation
  , mailUndeliveredInvitation
  , mailUndeliveredConfirmation
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Crypto.RNG
import Data.Functor ((<&>))
import Data.Time
import Log
import Text.StringTemplates.Templates hiding (runTemplatesT)
import Text.StringTemplates.TemplatesLoader
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Control.Monad.Trans.Instances ()
import CronEnv
import DB
import Doc.API.Callback.Model
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, withDocumentID)
import Doc.DocViewMail
import Doc.Logging
import Doc.Model
import Doc.SignatoryLinkID
import KontraLink
import Log.Identifier
import Mails.KontraInfoForMail
import Mails.MailsData
import Mails.Model hiding (Mail)
import Mails.SendMail
import Templates
import Theme.Model
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

processEvents :: (MonadDB m, MonadReader CronEnv m, MonadLog m, MonadCatch m, CryptoRNG m) => Int -> m Int
processEvents limit = do
  -- We limit processing to <limit> events not to have issues with large number of documents locked.
  -- kontra infos are retrieved in did ascending order to acquire locks without deadlocking
  events <- take limit <$> dbQuery GetUnreadEvents
  forM_ events $ \(eid, mid, eventType, mkifm) ->
    case mkifm of
      Nothing -> do
        logInfo "Proccessing event without document info" $
          object [ identifier eid
                 , identifier mid
                 , "event_type" .= show eventType
                 ]
        markEventAsRead eid
      Just kifm -> localData [identifier eid, identifier mid] $ processEvent (eid, mid, eventType, kifm)
  return $ length events
  where
    processEvent (eid, mid, eventType, kontraInfoForMail) = do
      now <- currentTime
      mailNoreplyAddress <- asks ceMailNoreplyAddress
      mmailSendTimeStamp <- dbQuery $ GetEmailSendoutTime mid
      let timeDiff = case mmailSendTimeStamp of
                       Nothing -> Nothing
                       Just mailSendTimeStamp -> Just $ floor $ toRational $ mailSendTimeStamp `diffUTCTime` now
      templates <- asks ceTemplates
      markEventAsRead eid
      case kontraInfoForMail of
        DocumentInvitationMail docid slid -> forExistingDocument docid $
          handleEventInvitation slid timeDiff templates eventType mailNoreplyAddress
        DocumentConfirmationMail docid slid -> forExistingDocument docid $
          handleEventConfirmation slid timeDiff templates eventType mailNoreplyAddress
        OtherDocumentMail docid -> forExistingDocument docid $
          handleEventOtherMail timeDiff eventType

    forExistingDocument docid action = do
      exists <- dbQuery $ DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor docid
      if exists
        then logDocument docid $ withDocumentID docid action
        else logInfo_ "Email event for purged/non-existing document"

markEventAsRead :: (MonadLog m, MonadThrow m, MonadDB m) => EventID -> m ()
markEventAsRead eid = do
  now <- currentTime
  success <- dbUpdate $ MarkEventAsRead eid now
  unless success $
    logAttention_ "Couldn't mark event as read"

logEmails :: MonadLog m => Text -> Text -> m ()
logEmails signemail email = logInfo "Comparing emails" $ object
  [ "signatory_email" .= signemail
  , "event_email" .= email
  ]

handleEventInvitation
  :: (DocumentMonad m, MonadLog m, CryptoRNG m, MonadCatch m, MonadDB m)
  => SignatoryLinkID -> Maybe Int -> GlobalTemplates -> Event -> Text -> m ()
handleEventInvitation slid timeDiff templates eventType mailNoreplyAddress =
  logSignatory slid $ do
    logInfo_ "Processing invitation event"

    (muid, signemail) <- theDocument <&> \doc -> case getSigLinkFor slid doc of
      Nothing -> (Nothing, "")
      Just sl -> (maybesignatory sl, getEmail sl)
    let (email, nev) = normaliseEvent signemail eventType
    bd <- theDocument >>= \doc -> case getAuthorSigLink doc >>= maybesignatory of
      Nothing -> dbQuery $ GetMainBrandedDomain
      Just uid -> dbQuery $ GetBrandedDomainByUserID uid

    logEmails signemail email
    theDocument >>= \doc -> runTemplatesT (getLang doc, templates) $ case nev of
      EmailOpenedEvent -> handleOpenedInvitation slid email muid
      DeliveryEvent Delivered ->
        handleDeliveredInvitation mailNoreplyAddress bd slid timeDiff
      DeliveryEvent Undelivered ->
        handleUndeliveredInvitation mailNoreplyAddress bd slid
      DeliveryEvent Deferred ->
        handleDeferredInvitation mailNoreplyAddress bd slid email
      _ -> return ()

handleEventConfirmation
  :: ( CryptoRNG m, DocumentMonad m, MonadCatch m, MonadLog m, MonadThrow m
     , MonadDB m )
  => SignatoryLinkID -> Maybe Int -> GlobalTemplates -> Event
  -> Text -> m ()
handleEventConfirmation slid timeDiff templates eventType
                        mailNoreplyAddress =
  logSignatory slid $ do
    logInfo_ "Processing confirmation event"

    signemail <- (maybe "" getEmail . getSigLinkFor slid) <$> theDocument
    let (email, nev) = normaliseEvent signemail eventType
    bd <- theDocument >>= \doc -> case getAuthorSigLink doc >>= maybesignatory of
      Nothing -> dbQuery $ GetMainBrandedDomain
      Just uid -> dbQuery $ GetBrandedDomainByUserID uid

    logEmails signemail email
    theDocument >>= \doc -> runTemplatesT (getLang doc, templates) $ case nev of
      DeliveryEvent Delivered ->
        handleDeliveredConfirmation slid timeDiff
      DeliveryEvent Undelivered ->
        handleUndeliveredConfirmation mailNoreplyAddress bd slid
      DeliveryEvent Deferred -> handleDeferredConfirmation slid
      _ -> handleEventLoggingDeliveryTime signemail timeDiff eventType

handleEventOtherMail
  :: (DocumentMonad m, MonadLog m, MonadThrow m, MonadDB m)
  => Maybe Int -> Event -> m ()
handleEventOtherMail timeDiff eventType = do
  logInfo_ "Processing related mail event"
  handleEventLoggingDeliveryTime "" timeDiff eventType

handleEventLoggingDeliveryTime
  :: (DocumentMonad m, MonadLog m, MonadThrow m)
  => Text -> Maybe Int -> Event -> m ()
handleEventLoggingDeliveryTime signemail timeDiff eventType = do
  case snd $ normaliseEvent signemail eventType of
    DeliveryEvent Delivered -> logDeliveryTime timeDiff
    _ -> return ()

logDeliveryTime :: (DocumentMonad m, MonadLog m, MonadThrow m) => Maybe Int -> m ()
logDeliveryTime timeDiff = theDocument >>= \d -> do
  logInfo "Email delivered" $ object [
      identifier $ documentauthorugid d
    , "delivery_time" .= timeDiff
    ]

handleDeliveredInvitation :: (CryptoRNG m, MonadThrow m, MonadLog m, DocumentMonad m, TemplatesMonad m)
                          => Text -> BrandedDomain -> SignatoryLinkID -> Maybe Int -> m ()
handleDeliveredInvitation mailNoreplyAddress bd slid timeDiff = do
  getSigLinkFor slid <$> theDocument >>= \case
    Just signlink -> do
      logDeliveryTime timeDiff
      -- send it only if email was reported deferred earlier
      when (mailinvitationdeliverystatus signlink == Deferred) $ do
        mail <- mailDeliveredInvitation mailNoreplyAddress bd signlink =<< theDocument
        theDocument >>= \d -> scheduleEmailSendout $ mail {
          to = [getMailAddress $ fromJust $ getAuthorSigLink d]
        }
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) slid
      void $ dbUpdate $ SetEmailInvitationDeliveryStatus slid Delivered actor
      return ()
    Nothing -> return ()

handleOpenedInvitation :: (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => SignatoryLinkID -> Text -> Maybe UserID -> m ()
handleOpenedInvitation slid email muid = do
  now  <- currentTime
  void $ dbUpdate $ MarkInvitationRead slid
          (mailSystemActor now muid email slid)
  return ()

handleDeferredInvitation :: (CryptoRNG m, MonadLog m, MonadThrow m, DocumentMonad m, TemplatesMonad m) => Text -> BrandedDomain -> SignatoryLinkID -> Text -> m ()
handleDeferredInvitation mailNoreplyAddress bd slid email = do
  time <- currentTime
  getSigLinkFor slid <$> theDocument >>= \case
    Just sl -> do
      let actor = mailSystemActor time (maybesignatory sl) email slid
      success <- dbUpdate $ SetEmailInvitationDeliveryStatus slid Deferred actor
      when success $ do
        mail <- mailDeferredInvitation mailNoreplyAddress bd sl =<< theDocument
        theDocument >>= \d -> scheduleEmailSendout $ mail {
          to = [getMailAddress $ fromJust $ getAuthorSigLink d]
        }
    Nothing -> return ()

handleUndeliveredInvitation :: (CryptoRNG m, MonadCatch m, MonadLog m, DocumentMonad m, TemplatesMonad m) => Text -> BrandedDomain -> SignatoryLinkID -> m ()
handleUndeliveredInvitation mailNoreplyAddress bd slid = do
  getSigLinkFor slid <$> theDocument >>= \case
    Just signlink | mailinvitationdeliverystatus signlink == Delivered -> do
      logInfo "Undelivered email event for email that was already delivered" $ object ["signatory_email" .= getEmail signlink]
                  | signatoryAlreadyHandled signlink -> return ()
                  | otherwise -> do
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) slid
      void $ dbUpdate $ SetEmailInvitationDeliveryStatus slid Undelivered actor
      mail <- mailUndeliveredInvitation mailNoreplyAddress bd signlink =<< theDocument
      theDocument >>= \d -> scheduleEmailSendout $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink d]
      }
      triggerAPICallbackIfThereIsOne =<< theDocument
    Nothing -> return ()
  where signatoryAlreadyHandled sl = case signatoryrole sl of
          SignatoryRoleSigningParty -> isJust $ maybesigninfo sl
          SignatoryRoleViewer -> isJust $ maybeseeninfo sl
          SignatoryRoleApprover -> isJust $ maybesigninfo sl
          SignatoryRoleForwardedSigningParty -> True
          SignatoryRoleForwardedApprover -> True

handleDeliveredConfirmation
  :: (DocumentMonad m, MonadCatch m, MonadLog m, TemplatesMonad m)
  => SignatoryLinkID -> Maybe Int -> m ()
handleDeliveredConfirmation slid timeDiff = do
  logDeliveryTime timeDiff
  mSL <- getSigLinkFor slid <$> theDocument
  case mSL of
    Nothing -> return ()
    Just sl -> do
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory sl) (getEmail sl) slid
      void $ dbUpdate $ SetEmailConfirmationDeliveryStatus slid Delivered actor
      triggerAPICallbackIfThereIsOne =<< theDocument

handleUndeliveredConfirmation
  :: ( CryptoRNG m, DocumentMonad m, MonadCatch m, MonadLog m, MonadThrow m
     , TemplatesMonad m )
  => Text -> BrandedDomain -> SignatoryLinkID -> m ()
handleUndeliveredConfirmation mailNoreplyAddress bd slid = do
  mSL <- getSigLinkFor slid <$> theDocument
  case mSL of
    Nothing -> return ()
    Just sl -> do
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory sl) (getEmail sl) slid
      void $ dbUpdate $ SetEmailConfirmationDeliveryStatus slid Undelivered actor
      triggerAPICallbackIfThereIsOne =<< theDocument

      mAuthorSL <- getAuthorSigLink <$> theDocument
      case mAuthorSL of
        Just authorSL | signatorylinkid authorSL /= slid -> do
          mail <- mailUndeliveredConfirmation mailNoreplyAddress bd sl =<< theDocument
          scheduleEmailSendout $ mail { to = [getMailAddress authorSL] }
        _ -> return ()

handleDeferredConfirmation
  :: (DocumentMonad m, MonadCatch m, MonadLog m, TemplatesMonad m)
  => SignatoryLinkID -> m ()
handleDeferredConfirmation slid = do
  mSL <- getSigLinkFor slid <$> theDocument
  case mSL of
    Nothing -> return ()
    Just sl -> do
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory sl) (getEmail sl) slid
      void $ dbUpdate $ SetEmailConfirmationDeliveryStatus slid Deferred actor
      triggerAPICallbackIfThereIsOne =<< theDocument

mailDeliveredInvitation :: (TemplatesMonad m, MonadDB m, MonadThrow m) => Text -> BrandedDomain -> SignatoryLink -> Document -> m Mail
mailDeliveredInvitation mailNoreplyAddress bd signlink doc =do
  theme <- dbQuery $ GetTheme $ get bdMailTheme bd
  kontramail mailNoreplyAddress bd theme "invitationMailDeliveredAfterDeferred" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "email" $ getEmail signlink
    F.value "documenttitle" $ documenttitle doc
    F.value "ctxhostpart" $ get bdUrl bd
    brandingMailFields theme

mailDeferredInvitation ::(TemplatesMonad m, MonadDB m, MonadThrow m) => Text -> BrandedDomain -> SignatoryLink -> Document -> m Mail
mailDeferredInvitation mailNoreplyAddress bd sl doc = do
  theme <- dbQuery $ GetTheme $ get bdMailTheme bd
  kontramail mailNoreplyAddress bd theme"invitationMailDeferred" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "counterpartname" $ getFullName sl
    F.value "counterpartemail" $ getEmail sl
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" $ get bdUrl bd
    brandingMailFields theme

mailUndeliveredInvitation :: (TemplatesMonad m, MonadDB m, MonadThrow m) => Text -> BrandedDomain -> SignatoryLink -> Document -> m Mail
mailUndeliveredInvitation mailNoreplyAddress bd signlink doc =do
  theme <- dbQuery $ GetTheme $ get bdMailTheme bd
  kontramail mailNoreplyAddress bd theme "invitationMailUndelivered" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "documenttitle" $ documenttitle doc
    F.value "email" $ getEmail signlink
    F.value "name" $ getFullName signlink
    F.value "signing" $ signatoryrole signlink == SignatoryRoleSigningParty
    F.value "viewing" $ signatoryrole signlink == SignatoryRoleViewer
    F.value "approving" $ signatoryrole signlink == SignatoryRoleApprover
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" $ get bdUrl bd
    brandingMailFields theme

mailUndeliveredConfirmation
  :: (MonadDB m, MonadThrow m, TemplatesMonad m)
  => Text -> BrandedDomain -> SignatoryLink -> Document -> m Mail
mailUndeliveredConfirmation mailNoreplyAddress bd sl doc = do
  theme <- dbQuery $ GetTheme $ get bdMailTheme bd
  kontramail mailNoreplyAddress bd theme "confirmationMailUndelivered" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "email" $ getEmail sl
    F.value "name" $ getFullName sl
    F.value "documenttitle" $ documenttitle doc
    F.value "documentlink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" $ get bdUrl bd
    brandingMailFields theme

data NormalisedEvent
  = DeliveryEvent DeliveryStatus
  | EmailOpenedEvent
  | OtherEvent

-- Since when email is reported deferred author has a possibility to
-- change email address, we don't want to send him emails reporting
-- success/failure for old signatory address, so we need to compare
-- addresses here (for dropped/bounce events.)
--
-- We send notification that email is reported deferred after
-- fifth attempt has failed - this happens after ~10 minutes
-- from sendout.
normaliseEvent :: Text -> Event -> (Text, NormalisedEvent)
normaliseEvent currentEmail = \case
  SendGridEvent email ev _ -> (email, normaliseSendGridEvent email ev)
  MailGunEvent email ev -> (email, normaliseMailGunEvent email ev)
  SocketLabsEvent email ev -> (email, normaliseSocketLabsEvent email ev)
  MailJetEvent email ev -> (email, normaliseMailJetEvent ev)

  where
    normaliseSendGridEvent :: Text -> SendGridEvent -> NormalisedEvent
    normaliseSendGridEvent email = \case
      SG_Opened -> EmailOpenedEvent
      SG_Dropped _ | currentEmail == email -> DeliveryEvent Undelivered
      SG_Deferred _ 5 -> DeliveryEvent Deferred
      SG_Bounce _ _ _ | currentEmail == email -> DeliveryEvent Undelivered
      SG_Delivered _ -> DeliveryEvent Delivered
      _ -> OtherEvent

    normaliseMailGunEvent :: Text -> MailGunEvent -> NormalisedEvent
    normaliseMailGunEvent email = \case
      MG_Opened -> EmailOpenedEvent
      MG_Delivered -> DeliveryEvent Delivered
      MG_Bounced _ _ _  | currentEmail == email -> DeliveryEvent Undelivered
      MG_Dropped _ | currentEmail == email -> DeliveryEvent Undelivered
      _ -> OtherEvent

    normaliseSocketLabsEvent :: Text -> SocketLabsEvent -> NormalisedEvent
    normaliseSocketLabsEvent email = \case
      SL_Opened -> EmailOpenedEvent
      SL_Delivered -> DeliveryEvent Delivered
      -- 5001 = out of office / autoreply.
      -- See https://support.socketlabs.com/index.php/Knowledgebase/Article/View/123
      SL_Failed 0 5001 -> DeliveryEvent Delivered
      SL_Failed _ _ | currentEmail == email -> DeliveryEvent Undelivered
      _ -> OtherEvent

    normaliseMailJetEvent :: MailJetEvent -> NormalisedEvent
    normaliseMailJetEvent = \case
      MJ_Open -> EmailOpenedEvent
      MJ_Sent -> DeliveryEvent Delivered
      MJ_Bounce_Hard -> DeliveryEvent Undelivered
      MJ_Blocked -> DeliveryEvent Undelivered
      _ -> OtherEvent

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
  ) where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Functor
import Log
import Text.StringTemplates.Templates hiding (runTemplatesT)
import qualified Text.StringTemplates.Fields as F

import ActionQueue.Scheduler
import AppConf
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import Control.Monad.Trans.Instances ()
import Crypto.RNG
import DB
import Doc.API.Callback.Model
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, withDocumentID)
import Doc.DocViewMail
import Doc.Logging
import Doc.Model
import Doc.SignatoryLinkID
import KontraLink
import KontraPrelude
import Log.Identifier
import Mails.MailsConfig
import Mails.MailsData
import Mails.Model hiding (Mail)
import Mails.SendMail
import Templates
import Theme.Model
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

processEvents :: Scheduler ()
processEvents = (take 50 <$> dbQuery GetUnreadEvents) >>= mapM_ (\event@(eid, mid, _, _) -> do
  -- We limit processing to 50 events not to have issues with large number of documents locked.
  localData [identifier_ eid, identifier_ mid] $ do
    processEvent event
  )
  where
    processEvent (eid, _mid, XSMTPAttrs [("mailinfo", mi)], eventType) = do
      case maybeRead mi of
        Just (Invitation docid slid) -> logDocumentAndSignatory docid slid $ do
          logInfo_ "Processing invitation event"
          withDocumentID docid $ do
              markEventAsRead eid
              appConf <- sdAppConf <$> ask
              bd <- (maybesignatory =<<) . getAuthorSigLink <$> theDocument >>= \case
                          Nothing -> dbQuery $ GetMainBrandedDomain
                          Just uid -> do
                            dbQuery $ GetBrandedDomainByUserID uid
              msl <- getSigLinkFor slid <$> theDocument
              let muid = maybe Nothing maybesignatory msl
              let signemail = maybe "" getEmail msl
              templates <- getGlobalTemplates
              let logEmails email = logInfo "Comparing emails" $ object [
                        "signatory_email" .= signemail
                      , "event_email" .= email
                      ]
                  mc = mailsConfig $ appConf
                  -- since when email is reported deferred author has a possibility to
                  -- change email address, we don't want to send him emails reporting
                  -- success/failure for old signatory address, so we need to compare
                  -- addresses here (for dropped/bounce events)
                  handleEv (SendGridEvent email ev _) = do
                    logEmails email
                    case ev of
                      SG_Opened -> handleOpenedInvitation slid email muid
                      SG_Delivered _ -> handleDeliveredInvitation bd mc slid
                      -- we send notification that email is reported deferred after
                      -- fifth attempt has failed - this happens after ~10 minutes
                      -- from sendout
                      SG_Deferred _ 5 -> handleDeferredInvitation bd mc slid email
                      SG_Dropped _ -> when (signemail == email) $ handleUndeliveredInvitation bd mc slid
                      SG_Bounce _ _ _ -> when (signemail == email) $ handleUndeliveredInvitation bd mc slid
                      _ -> return ()
                  handleEv (MailGunEvent email ev) = do
                    logEmails email
                    case ev of
                      MG_Opened -> handleOpenedInvitation slid email muid
                      MG_Delivered -> handleDeliveredInvitation bd mc slid
                      MG_Bounced _ _ _ -> when (signemail == email) $ handleUndeliveredInvitation bd mc slid
                      MG_Dropped _ -> when (signemail == email) $ handleUndeliveredInvitation bd mc slid
                      _ -> return ()
                  handleEv (SocketLabsEvent email ev) = do
                    logEmails email
                    case ev of
                      SL_Opened -> handleOpenedInvitation slid email muid
                      SL_Delivered -> handleDeliveredInvitation bd mc slid
                      SL_Failed 0 5001 -> handleDeliveredInvitation bd mc slid -- out of office/autoreply; https://support.socketlabs.com/index.php/Knowledgebase/Article/View/123
                      SL_Failed _ _-> when (signemail == email) $ handleUndeliveredInvitation bd mc slid
                      _ -> return ()
                  handleEv (SendinBlueEvent email ev) = do
                    logEmails email
                    case ev of
                      SiB_Delivered -> handleDeliveredInvitation bd mc slid
                      SiB_Opened -> handleOpenedInvitation slid email muid
                      SiB_Deferred _ -> when (signemail == email) $ handleDeferredInvitation bd mc slid email
                      SiB_HardBounce   _ -> when (signemail == email) $ handleUndeliveredInvitation bd mc slid
                      SiB_SoftBounce   _ -> when (signemail == email) $ handleUndeliveredInvitation bd mc slid
                      SiB_Blocked      _ -> when (signemail == email) $ handleUndeliveredInvitation bd mc slid
                      SiB_InvalidEmail _ -> when (signemail == email) $ handleUndeliveredInvitation bd mc slid
                      _ -> return ()

              theDocument >>= \doc -> runTemplatesT (getLang doc, templates) $ handleEv eventType
        _ -> markEventAsRead eid
    processEvent (eid, _ , _, _) = markEventAsRead eid

    markEventAsRead eid = do
      now <- currentTime
      success <- dbUpdate $ MarkEventAsRead eid now
      when (not success) $
        logAttention_ "Couldn't mark event as read"

handleDeliveredInvitation :: (CryptoRNG m, MonadThrow m, MonadLog m, DocumentMonad m, TemplatesMonad m)
                          => BrandedDomain -> MailsConfig -> SignatoryLinkID -> m ()
handleDeliveredInvitation bd mc slid = do
  getSigLinkFor slid <$> theDocument >>= \case
    Just signlink -> do
      -- send it only if email was reported deferred earlier
      when (mailinvitationdeliverystatus signlink == Deferred) $ do
        mail <- mailDeliveredInvitation bd signlink =<< theDocument
        theDocument >>= \d -> scheduleEmailSendout mc $ mail {
          to = [getMailAddress $ $fromJust $ getAuthorSigLink d]
        }
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) slid
      _ <- dbUpdate $ SetEmailInvitationDeliveryStatus slid Delivered actor
      return ()
    Nothing -> return ()

handleOpenedInvitation :: (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => SignatoryLinkID -> String -> Maybe UserID -> m ()
handleOpenedInvitation slid email muid = do
  now  <- currentTime
  _ <- dbUpdate $ MarkInvitationRead slid
          (mailSystemActor now muid email slid)
  return ()

handleDeferredInvitation :: (CryptoRNG m, MonadLog m, MonadThrow m, DocumentMonad m, TemplatesMonad m) => BrandedDomain -> MailsConfig -> SignatoryLinkID -> String -> m ()
handleDeferredInvitation bd mc slid email = do
  time <- currentTime
  getSigLinkFor slid <$> theDocument >>= \case
    Just sl -> do
      let actor = mailSystemActor time (maybesignatory sl) email slid
      success <- dbUpdate $ SetEmailInvitationDeliveryStatus slid Deferred actor
      when success $ do
        mail <- mailDeferredInvitation bd sl =<< theDocument
        theDocument >>= \d -> scheduleEmailSendout mc $ mail {
          to = [getMailAddress $ $fromJust $ getAuthorSigLink d]
        }
    Nothing -> return ()

handleUndeliveredInvitation :: (CryptoRNG m, MonadCatch m, MonadLog m, DocumentMonad m, TemplatesMonad m) => BrandedDomain -> MailsConfig -> SignatoryLinkID -> m ()
handleUndeliveredInvitation bd mc slid = do
  getSigLinkFor slid <$> theDocument >>= \case
    Just signlink -> do
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) slid
      _ <- dbUpdate $ SetEmailInvitationDeliveryStatus slid Undelivered actor
      mail <- mailUndeliveredInvitation bd signlink =<< theDocument
      theDocument >>= \d -> scheduleEmailSendout mc $ mail {
        to = [getMailAddress $ $fromJust $ getAuthorSigLink d]
      }
      triggerAPICallbackIfThereIsOne =<< theDocument
    Nothing -> return ()

mailDeliveredInvitation :: (TemplatesMonad m, MonadDB m, MonadThrow m) => BrandedDomain -> SignatoryLink -> Document -> m Mail
mailDeliveredInvitation bd signlink doc =do
  theme <- dbQuery $ GetTheme $ bdMailTheme bd
  kontramail bd theme "invitationMailDeliveredAfterDeferred" $ do
    F.value "authorname" $ getFullName $ $fromJust $ getAuthorSigLink doc
    F.value "email" $ getEmail signlink
    F.value "documenttitle" $ documenttitle doc
    F.value "ctxhostpart" $ bdUrl bd
    brandingMailFields theme


mailDeferredInvitation ::(TemplatesMonad m, MonadDB m, MonadThrow m) => BrandedDomain -> SignatoryLink -> Document -> m Mail
mailDeferredInvitation bd sl doc = do
  theme <- dbQuery $ GetTheme $ bdMailTheme bd
  kontramail bd theme"invitationMailDeferred" $ do
    F.value "authorname" $ getFullName $ $fromJust $ getAuthorSigLink doc
    F.value "counterpartname" $ getFullName sl
    F.value "counterpartemail" $ getEmail sl
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" $ bdUrl bd
    brandingMailFields theme


mailUndeliveredInvitation :: (TemplatesMonad m, MonadDB m, MonadThrow m) => BrandedDomain -> SignatoryLink -> Document -> m Mail
mailUndeliveredInvitation bd signlink doc =do
  theme <- dbQuery $ GetTheme $ bdMailTheme bd
  kontramail bd theme "invitationMailUndelivered" $ do
    F.value "authorname" $ getFullName $ $fromJust $ getAuthorSigLink doc
    F.value "documenttitle" $ documenttitle doc
    F.value "email" $ getEmail signlink
    F.value "name" $ getFullName signlink
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" $ bdUrl bd
    brandingMailFields theme

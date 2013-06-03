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

import Data.Maybe
import Control.Monad.Reader

import AppConf
import ActionQueue.Scheduler
import Crypto.RNG
import DB
import Doc.Model
import Doc.DocStateData
import KontraLink
import Mails.MailsConfig
import Mails.MailsData
import Mails.Model hiding (Mail)
import Mails.SendMail
import Utils.Read
import Text.StringTemplates.Templates hiding (runTemplatesT)
import Templates
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Doc.SignatoryLinkID
import qualified Log
import Util.Actor
import Stats.Control
import qualified Text.StringTemplates.Fields as F

processEvents :: Scheduler ()
processEvents = dbQuery GetUnreadEvents >>= mapM_ processEvent
  where
    processEvent (eid, mid, XSMTPAttrs [("mailinfo", mi)], eventType) = do
      markEventAsRead eid
      case maybeRead mi of
        Just (Invitation docid signlinkid) -> do
          Log.debug $ "Processiong invitation event: " ++ show (Invitation docid signlinkid)
          mdoc <- dbQuery $ GetDocumentByDocumentID docid
          case mdoc of
            Nothing -> do
              Log.debug $ "No document with id = " ++ show docid
              deleteEmail mid
            Just doc -> do
              let msl = getSigLinkFor doc signlinkid
                  muid = maybe Nothing maybesignatory msl
              let signemail = maybe "" getEmail msl
              sd <- ask
              templates <- getGlobalTemplates
              let host = hostpart $ sdAppConf sd
                  mc = mailsConfig $ sdAppConf sd
                  -- since when email is reported deferred author has a possibility to
                  -- change email address, we don't want to send him emails reporting
                  -- success/failure for old signatory address, so we need to compare
                  -- addresses here (for dropped/bounce events)
                  handleEv (SendGridEvent email ev _) = do
                    Log.debug $ signemail ++ " == " ++ email
                    runTemplatesT (getLang doc, templates) $ case ev of
                      SG_Opened -> handleOpenedInvitation doc signlinkid email muid
                      SG_Delivered _ -> handleDeliveredInvitation (host, mc) doc signlinkid
                      -- we send notification that email is reported deferred after
                      -- fifth attempt has failed - this happens after ~10 minutes
                      -- from sendout
                      SG_Deferred _ 5 -> handleDeferredInvitation (host, mc) doc signlinkid email
                      SG_Dropped _ -> when (signemail == email) $ handleUndeliveredInvitation (host, mc) doc signlinkid
                      SG_Bounce _ _ _ -> when (signemail == email) $ handleUndeliveredInvitation (host, mc) doc signlinkid
                      _ -> return ()
                  handleEv (MailGunEvent email ev) = do
                    Log.debug $ signemail ++ " == " ++ email
                    runTemplatesT (getLang doc, templates) $ case ev of
                      MG_Opened -> handleOpenedInvitation doc signlinkid email muid
                      MG_Delivered -> handleDeliveredInvitation (host, mc) doc signlinkid
                      MG_Bounced _ _ _ -> when (signemail == email) $ handleUndeliveredInvitation (host, mc) doc signlinkid
                      MG_Dropped _ -> when (signemail == email) $ handleUndeliveredInvitation (host, mc) doc signlinkid
                      _ -> return ()
              handleEv eventType
        _ -> return ()
    processEvent (eid, _ , _, _) = markEventAsRead eid

    markEventAsRead eid = do
      now <- getMinutesTime
      success <- dbUpdate $ MarkEventAsRead eid now
      when (not success) $
        Log.error $ "Couldn't mark event #" ++ show eid ++ " as read"

    deleteEmail :: MonadDB m => MailID -> m ()
    deleteEmail mid = do
      success <- dbUpdate $ DeleteEmail mid
      if (not success)
        then Log.error $ "Couldn't delete email #" ++ show mid
        else Log.debug $ "Deleted email #" ++ show mid

handleDeliveredInvitation :: (CryptoRNG m, MonadDB m, TemplatesMonad m) => (String, MailsConfig) -> Document -> SignatoryLinkID -> m ()
handleDeliveredInvitation (hostpart, mc) doc signlinkid = do
  case getSigLinkFor doc signlinkid of
    Just signlink -> do
      -- send it only if email was reported deferred earlier
      when (invitationdeliverystatus signlink == Deferred) $ do
        mail <- mailDeliveredInvitation hostpart doc signlink
        scheduleEmailSendout mc $ mail {
          to = [getMailAddress $ fromJust $ getAuthorSigLink doc]
        }
      time <- getMinutesTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetEmailInvitationDeliveryStatus (documentid doc) signlinkid Delivered actor
      return ()
    Nothing -> return ()

handleOpenedInvitation :: (MonadDB m, TemplatesMonad m, MonadIO m) => Document -> SignatoryLinkID -> String -> Maybe UserID -> m ()
handleOpenedInvitation doc signlinkid email muid = do
  now  <- getMinutesTime
  success <- dbUpdate $ MarkInvitationRead (documentid doc) signlinkid
          (mailSystemActor now muid email signlinkid)
  when success $ do
    Just d <- dbQuery $ GetDocumentByDocumentID $ documentid doc
    case getSigLinkFor d signlinkid of
      Just sl -> do
        _ <- addSignStatOpenEvent d sl
        return ()
      _ -> return ()

handleDeferredInvitation :: (CryptoRNG m, MonadDB m, TemplatesMonad m) => (String, MailsConfig) -> Document -> SignatoryLinkID -> String -> m ()
handleDeferredInvitation (hostpart, mc) doc signlinkid email = do
  time <- getMinutesTime
  case getSigLinkFor doc signlinkid of
    Just sl -> do
      let actor = mailSystemActor time (maybesignatory sl) email signlinkid
      success <- dbUpdate $ SetEmailInvitationDeliveryStatus (documentid doc) signlinkid Deferred actor
      when success $ do
        Just d <- dbQuery $ GetDocumentByDocumentID $ documentid doc
        mail <- mailDeferredInvitation hostpart d sl
        scheduleEmailSendout mc $ mail {
          to = [getMailAddress $ fromJust $ getAuthorSigLink d]
        }
    Nothing -> return ()

handleUndeliveredInvitation :: (CryptoRNG m, MonadDB m, TemplatesMonad m) => (String, MailsConfig) -> Document -> SignatoryLinkID -> m ()
handleUndeliveredInvitation (hostpart, mc) doc signlinkid = do
  case getSigLinkFor doc signlinkid of
    Just signlink -> do
      time <- getMinutesTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetEmailInvitationDeliveryStatus (documentid doc) signlinkid Undelivered actor
      mail <- mailUndeliveredInvitation hostpart doc signlink
      scheduleEmailSendout mc $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink doc]
      }
    Nothing -> return ()

mailDeliveredInvitation :: TemplatesMonad m => String -> Document -> SignatoryLink -> m Mail
mailDeliveredInvitation hostpart doc signlink =
  kontramail "invitationMailDeliveredAfterDeferred" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "email" $ getEmail signlink
    F.value "documenttitle" $ documenttitle doc
    F.value "ctxhostpart" hostpart

mailDeferredInvitation :: TemplatesMonad m => String -> Document -> SignatoryLink -> m Mail
mailDeferredInvitation hostpart doc sl = kontramail "invitationMailDeferred" $ do
  F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
  F.value "counterpartname" $ getFullName sl
  F.value "counterpartemail" $ getEmail sl
  F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
  F.value "ctxhostpart" hostpart

mailUndeliveredInvitation :: TemplatesMonad m => String -> Document -> SignatoryLink -> m Mail
mailUndeliveredInvitation hostpart doc signlink =
  kontramail "invitationMailUndelivered" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "documenttitle" $ documenttitle doc
    F.value "email" $ getEmail signlink
    F.value "name" $ getFullName signlink
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" hostpart

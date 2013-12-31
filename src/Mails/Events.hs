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
import Doc.DocumentMonad (DocumentMonad, theDocument, withDocumentID)
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
import qualified Text.StringTemplates.Fields as F
import BrandedDomains
import Data.Functor
import Doc.DocViewMail

processEvents :: Scheduler ()
processEvents = dbQuery GetUnreadEvents >>= mapM_ processEvent
  where
    processEvent (eid, _mid, XSMTPAttrs [("mailinfo", mi)], eventType) = do
      case maybeRead mi of
        Just (Invitation docid signlinkid) -> do
          Log.mixlog_ $ "Processing invitation event: " ++ show (Invitation docid signlinkid)
          withDocumentID docid $ do
              markEventAsRead eid
              appConf <- sdAppConf <$> ask
              mbd <- (maybesignatory =<<) . getAuthorSigLink <$> theDocument >>= \case
                          Nothing -> return Nothing
                          Just uid -> do
                            user  <- dbQuery $ GetUserByID uid
                            return $ findBrandedDomain (fromMaybe "" $ join $ userassociateddomain <$> user) (brandedDomains $ appConf)
              msl <- getSigLinkFor signlinkid <$> theDocument
              let muid = maybe Nothing maybesignatory msl
              let signemail = maybe "" getEmail msl
              templates <- getGlobalTemplates
              let host = fromMaybe (hostpart $ appConf) (bdurl <$> mbd)
                  mc = mailsConfig $ appConf
                  -- since when email is reported deferred author has a possibility to
                  -- change email address, we don't want to send him emails reporting
                  -- success/failure for old signatory address, so we need to compare
                  -- addresses here (for dropped/bounce events)
                  handleEv (SendGridEvent email ev _) = do
                    Log.mixlog_ $ signemail ++ " == " ++ email
                    case ev of
                      SG_Opened -> handleOpenedInvitation signlinkid email muid
                      SG_Delivered _ -> handleDeliveredInvitation mbd host mc signlinkid
                      -- we send notification that email is reported deferred after
                      -- fifth attempt has failed - this happens after ~10 minutes
                      -- from sendout
                      SG_Deferred _ 5 -> handleDeferredInvitation mbd host mc signlinkid email
                      SG_Dropped _ -> when (signemail == email) $ handleUndeliveredInvitation mbd host mc signlinkid
                      SG_Bounce _ _ _ -> when (signemail == email) $ handleUndeliveredInvitation mbd host mc signlinkid
                      _ -> return ()
                  handleEv (MailGunEvent email ev) = do
                    Log.mixlog_ $ signemail ++ " == " ++ email
                    case ev of
                      MG_Opened -> handleOpenedInvitation signlinkid email muid
                      MG_Delivered -> handleDeliveredInvitation mbd host mc signlinkid
                      MG_Bounced _ _ _ -> when (signemail == email) $ handleUndeliveredInvitation mbd host mc signlinkid
                      MG_Dropped _ -> when (signemail == email) $ handleUndeliveredInvitation mbd host mc signlinkid
                      _ -> return ()
              theDocument >>= \doc -> runTemplatesT (getLang doc, templates) $ handleEv eventType
        _ -> markEventAsRead eid
    processEvent (eid, _ , _, _) = markEventAsRead eid

    markEventAsRead eid = do
      now <- getMinutesTime
      success <- dbUpdate $ MarkEventAsRead eid now
      when (not success) $
        Log.mixlog_ $ "Couldn't mark event #" ++ show eid ++ " as read"

handleDeliveredInvitation :: (CryptoRNG m, DocumentMonad m, TemplatesMonad m)
                          => Maybe BrandedDomain -> String -> MailsConfig -> SignatoryLinkID -> m ()
handleDeliveredInvitation mbd hostpart mc signlinkid = do
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just signlink -> do
      -- send it only if email was reported deferred earlier
      when (mailinvitationdeliverystatus signlink == Deferred) $ do
        mail <- mailDeliveredInvitation mc mbd hostpart signlink =<< theDocument
        theDocument >>= \d -> scheduleEmailSendout mc $ mail {
          to = [getMailAddress $ fromJust $ getAuthorSigLink d]
        }
      time <- getMinutesTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetEmailInvitationDeliveryStatus signlinkid Delivered actor
      return ()
    Nothing -> return ()

handleOpenedInvitation :: (DocumentMonad m, TemplatesMonad m, MonadIO m) => SignatoryLinkID -> String -> Maybe UserID -> m ()
handleOpenedInvitation signlinkid email muid = do
  now  <- getMinutesTime
  _ <- dbUpdate $ MarkInvitationRead signlinkid
          (mailSystemActor now muid email signlinkid)
  return ()

handleDeferredInvitation :: (CryptoRNG m, DocumentMonad m, TemplatesMonad m) => Maybe BrandedDomain -> String -> MailsConfig -> SignatoryLinkID -> String -> m ()
handleDeferredInvitation mbd hostpart mc signlinkid email = do
  time <- getMinutesTime
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just sl -> do
      let actor = mailSystemActor time (maybesignatory sl) email signlinkid
      success <- dbUpdate $ SetEmailInvitationDeliveryStatus signlinkid Deferred actor
      when success $ do
        mail <- mailDeferredInvitation mc mbd hostpart sl =<< theDocument
        theDocument >>= \d -> scheduleEmailSendout mc $ mail {
          to = [getMailAddress $ fromJust $ getAuthorSigLink d]
        }
    Nothing -> return ()

handleUndeliveredInvitation :: (CryptoRNG m, DocumentMonad m, TemplatesMonad m) => Maybe BrandedDomain -> String -> MailsConfig -> SignatoryLinkID -> m ()
handleUndeliveredInvitation mbd hostpart mc signlinkid = do
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just signlink -> do
      time <- getMinutesTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetEmailInvitationDeliveryStatus signlinkid Undelivered actor
      mail <- mailUndeliveredInvitation mc mbd hostpart signlink =<< theDocument
      theDocument >>= \d -> scheduleEmailSendout mc $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink d]
      }
    Nothing -> return ()

mailDeliveredInvitation :: TemplatesMonad m =>  MailsConfig -> Maybe BrandedDomain -> String -> SignatoryLink -> Document -> m Mail
mailDeliveredInvitation mc mbd hostpart signlink doc =
  kontramail mc mbd "invitationMailDeliveredAfterDeferred" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "email" $ getEmail signlink
    F.value "documenttitle" $ documenttitle doc
    F.value "ctxhostpart" hostpart
    brandingMailFields mbd Nothing


mailDeferredInvitation :: TemplatesMonad m => MailsConfig -> Maybe BrandedDomain -> String -> SignatoryLink -> Document -> m Mail
mailDeferredInvitation mc mbd hostpart sl doc = kontramail mc mbd "invitationMailDeferred" $ do
  F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
  F.value "counterpartname" $ getFullName sl
  F.value "counterpartemail" $ getEmail sl
  F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
  F.value "ctxhostpart" hostpart
  brandingMailFields mbd Nothing


mailUndeliveredInvitation :: TemplatesMonad m => MailsConfig -> Maybe BrandedDomain -> String -> SignatoryLink -> Document -> m Mail
mailUndeliveredInvitation mc mbd hostpart signlink doc =
  kontramail mc mbd "invitationMailUndelivered" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "documenttitle" $ documenttitle doc
    F.value "email" $ getEmail signlink
    F.value "name" $ getFullName signlink
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" hostpart
    brandingMailFields mbd Nothing

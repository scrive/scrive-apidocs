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
import Data.Maybe
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
import Doc.Model
import Doc.SignatoryLinkID
import KontraLink
import Mails.MailsConfig
import Mails.MailsData
import Mails.Model hiding (Mail)
import Mails.SendMail
import MinutesTime
import Templates
import Theme.Model
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Read
import qualified Log

processEvents :: Scheduler ()
processEvents = (take 50 <$> dbQuery GetUnreadEvents) >>= mapM_ processEvent -- We limit processing to 50 events not to have issues with large number of documents locked.
  where
    processEvent (eid, _mid, XSMTPAttrs [("mailinfo", mi)], eventType) = do
      case maybeRead mi of
        Just (Invitation docid signlinkid) -> do
          Log.mixlog_ $ "Processing invitation event: " ++ show (Invitation docid signlinkid)
          withDocumentID docid $ do
              markEventAsRead eid
              appConf <- sdAppConf <$> ask
              bd <- (maybesignatory =<<) . getAuthorSigLink <$> theDocument >>= \case
                          Nothing -> dbQuery $ GetMainBrandedDomain
                          Just uid -> do
                            dbQuery $ GetBrandedDomainByUserID uid
              msl <- getSigLinkFor signlinkid <$> theDocument
              let muid = maybe Nothing maybesignatory msl
              let signemail = maybe "" getEmail msl
              templates <- getGlobalTemplates
              let host = bdUrl $ bd
                  mc = mailsConfig $ appConf
                  -- since when email is reported deferred author has a possibility to
                  -- change email address, we don't want to send him emails reporting
                  -- success/failure for old signatory address, so we need to compare
                  -- addresses here (for dropped/bounce events)
                  handleEv (SendGridEvent email ev _) = do
                    Log.mixlog_ $ signemail ++ " == " ++ email
                    case ev of
                      SG_Opened -> handleOpenedInvitation signlinkid email muid
                      SG_Delivered _ -> handleDeliveredInvitation bd host mc signlinkid
                      -- we send notification that email is reported deferred after
                      -- fifth attempt has failed - this happens after ~10 minutes
                      -- from sendout
                      SG_Deferred _ 5 -> handleDeferredInvitation bd host mc signlinkid email
                      SG_Dropped _ -> when (signemail == email) $ handleUndeliveredInvitation bd host mc signlinkid
                      SG_Bounce _ _ _ -> when (signemail == email) $ handleUndeliveredInvitation bd host mc signlinkid
                      _ -> return ()
                  handleEv (MailGunEvent email ev) = do
                    Log.mixlog_ $ signemail ++ " == " ++ email
                    case ev of
                      MG_Opened -> handleOpenedInvitation signlinkid email muid
                      MG_Delivered -> handleDeliveredInvitation bd host mc signlinkid
                      MG_Bounced _ _ _ -> when (signemail == email) $ handleUndeliveredInvitation bd host mc signlinkid
                      MG_Dropped _ -> when (signemail == email) $ handleUndeliveredInvitation bd host mc signlinkid
                      _ -> return ()
              theDocument >>= \doc -> runTemplatesT (getLang doc, templates) $ handleEv eventType
        _ -> markEventAsRead eid
    processEvent (eid, _ , _, _) = markEventAsRead eid

    markEventAsRead eid = do
      now <- currentTime
      success <- dbUpdate $ MarkEventAsRead eid now
      when (not success) $
        Log.attention_ $ "Couldn't mark event #" ++ show eid ++ " as read"

handleDeliveredInvitation :: (CryptoRNG m, MonadThrow m, Log.MonadLog m, DocumentMonad m, TemplatesMonad m)
                          => BrandedDomain -> String -> MailsConfig -> SignatoryLinkID -> m ()
handleDeliveredInvitation bd hostpart mc signlinkid = do
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just signlink -> do
      -- send it only if email was reported deferred earlier
      when (mailinvitationdeliverystatus signlink == Deferred) $ do
        mail <- mailDeliveredInvitation bd hostpart signlink =<< theDocument
        theDocument >>= \d -> scheduleEmailSendout mc $ mail {
          to = [getMailAddress $ fromJust $ getAuthorSigLink d]
        }
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetEmailInvitationDeliveryStatus signlinkid Delivered actor
      return ()
    Nothing -> return ()

handleOpenedInvitation :: (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => SignatoryLinkID -> String -> Maybe UserID -> m ()
handleOpenedInvitation signlinkid email muid = do
  now  <- currentTime
  _ <- dbUpdate $ MarkInvitationRead signlinkid
          (mailSystemActor now muid email signlinkid)
  return ()

handleDeferredInvitation :: (CryptoRNG m, Log.MonadLog m, MonadThrow m, DocumentMonad m, TemplatesMonad m) => BrandedDomain -> String -> MailsConfig -> SignatoryLinkID -> String -> m ()
handleDeferredInvitation bd hostpart mc signlinkid email = do
  time <- currentTime
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just sl -> do
      let actor = mailSystemActor time (maybesignatory sl) email signlinkid
      success <- dbUpdate $ SetEmailInvitationDeliveryStatus signlinkid Deferred actor
      when success $ do
        mail <- mailDeferredInvitation bd hostpart sl =<< theDocument
        theDocument >>= \d -> scheduleEmailSendout mc $ mail {
          to = [getMailAddress $ fromJust $ getAuthorSigLink d]
        }
    Nothing -> return ()

handleUndeliveredInvitation :: (CryptoRNG m, MonadCatch m, Log.MonadLog m, DocumentMonad m, TemplatesMonad m) => BrandedDomain -> String -> MailsConfig -> SignatoryLinkID -> m ()
handleUndeliveredInvitation bd hostpart mc signlinkid = do
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just signlink -> do
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetEmailInvitationDeliveryStatus signlinkid Undelivered actor
      mail <- mailUndeliveredInvitation bd hostpart signlink =<< theDocument
      theDocument >>= \d -> scheduleEmailSendout mc $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink d]
      }
      triggerAPICallbackIfThereIsOne =<< theDocument
    Nothing -> return ()

mailDeliveredInvitation :: (TemplatesMonad m, MonadDB m, MonadThrow m) => BrandedDomain -> String -> SignatoryLink -> Document -> m Mail
mailDeliveredInvitation bd hostpart signlink doc =do
  theme <- dbQuery $ GetTheme $ bdMailTheme bd
  kontramail bd theme "invitationMailDeliveredAfterDeferred" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "email" $ getEmail signlink
    F.value "documenttitle" $ documenttitle doc
    F.value "ctxhostpart" hostpart
    brandingMailFields theme


mailDeferredInvitation ::(TemplatesMonad m, MonadDB m, MonadThrow m) => BrandedDomain -> String -> SignatoryLink -> Document -> m Mail
mailDeferredInvitation bd hostpart sl doc = do
  theme <- dbQuery $ GetTheme $ bdMailTheme bd
  kontramail bd theme"invitationMailDeferred" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "counterpartname" $ getFullName sl
    F.value "counterpartemail" $ getEmail sl
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" hostpart
    brandingMailFields theme


mailUndeliveredInvitation :: (TemplatesMonad m, MonadDB m, MonadThrow m) => BrandedDomain -> String -> SignatoryLink -> Document -> m Mail
mailUndeliveredInvitation bd hostpart signlink doc =do
  theme <- dbQuery $ GetTheme $ bdMailTheme bd
  kontramail bd theme "invitationMailUndelivered" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "documenttitle" $ documenttitle doc
    F.value "email" $ getEmail signlink
    F.value "name" $ getFullName signlink
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" hostpart
    brandingMailFields theme 

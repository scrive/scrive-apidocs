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
import Crypto.RNG
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
import Doc.DocumentID
import Doc.DocumentMonad (DocumentMonad, theDocument, withDocumentID)
import Doc.DocViewMail
import Doc.Logging
import Doc.Model
import Doc.SignatoryLinkID
import KontraLink
import Log.Identifier
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
  events <- take limit <$> dbQuery GetUnreadEvents
  forM_ events $ \event@(eid, mid, _) -> do
    localData [identifier eid, identifier mid] $ processEvent event
  return $ length events
  where
    processEvent (eid, mid, eventType) = do
      now <- currentTime
      mailNoreplyAddress <- asks ceMailNoreplyAddress
      mmailSendTimeStamp <- dbQuery $ GetEmailSendoutTime mid
      let timeDiff = case mmailSendTimeStamp of
                       Nothing -> Nothing
                       Just mailSendTimeStamp -> Just $ floor $ toRational $ mailSendTimeStamp `diffUTCTime` now
      templates <- asks ceTemplates
      mkontraInfoForMail <- dbQuery $ GetKontraInfoForMail mid
      case mkontraInfoForMail of
        Nothing -> markEventAsRead eid
        Just (DocumentInvitationMail docid slid) -> do
          handleEventInvitation docid slid eid timeDiff templates eventType mailNoreplyAddress
        Just (OtherDocumentMail docid) -> do
          handleEventOtherMail docid eid timeDiff templates eventType

markEventAsRead :: (MonadLog m, MonadThrow m, MonadDB m) => EventID -> m ()
markEventAsRead eid = do
  now <- currentTime
  success <- dbUpdate $ MarkEventAsRead eid now
  when (not success) $
    logAttention_ "Couldn't mark event as read"

handleEventInvitation
  :: (MonadLog m, CryptoRNG m, MonadCatch m, MonadDB m)
  => DocumentID -> SignatoryLinkID -> EventID -> Maybe Int -> GlobalTemplates -> Event -> String -> m ()
handleEventInvitation docid slid eid timeDiff templates eventType mailNoreplyAddress =
  logDocumentAndSignatory docid slid $ do
    exists <- dbQuery $ DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor docid
    if not exists
      then do
        logInfo_ "Email event for purged/non-existing document"
        markEventAsRead eid
    else do
      logInfo_ "Processing invitation event"
      withDocumentID docid $ do
        markEventAsRead eid
        bd <- (maybesignatory =<<) . getAuthorSigLink <$> theDocument >>= \case
                    Nothing -> dbQuery $ GetMainBrandedDomain
                    Just uid -> do
                      dbQuery $ GetBrandedDomainByUserID uid
        msl <- getSigLinkFor slid <$> theDocument
        let muid = maybe Nothing maybesignatory msl
        let signemail = maybe "" getEmail msl
        let logEmails email = logInfo "Comparing emails" $ object [
                  "signatory_email" .= signemail
                , "event_email" .= email
                ]
            -- since when email is reported deferred author has a possibility to
            -- change email address, we don't want to send him emails reporting
            -- success/failure for old signatory address, so we need to compare
            -- addresses here (for dropped/bounce events)
            handleEv (SendGridEvent email ev _) = do
              logEmails email
              case ev of
                SG_Opened -> handleOpenedInvitation slid email muid
                SG_Delivered _ -> handleDeliveredInvitation mailNoreplyAddress bd slid timeDiff
                -- we send notification that email is reported deferred after
                -- fifth attempt has failed - this happens after ~10 minutes
                -- from sendout
                SG_Deferred _ 5 -> handleDeferredInvitation mailNoreplyAddress bd slid email
                SG_Dropped _ -> when (signemail == email) $ handleUndeliveredInvitation mailNoreplyAddress bd slid
                SG_Bounce _ _ _ -> when (signemail == email) $ handleUndeliveredInvitation mailNoreplyAddress bd slid
                _ -> return ()
            handleEv (MailGunEvent email ev) = do
              logEmails email
              case ev of
                MG_Opened -> handleOpenedInvitation slid email muid
                MG_Delivered -> handleDeliveredInvitation mailNoreplyAddress bd slid timeDiff
                MG_Bounced _ _ _ -> when (signemail == email) $ handleUndeliveredInvitation mailNoreplyAddress bd slid
                MG_Dropped _ -> when (signemail == email) $ handleUndeliveredInvitation mailNoreplyAddress bd slid
                _ -> return ()
            handleEv (SocketLabsEvent email ev) = do
              logEmails email
              case ev of
                SL_Opened -> handleOpenedInvitation slid email muid
                SL_Delivered -> handleDeliveredInvitation mailNoreplyAddress bd slid timeDiff
                SL_Failed 0 5001 -> handleDeliveredInvitation mailNoreplyAddress bd slid timeDiff -- out of office/autoreply; https://support.socketlabs.com/index.php/Knowledgebase/Article/View/123
                SL_Failed _ _-> when (signemail == email) $ handleUndeliveredInvitation mailNoreplyAddress bd slid
                _ -> return ()
            handleEv (MailJetEvent email ev) = do
              logEmails email
              case ev of
                MJ_Open -> handleOpenedInvitation slid email muid
                MJ_Sent -> handleDeliveredInvitation mailNoreplyAddress bd slid timeDiff
                MJ_Bounce_Hard -> handleUndeliveredInvitation mailNoreplyAddress bd slid
                MJ_Blocked -> handleUndeliveredInvitation mailNoreplyAddress bd slid
                _ -> return ()
        theDocument >>= \doc -> runTemplatesT (getLang doc, templates) $ handleEv eventType

handleEventOtherMail
  :: (MonadLog m, MonadThrow m, MonadDB m)
  => DocumentID -> EventID -> Maybe Int -> GlobalTemplates -> Event -> m ()
handleEventOtherMail docid eid timeDiff templates eventType = logDocument docid $ do
  exists <- dbQuery $ DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor docid
  if not exists
    then do
      logInfo_ "Email event for purged/non-existing document"
      markEventAsRead eid
  else do
    logInfo_ "Processing related mail event"
    withDocumentID docid $ do
      let handleEv (SendGridEvent _ ev _) = case ev of
                                              SG_Delivered _ -> logDeliveryTime timeDiff
                                              _ -> return ()
          handleEv (MailGunEvent _ ev) = case ev of
                                           MG_Delivered -> logDeliveryTime timeDiff
                                           _ -> return ()
          handleEv (SocketLabsEvent _ ev) = case ev of
                                              SL_Delivered -> logDeliveryTime timeDiff
                                              SL_Failed 0 5001 -> logDeliveryTime timeDiff -- out of office/autoreply; https://support.socketlabs.com/index.php/Knowledgebase/Article/View/123
                                              _ -> return ()
          handleEv (MailJetEvent _ ev) = case ev of
                                              MJ_Sent -> logDeliveryTime timeDiff
                                              _ -> return ()
      -- no templates are used here, so runTemplatesT is not necessary, right? XXX
      theDocument >>= \doc -> runTemplatesT (getLang doc, templates) $ handleEv eventType
      markEventAsRead eid

logDeliveryTime :: (DocumentMonad m, MonadLog m, MonadThrow m) => Maybe Int -> m ()
logDeliveryTime timeDiff = theDocument >>= \d -> do
  logInfo "Email delivered" $ object [
      identifier $ documentauthorugid d
    , "delivery_time" .= timeDiff
    ]

handleDeliveredInvitation :: (CryptoRNG m, MonadThrow m, MonadLog m, DocumentMonad m, TemplatesMonad m)
                          => String -> BrandedDomain -> SignatoryLinkID -> Maybe Int -> m ()
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
      _ <- dbUpdate $ SetEmailInvitationDeliveryStatus slid Delivered actor
      return ()
    Nothing -> return ()

handleOpenedInvitation :: (DocumentMonad m, TemplatesMonad m, MonadThrow m, MonadTime m) => SignatoryLinkID -> String -> Maybe UserID -> m ()
handleOpenedInvitation slid email muid = do
  now  <- currentTime
  _ <- dbUpdate $ MarkInvitationRead slid
          (mailSystemActor now muid email slid)
  return ()

handleDeferredInvitation :: (CryptoRNG m, MonadLog m, MonadThrow m, DocumentMonad m, TemplatesMonad m) => String -> BrandedDomain -> SignatoryLinkID -> String -> m ()
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

handleUndeliveredInvitation :: (CryptoRNG m, MonadCatch m, MonadLog m, DocumentMonad m, TemplatesMonad m) => String -> BrandedDomain -> SignatoryLinkID -> m ()
handleUndeliveredInvitation mailNoreplyAddress bd slid = do
  getSigLinkFor slid <$> theDocument >>= \case
    Just signlink | mailinvitationdeliverystatus signlink == Delivered -> do
      logInfo "Undelivered email event for email that was already delivered" $ object ["signatory_email" .= getEmail signlink]
                  | otherwise -> do
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) slid
      _ <- dbUpdate $ SetEmailInvitationDeliveryStatus slid Undelivered actor
      mail <- mailUndeliveredInvitation mailNoreplyAddress bd signlink =<< theDocument
      theDocument >>= \d -> scheduleEmailSendout $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink d]
      }
      triggerAPICallbackIfThereIsOne =<< theDocument
    Nothing -> return ()

mailDeliveredInvitation :: (TemplatesMonad m, MonadDB m, MonadThrow m) => String -> BrandedDomain -> SignatoryLink -> Document -> m Mail
mailDeliveredInvitation mailNoreplyAddress bd signlink doc =do
  theme <- dbQuery $ GetTheme $ get bdMailTheme bd
  kontramail mailNoreplyAddress bd theme "invitationMailDeliveredAfterDeferred" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "email" $ getEmail signlink
    F.value "documenttitle" $ documenttitle doc
    F.value "ctxhostpart" $ get bdUrl bd
    brandingMailFields theme


mailDeferredInvitation ::(TemplatesMonad m, MonadDB m, MonadThrow m) => String -> BrandedDomain -> SignatoryLink -> Document -> m Mail
mailDeferredInvitation mailNoreplyAddress bd sl doc = do
  theme <- dbQuery $ GetTheme $ get bdMailTheme bd
  kontramail mailNoreplyAddress bd theme"invitationMailDeferred" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "counterpartname" $ getFullName sl
    F.value "counterpartemail" $ getEmail sl
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" $ get bdUrl bd
    brandingMailFields theme


mailUndeliveredInvitation :: (TemplatesMonad m, MonadDB m, MonadThrow m) => String -> BrandedDomain -> SignatoryLink -> Document -> m Mail
mailUndeliveredInvitation mailNoreplyAddress bd signlink doc =do
  theme <- dbQuery $ GetTheme $ get bdMailTheme bd
  kontramail mailNoreplyAddress bd theme "invitationMailUndelivered" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "documenttitle" $ documenttitle doc
    F.value "email" $ getEmail signlink
    F.value "name" $ getFullName signlink
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" $ get bdUrl bd
    brandingMailFields theme

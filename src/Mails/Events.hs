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

--import Control.Applicative
import Data.Maybe
--import Data.List
import Control.Monad.Reader
import qualified Data.ByteString.UTF8 as BS

import AppConf
import ActionScheduler
import ActionSchedulerState
import DB.Classes
import Doc.Model
import Doc.DocStateData hiding (MailsDeliveryStatus(..))
import KontraLink
import Mails.MailsConfig
import Mails.MailsData
import Mails.Model hiding (Mail)
import Mails.SendMail
import MinutesTime
import Misc
import Templates.Templates
import Templates.TemplatesUtils
import Templates.Trans
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Doc.DocStateData as D
import qualified Log
import EvidenceLog.Model

processEvents :: ActionScheduler ()
processEvents = runDBQuery GetUnreadEvents >>= mapM_ processEvent
  where
    processEvent (eid, mid, XSMTPAttrs [("mailinfo", mi)], SendGridEvent email ev _) = do
      markEventAsRead eid  
      case maybeRead mi of
        Just (Invitation docid signlinkid) -> do
          mdoc <- runDBQuery $ GetDocumentByDocumentID docid
          case mdoc of
            Nothing -> do
              Log.debug $ "No document with id = " ++ show docid
              deleteEmail mid
            Just doc -> do
              let msl = getSigLinkFor doc signlinkid
                  muid = maybe Nothing maybesignatory msl
              let signemail = maybe "" (BS.toString . getEmail) msl
              Log.debug $ signemail ++ " == " ++ email
              sd <- ask
              templates <- getGlobalTemplates
              let host = hostpart $ sdAppConf sd
                  mc = sdMailsConfig sd
              -- since when email is reported deferred author has a possibility to change
              -- email address, we don't want to send him emails reporting success/failure
              -- for old signatory address, so we need to compare addresses here.
              runTemplatesT (getLocale doc, templates) $ case ev of
                Opened       -> handleOpenedInvitation docid signlinkid email muid
                Delivered _  -> handleDeliveredInvitation mc doc signlinkid
                -- we send notification that email is reported deferred after fifth
                -- attempt has failed - this happens after ~10 minutes from sendout
                Deferred _ 5 -> handleDeferredInvitation (host, mc) doc signlinkid email
                Dropped _    -> when (signemail == email) $  handleUndeliveredInvitation (host, mc) doc signlinkid
                Bounce _ _ _ -> when (signemail == email) $  handleUndeliveredInvitation (host, mc) doc signlinkid
                _            -> return ()
        _ -> return ()
    processEvent (eid, _ , _, _) = markEventAsRead eid

    markEventAsRead eid = do
      now <- getMinutesTime
      success <- runDBUpdate $ MarkEventAsRead eid now
      when (not success) $
        Log.error $ "Couldn't mark event #" ++ show eid ++ " as read"

    deleteEmail :: DBMonad m => MailID -> m ()
    deleteEmail mid = do
      success <- runDBUpdate $ DeleteEmail mid
      if (not success) 
        then Log.error $ "Couldn't delete email #" ++ show mid
        else Log.debug $ "Deleted email #" ++ show mid

handleDeliveredInvitation :: (DBMonad m, TemplatesMonad m) => MailsConfig -> Document -> SignatoryLinkID -> m ()
handleDeliveredInvitation mc doc signlinkid = do
  case getSigLinkFor doc signlinkid of
    Just signlink -> do
      -- send it only if email was reported deferred earlier
      when (invitationdeliverystatus signlink == D.Deferred) $ do
        mail <- mailDeliveredInvitation doc signlink
        scheduleEmailSendout mc $ mail {
          to = [getMailAddress $ fromJust $ getAuthorSigLink doc]
        }
      time <- getMinutesTime
      let actor = MailSystemActor time (maybesignatory signlink) (BS.toString $ getEmail signlink) signlinkid
      _ <- runDBUpdate $ SetInvitationDeliveryStatus (documentid doc) signlinkid D.Delivered actor
      return ()
    Nothing -> return ()

handleOpenedInvitation :: DBMonad m => DocumentID -> SignatoryLinkID -> String -> Maybe UserID -> m ()
handleOpenedInvitation docid signlinkid email muid = do
  now <- getMinutesTime
  _   <- runDBUpdate $ MarkInvitationRead docid signlinkid 
         (MailSystemActor now muid email signlinkid)
  return ()

handleDeferredInvitation :: (DBMonad m, TemplatesMonad m) => (String, MailsConfig) -> Document -> SignatoryLinkID -> String -> m ()
handleDeferredInvitation (hostpart, mc) doc signlinkid email = do
  time <- getMinutesTime
  case getSigLinkFor doc signlinkid of
    Just sl -> do
      let actor = MailSystemActor time (maybesignatory sl) email signlinkid
      mdoc <- runDBUpdate $ SetInvitationDeliveryStatus (documentid doc) signlinkid D.Deferred actor
      case mdoc of
        Right doc' -> do
          mail <- mailDeferredInvitation hostpart doc'
          scheduleEmailSendout mc $ mail {
            to = [getMailAddress $ fromJust $ getAuthorSigLink doc']
            }
        Left _ -> return ()
    Nothing -> return ()


handleUndeliveredInvitation :: (DBMonad m, TemplatesMonad m) => (String, MailsConfig) -> Document -> SignatoryLinkID -> m ()
handleUndeliveredInvitation (hostpart, mc) doc signlinkid = do
  case getSigLinkFor doc signlinkid of
    Just signlink -> do
      time <- getMinutesTime
      let actor = MailSystemActor time (maybesignatory signlink) (BS.toString $ getEmail signlink) signlinkid
      _ <- runDBUpdate $ SetInvitationDeliveryStatus (documentid doc) signlinkid D.Undelivered actor
      mail <- mailUndeliveredInvitation hostpart doc signlink
      scheduleEmailSendout mc $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink doc]
      }
    Nothing -> return ()

mailDeliveredInvitation :: TemplatesMonad m =>  Document -> SignatoryLink -> m Mail
mailDeliveredInvitation doc signlink =
  kontramail "invitationMailDeliveredAfterDeferred" $ do
    field "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    field "email" $ getEmail signlink
    field "documenttitle" $ BS.toString $ documenttitle doc

mailDeferredInvitation :: TemplatesMonad m => String -> Document -> m Mail
mailDeferredInvitation hostpart doc = kontramail "invitationMailDeferred" $ do
  field "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
  field "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
  field "ctxhostpart" hostpart

mailUndeliveredInvitation :: TemplatesMonad m => String -> Document -> SignatoryLink -> m Mail
mailUndeliveredInvitation hostpart doc signlink =
  kontramail "invitationMailUndelivered" $ do
    field "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    field "documenttitle" $ documenttitle doc
    field "email" $ getEmail signlink
    field "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    field "ctxhostpart" hostpart

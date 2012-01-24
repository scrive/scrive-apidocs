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

import Control.Applicative
import Data.Maybe
import Data.List
import Control.Monad.Reader
import qualified Data.ByteString.UTF8 as BS

import AppConf
import ActionScheduler
import ActionSchedulerState
import DB.Classes
import Doc.Model
import Doc.DocStateData
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
import qualified Log
import qualified Mails.MailsUtil as Mail

processEvents :: ActionScheduler ()
processEvents = runDBQuery GetUnreadEvents >>= mapM_ processEvent
  where
    processEvent (eid, mid, XSMTPAttrs [("mailinfo", mi)], SendGridEvent email ev _) = do
      markEventAsRead eid  
      case maybeRead mi of
        Just (Invitation docid signlinkid) -> do
          mdoc <- (runDB . dbQuery) $ GetDocumentByDocumentID docid
          case mdoc of
            Nothing -> do
              Log.debug $ "No document with id = " ++ show docid
              deleteEmail mid
            Just doc -> do
              let signemail = fromMaybe "" (BS.toString . getEmail <$> getSignatoryLinkFromDocumentByID doc signlinkid)
              Log.debug $ signemail ++ " == " ++ email
              sd <- ask
              templates <- getGlobalTemplates
              let host = hostpart $ sdAppConf sd
                  mc = sdMailsConfig sd
              -- since when email is reported deferred author has a possibility to change
              -- email address, we don't want to send him emails reporting success/failure
              -- for old signatory address, so we need to compare addresses here.
              runTemplatesT (getLocale doc, templates) $ case ev of
                Opened       -> handleOpenedInvitation docid signlinkid
                Delivered _  -> handleDeliveredInvitation mc doc signlinkid
                -- we send notification that email is reported deferred after fifth
                -- attempt has failed - this happens after ~10 minutes from sendout
                Deferred _ 5 -> handleDeferredInvitation (host, mc) docid signlinkid
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
  case getSignatoryLinkFromDocumentByID doc signlinkid of
    Just signlink -> do
      -- send it only if email was reported deferred earlier
      when (invitationdeliverystatus signlink == Mail.Deferred) $ do
        mail <- mailDeliveredInvitation doc signlink
        scheduleEmailSendout mc $ mail {
          to = [getMailAddress $ fromJust $ getAuthorSigLink doc]
        }
    Nothing -> return ()
  _ <- (runDB . dbUpdate) $ SetInvitationDeliveryStatus (documentid doc) signlinkid Mail.Delivered
  return ()

handleOpenedInvitation :: DBMonad m => DocumentID -> SignatoryLinkID -> m ()
handleOpenedInvitation docid signlinkid = do
  now <- getMinutesTime
  _ <- (runDB . dbUpdate) $ MarkInvitationRead docid signlinkid now
  return ()

handleDeferredInvitation :: (DBMonad m, TemplatesMonad m) => (String, MailsConfig) -> DocumentID -> SignatoryLinkID -> m ()
handleDeferredInvitation (hostpart, mc) docid signlinkid = do
  mdoc <- (runDB . dbUpdate) $ SetInvitationDeliveryStatus docid signlinkid Mail.Deferred
  case mdoc of
    Right doc -> do
      mail <- mailDeferredInvitation hostpart doc
      scheduleEmailSendout mc $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink doc]
      }
    Left _ -> return ()

handleUndeliveredInvitation :: (DBMonad m, TemplatesMonad m) => (String, MailsConfig) -> Document -> SignatoryLinkID -> m ()
handleUndeliveredInvitation (hostpart, mc) doc signlinkid = do
  case getSignatoryLinkFromDocumentByID doc signlinkid of
    Just signlink -> do
      _ <- (runDB . dbUpdate) $ SetInvitationDeliveryStatus (documentid doc) signlinkid Mail.Undelivered
      mail <- mailUndeliveredInvitation hostpart doc signlink
      scheduleEmailSendout mc $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink doc]
      }
    Nothing -> return ()

getSignatoryLinkFromDocumentByID :: Document -> SignatoryLinkID -> Maybe SignatoryLink
getSignatoryLinkFromDocumentByID Document{documentsignatorylinks} signlinkid =
  find ((==) signlinkid . signatorylinkid) documentsignatorylinks

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

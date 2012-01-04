{-# OPTIONS_GHC -w #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Mails.SendGridEvents
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Sendgrid events interface. 'handleSendgridEvent' is used when sendgrid contacts us.
-- mailinfo param is set when we are sending mails.
-----------------------------------------------------------------------------
module Mails.Events (
    processEvents
  , mailDeliveredInvitation
  , mailDeferredInvitation
  , mailUndeliveredInvitation
  ) where

import Control.Applicative
import Kontra
import Control.Monad.Trans
import KontraLink
import Misc
import Data.Maybe
import qualified Mails.MailsUtil as Mail
import Doc.Transitory
import Doc.DocStateData
import ActionSchedulerState
import Happstack.State
import Mails.SendMail
import Mails.MailsData
import Templates.Templates
import Templates.TemplatesUtils
import Control.Monad.State
import qualified Data.ByteString.UTF8 as BS
import Data.List
import qualified AppLogger as Log
import MinutesTime
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Util.MonadUtils
import User.Model
import Happstack.Server
import DB.Classes
import ActionScheduler
import Mails.Public

processEvents :: ActionScheduler ()
processEvents = return ()

{-
processEvents :: ActionScheduler ()
processEvents = runDBQuery GetEvents >>= mapM_ processEvent
  where
    processEvent (eid, mid, XSMTPAttrs [("mailinfo", mi)], SendGridEvent email ev _) = do
      case maybeRead mi of
        Nothing -> Log.error $ "Invalid MailInfo object received: " ++ show mi
        Just (Invitation docid signlinkid) -> do
          mdoc <- doc_query $ GetDocumentByDocumentID docid
          case mdoc of
            Nothing -> Log.debug $ "No document with id = " ++ show docid
            Just doc -> do
              let signemail = fromMaybe "" (BS.toString . getEmail <$> getSignatoryLinkFromDocumentByID doc signlinkid)
              Log.debug $ signemail ++ " == " ++ email
              -- since when email is reported deferred author has a possibility to change
              -- email address, we don't want to send him emails reporting success/failure
              -- for old signatory address, so we need to compare addresses here.
              case ev of
                Opened       -> handleOpenedInvitation docid signlinkid
                Dropped _    -> if signemail == email
                                    then handleUndeliveredInvitation docid signlinkid
                                    else return ()
                Delivered _  -> handleDeliveredInvitation docid signlinkid
                -- we send notification that email is reported deferred after fifth
                -- attempt has failed - this happens after ~10 minutes from sendout
                Deferred _ 5 -> handleDeferredInvitation docid signlinkid
                Bounce _ _ _ -> if signemail == email
                                    then handleUndeliveredInvitation docid signlinkid
                                    else return ()
                _            -> return ()
        Just _ -> deleteEmail mid
    processEvent (_, mid, _, _) = deleteEmail mid

    deleteEmail mid = do
      success <- runDBUpdate $ DeleteEmail mid
      when (not success) $ Log.error $ "Couldn't delete email #" ++ show mid
      -}
{-
-- | Main routing table after getting sendgrid event.
routeToHandler :: (Kontrakcja m, DBMonad m) => SendgridEvent -> m ()
routeToHandler (SendgridEvent {mailAddress, info = Invitation docid signlinkid, event}) = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    let signemail = fromMaybe "" (BS.toString . getEmail <$> getSignatoryLinkFromDocumentByID doc signlinkid)
    Log.debug $ signemail ++ " == " ++ mailAddress
    -- since when email is reported deferred author has a possibility to change
    -- email address, we don't want to send him emails reporting success/failure
    -- for old signatory address, so we need to compare addresses here.
    case event of
         Opened       -> handleOpenedInvitation docid signlinkid
         Dropped _    -> if signemail == mailAddress
                            then handleUndeliveredInvitation docid signlinkid
                            else return ()
         Delivered _  -> handleDeliveredInvitation docid signlinkid
         -- we send notification that email is reported deferred after fifth
         -- attempt has failed - this happens after ~10 minutes from sendout
         Deferred _ 5 -> handleDeferredInvitation docid signlinkid
         Bounce _ _ _ -> if signemail == mailAddress
                            then handleUndeliveredInvitation docid signlinkid
                            else return ()
         _            -> return ()
routeToHandler _ = return ()
-}
-- | Actions perform that are performed then
handleDeliveredInvitation :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m ()
handleDeliveredInvitation docid signlinkid = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    case getSignatoryLinkFromDocumentByID doc signlinkid of
         Just signlink -> do
             -- send it only if email was reported deferred earlier
             when (invitationdeliverystatus signlink == Mail.Deferred) $ do
                 ctx <- getContext
                 mail <- mailDeliveredInvitation doc signlink
                 scheduleEmailSendout (ctxmailsconfig ctx) $ mail { to = [getMailAddress $ fromJust $ getAuthorSigLink doc] }
         Nothing -> return ()
    _ <- doc_update $ SetInvitationDeliveryStatus docid signlinkid Mail.Delivered
    return ()

handleOpenedInvitation :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m ()
handleOpenedInvitation docid signlinkid = do
    now <- liftIO $ getMinutesTime
    _ <- doc_update $ MarkInvitationRead docid signlinkid now
    return ()

handleDeferredInvitation :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m ()
handleDeferredInvitation docid signlinkid = do
    mdoc <- doc_update $ SetInvitationDeliveryStatus docid signlinkid Mail.Deferred
    case mdoc of
         Right doc -> do
             ctx <- getContext
             mail <- mailDeferredInvitation (ctxhostpart ctx) doc
             scheduleEmailSendout (ctxmailsconfig ctx) $ mail {  to = [getMailAddress $ fromJust $ getAuthorSigLink doc] }
         Left _ -> return ()

handleUndeliveredInvitation :: Kontrakcja m => DocumentID -> SignatoryLinkID -> m ()
handleUndeliveredInvitation docid signlinkid = do
    doc <- queryOrFail $ GetDocumentByDocumentID docid
    ctx <- getContext
    case getSignatoryLinkFromDocumentByID doc signlinkid of
         Just signlink -> do
             _ <- doc_update $ SetInvitationDeliveryStatus docid signlinkid Mail.Undelivered
             mail <- mailUndeliveredInvitation (ctxhostpart ctx) doc signlink
             scheduleEmailSendout (ctxmailsconfig ctx) $ mail {  to = [getMailAddress $ fromJust $ getAuthorSigLink doc]  }
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

getSignatoryLinkFromDocumentByID :: Document -> SignatoryLinkID -> Maybe SignatoryLink
getSignatoryLinkFromDocumentByID Document{documentsignatorylinks} signlinkid =
    find ((==) signlinkid . signatorylinkid) documentsignatorylinks

-----------------------------------------------------------------------------
-- |
-- Module      :  SMS.Events
-- Maintainer  :  gracjan@scrive.com
-- Stability   :  development
-- Portability :  portable
--
-- Interface for passing delivered/undelivered events for SMS
-----------------------------------------------------------------------------
module SMS.Events (
    processEvents
  , smsUndeliveredInvitation
  ) where

import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Base

import AppConf
import ActionQueue.Scheduler
import Crypto.RNG
import DB
import Doc.Model
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID, withDocument)
import KontraLink
import SMS.Model
import SMS.Data
import Mails.SendMail
import Mails.MailsConfig
import MinutesTime
import Text.StringTemplates.Templates hiding (runTemplatesT)
import Templates
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Doc.SignatoryLinkID
import qualified Log
import Util.Actor
import qualified Text.StringTemplates.Fields as F
import Utils.Read
import Data.Functor
import BrandedDomains
import Doc.DocViewMail

processEvents :: Scheduler ()
processEvents = dbQuery GetUnreadSMSEvents >>= mapM_ (\(a,b,c,d) -> processEvent (a,b,c, fromMaybe None $ maybeRead d))
  where
    processEvent (eid, smsid, eventType, Invitation _did slid ) = do

      Log.mixlog_ $ "Messages.procesEvent: " ++ show (eid, smsid, eventType, slid)

      dbQuery (GetDocumentBySignatoryLinkID slid) >>= \case
        Nothing -> do
          _ <- dbUpdate $ MarkSMSEventAsRead eid
          Log.mixlog_ $ "No document with signatory link id = " ++ show slid
          deleteSMS smsid
        Just doc' -> withDocument doc' $ do
          _ <- dbUpdate $ MarkSMSEventAsRead eid
          msl <- getSigLinkFor slid <$> theDocument
          let signphone = maybe "" getMobile msl
          templates <- getGlobalTemplates
          appConf <- sdAppConf <$> ask
          mbd <- (maybesignatory =<<) . getAuthorSigLink <$> theDocument >>= \case
                          Nothing -> return Nothing
                          Just uid -> do
                            user  <- dbQuery $ GetUserByID uid
                            return $ findBrandedDomain (fromMaybe "" $ join $ userassociateddomain <$> user) (brandedDomains $ appConf)
          let host = fromMaybe (hostpart $ appConf) (bdurl <$> mbd)
              mc = mailsConfig $ appConf
              -- since when email is reported deferred author has a possibility to
              -- change email address, we don't want to send him emails reporting
              -- success/failure for old signatory address, so we need to compare
              -- addresses here (for dropped/bounce events)
              handleEv (SMSEvent phone ev) = do
                Log.mixlog_ $ signphone ++ " == " ++ phone
                theDocument >>= \doc -> runTemplatesT (getLang doc, templates) $ case ev of
                  SMSDelivered -> handleDeliveredInvitation slid
                  SMSUndelivered _ -> when (signphone == phone) $ handleUndeliveredSMSInvitation mbd host mc slid
          handleEv eventType
    processEvent (eid, _ , _, _) = do
      _ <- dbUpdate $ MarkSMSEventAsRead eid
      return ()

    deleteSMS :: (MonadDB m, Log.MonadLog m) => ShortMessageID -> m ()
    deleteSMS mid = do
      success <- dbUpdate $ DeleteSMS mid
      if (not success)
        then Log.attention_ $ "Couldn't delete sms #" ++ show mid
        else Log.mixlog_ $ "Deleted sms #" ++ show mid

handleDeliveredInvitation :: (CryptoRNG m, Log.MonadLog m, DocumentMonad m, TemplatesMonad m) => SignatoryLinkID -> m ()
handleDeliveredInvitation signlinkid = do
  theDocumentID >>= \did -> Log.mixlog_ $ "handleDeliveredInvitation: docid=" ++ show did ++ ", siglinkid=" ++ show signlinkid
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just signlink -> do
      time <- getMinutesTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetSMSInvitationDeliveryStatus signlinkid Delivered actor
      return ()
    Nothing -> return ()

handleUndeliveredSMSInvitation :: (CryptoRNG m, Log.MonadLog m, DocumentMonad m, TemplatesMonad m, MonadBase IO m) => Maybe BrandedDomain -> String -> MailsConfig -> SignatoryLinkID -> m ()
handleUndeliveredSMSInvitation mbd hostpart mc signlinkid = do
  theDocumentID >>= \did -> Log.mixlog_ $ "handleUndeliveredSMSInvitation: docid=" ++ show did ++ ", siglinkid=" ++ show signlinkid
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just signlink -> do
      time <- getMinutesTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetSMSInvitationDeliveryStatus signlinkid Undelivered actor
      mail <- theDocument >>= \d -> smsUndeliveredInvitation mc mbd hostpart d signlink
      theDocument >>= \d -> scheduleEmailSendout mc $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink d]
      }
    Nothing -> return ()

smsUndeliveredInvitation :: TemplatesMonad m => MailsConfig -> Maybe BrandedDomain -> String -> Document -> SignatoryLink -> m Mail
smsUndeliveredInvitation mc mbd hostpart doc signlink =
  kontramail mc mbd "invitationSMSUndelivered" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "documenttitle" $ documenttitle doc
    F.value "email" $ getEmail signlink
    F.value "name" $ getFullName signlink
    F.value "mobile" $ getMobile signlink
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" hostpart
    brandingMailFields mbd Nothing

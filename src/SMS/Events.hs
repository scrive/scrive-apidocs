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
import KontraLink
import SMS.Model
import SMS.Data
import Mails.SendMail
import Mails.MailsConfig
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
      _ <- dbUpdate $ MarkSMSEventAsRead eid

      Log.cron $ "Messages.procesEvent: " ++ show (eid, smsid, eventType, slid)

      mdoc <- dbQuery $ GetDocumentBySignatoryLinkID slid
      case mdoc of
        Nothing -> do
          Log.cron $ "No document with signatory link id = " ++ show slid
          deleteSMS smsid
        Just doc -> do
          let msl = getSigLinkFor doc slid
          let signphone = maybe "" getMobile msl
          templates <- getGlobalTemplates
          appConf <- sdAppConf <$> ask
          mbd <- case (maybesignatory =<< getAuthorSigLink doc) of
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
                Log.cron $ signphone ++ " == " ++ phone
                runTemplatesT (getLang doc, templates) $ case ev of
                  SMSDelivered -> handleDeliveredInvitation doc slid
                  SMSUndelivered _ -> when (signphone == phone) $ handleUndeliveredSMSInvitation mbd host mc doc slid
          handleEv eventType
    processEvent (eid, _ , _, _) = do
      _ <- dbUpdate $ MarkSMSEventAsRead eid
      return ()

    deleteSMS :: MonadDB m => ShortMessageID -> m ()
    deleteSMS mid = do
      success <- dbUpdate $ DeleteSMS mid
      if (not success)
        then Log.error $ "Couldn't delete email #" ++ show mid
        else Log.debug $ "Deleted email #" ++ show mid

handleDeliveredInvitation :: (CryptoRNG m, MonadDB m, TemplatesMonad m) => Document -> SignatoryLinkID -> m ()
handleDeliveredInvitation doc signlinkid = do
  Log.cron $ "handleDeliveredInvitation: docid=" ++ show (documentid doc) ++ ", siglinkid=" ++ show signlinkid
  case getSigLinkFor doc signlinkid of
    Just signlink -> do
      time <- getMinutesTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetSMSInvitationDeliveryStatus (documentid doc) signlinkid Delivered actor
      return ()
    Nothing -> return ()

handleUndeliveredSMSInvitation :: (CryptoRNG m, MonadDB m, TemplatesMonad m, MonadBase IO m) => Maybe BrandedDomain -> String -> MailsConfig -> Document -> SignatoryLinkID -> m ()
handleUndeliveredSMSInvitation mbd hostpart mc doc signlinkid = do
  Log.cron $ "handleUndeliveredSMSInvitation: docid=" ++ show (documentid doc) ++ ", siglinkid=" ++ show signlinkid
  case getSigLinkFor doc signlinkid of
    Just signlink -> do
      time <- getMinutesTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetSMSInvitationDeliveryStatus (documentid doc) signlinkid Undelivered actor
      mail <- smsUndeliveredInvitation mc mbd hostpart doc signlink
      scheduleEmailSendout mc $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink doc]
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

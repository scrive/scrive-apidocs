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

import Control.Monad.Base
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
import Crypto.RNG
import DB
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument, theDocumentID, withDocument)
import Doc.DocViewMail
import Doc.Model
import Doc.SignatoryLinkID
import EvidenceLog.Model
import KontraLink
import Mails.MailsConfig
import Mails.SendMail
import MinutesTime
import SMS.Data
import SMS.Model
import Templates
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Default (defaultValue)
import Utils.Read
import qualified Log

processEvents :: Scheduler ()
processEvents = dbQuery GetUnreadSMSEvents >>= mapM_ (\(a,b,c,d) -> processEvent (a,b,c, fromMaybe None $ maybeRead d))
  where
    processEvent (eid, smsid, eventType, Invitation _did slid ) = do
      Log.mixlog_ $ "Messages.procesEvent: " ++ show (eid, smsid, eventType, slid)
      dbQuery (GetDocumentBySignatoryLinkID slid) >>= \doc' -> withDocument doc' $ do
        _ <- dbUpdate $ MarkSMSEventAsRead eid
        msl <- getSigLinkFor slid <$> theDocument
        let signphone = maybe "" getMobile msl
        templates <- getGlobalTemplates
        appConf <- sdAppConf <$> ask
        mbd <- (maybesignatory =<<) . getAuthorSigLink <$> theDocument >>= \case
          Nothing -> return Nothing
          Just uid -> dbQuery $ GetBrandedDomainByUserID uid
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
                SMSUndelivered _ -> when (signphone == phone) $ do
                  handleUndeliveredSMSInvitation mbd host mc slid
        handleEv eventType

    processEvent (eid, smsid, eventType, SMSPinSendout slid) = do
      Log.mixlog_ $ "Messages.procesEvent: " ++ show (eid, smsid, eventType, slid)
      dbQuery (GetDocumentBySignatoryLinkID slid) >>= \doc' -> withDocument doc' $ do
        _ <- dbUpdate $ MarkSMSEventAsRead eid
        templates <- getGlobalTemplates
        msl <- getSigLinkFor slid <$> theDocument
        case (eventType,msl) of
          (SMSEvent phone SMSDelivered, Just sl) -> runTemplatesT (defaultValue, templates) $ do
            Log.mixlog_ $ "SMS with PIN delivered to " ++ phone
            time <- currentTime
            let actor = mailSystemActor time (maybesignatory sl) (getEmail sl) slid
            void $ dbUpdate $ InsertEvidenceEventWithAffectedSignatoryAndMsg
              SMSPinDeliveredEvidence
              (return ())
              (Just sl)
              (Just phone)
              (actor)
          _ -> return ()

    processEvent (eid, _ , _, _) = do
      _ <- dbUpdate $ MarkSMSEventAsRead eid
      return ()

handleDeliveredInvitation :: (CryptoRNG m, MonadThrow m, Log.MonadLog m, DocumentMonad m, TemplatesMonad m) => SignatoryLinkID -> m ()
handleDeliveredInvitation signlinkid = do
  theDocumentID >>= \did -> Log.mixlog_ $ "handleDeliveredInvitation: docid=" ++ show did ++ ", siglinkid=" ++ show signlinkid
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just signlink -> do
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetSMSInvitationDeliveryStatus signlinkid Delivered actor
      return ()
    Nothing -> return ()

handleUndeliveredSMSInvitation :: (CryptoRNG m, MonadThrow m, Log.MonadLog m, DocumentMonad m, TemplatesMonad m, MonadBase IO m) => Maybe BrandedDomain -> String -> MailsConfig -> SignatoryLinkID -> m ()
handleUndeliveredSMSInvitation mbd hostpart mc signlinkid = do
  theDocumentID >>= \did -> Log.mixlog_ $ "handleUndeliveredSMSInvitation: docid=" ++ show did ++ ", siglinkid=" ++ show signlinkid
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just signlink -> do
      time <- currentTime
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

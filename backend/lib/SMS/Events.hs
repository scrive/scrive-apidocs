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
import Crypto.RNG
import Data.Functor
import Log
import Text.StringTemplates.Templates hiding (runTemplatesT)
import qualified Text.StringTemplates.Fields as F

import ActionQueue.Scheduler
import BrandedDomain.BrandedDomain
import BrandedDomain.Model
import DB
import Doc.API.Callback.Model
import Doc.DocStateData
import Doc.DocumentMonad
import Doc.DocViewMail
import Doc.Logging
import Doc.Model
import Doc.SignatoryLinkID
import EvidenceLog.Model
import KontraLink
import KontraPrelude
import Log.Identifier
import Mails.SendMail
import SMS.Data
import SMS.Model
import SMS.SMS
import Templates
import Theme.Model
import User.Model
import Util.Actor
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

processEvents :: String -> Scheduler ()
processEvents mailNoreplyAddress = dbQuery GetUnreadSMSEvents >>= mapM_ (\(eid, smsid, eventType, smsOrigMsisdn) -> do
    mkontraInfoForSMS <- dbQuery $ GetKontraInfoForSMS smsid
    logInfo "Messages.procesEvent: logging info" . object $ [
            identifier_ eid
          , identifier_ smsid
          , "event_type" .= show eventType
          ]
    processEvent (eid, smsid, eventType, mkontraInfoForSMS, smsOrigMsisdn)
  )
  where
    processEvent (eid, _, eventType, Just (DocumentInvitationSMS _did slid), smsOrigMsisdn) = do
      docs <- dbQuery $ GetDocumentsBySignatoryLinkIDs [slid]
      case docs of
        [] -> do
          logInfo "SMS event for purged/non-existing document" $ object [identifier_ slid]
          void $ dbUpdate $ MarkSMSEventAsRead eid
        (doc':_) -> do
          exists <- dbQuery $ DocumentExistsAndIsNotPurgedOrReallyDeletedForAuthor (documentid doc')
          if not exists then do
            logInfo "SMS event for purged/non-existing document" $ object [identifier_ slid, logPair_ doc']
            void $ dbUpdate $ MarkSMSEventAsRead eid
          else withDocument doc' $ do
            _ <- dbUpdate $ MarkSMSEventAsRead eid
            msl <- getSigLinkFor slid <$> theDocument
            let signphone = maybe "" getMobile msl
            templates <- getGlobalTemplates
            bd <- (maybesignatory =<<) . getAuthorSigLink <$> theDocument >>= \case
              Nothing -> dbQuery $ GetMainBrandedDomain
              Just uid -> dbQuery $ GetBrandedDomainByUserID uid
            let host = bdUrl bd
                -- since when email is reported deferred author has a possibility to
                -- change email address, we don't want to send him emails reporting
                -- success/failure for old signatory address, so we need to compare
                -- addresses here (for dropped/bounce events)
                handleEv (SMSEvent phone ev) = do
                  logInfo "Comparing phone numbers" $ object [
                      identifier_ slid
                    , "signatory_phone" .= signphone
                    , "event_phone" .= phone
                    , "sms_original_phone" .= smsOrigMsisdn
                    ]
                  theDocument >>= \doc -> runTemplatesT (getLang doc, templates) $ case ev of
                    SMSDelivered -> handleDeliveredInvitation slid
                    SMSUndelivered _ -> when (signphone == smsOrigMsisdn) $ do
                      handleUndeliveredSMSInvitation mailNoreplyAddress bd host slid
            handleEv eventType

    processEvent (eid, _, eventType, Just (DocumentPinSendoutSMS _did slid), smsOrigMsisdn) =
      dbQuery (GetDocumentBySignatoryLinkID slid) >>= \doc' -> withDocument doc' $ do
        _ <- dbUpdate $ MarkSMSEventAsRead eid
        templates <- getGlobalTemplates
        msl <- getSigLinkFor slid <$> theDocument
        case (eventType,msl) of
          (SMSEvent phone SMSDelivered, Just sl) -> runTemplatesT (def, templates) $ do
            logInfo "SMS with PIN delivered" $ object [
                "recipient" .= phone
              , "sms_original_phone" .= smsOrigMsisdn
              ]
            time <- currentTime
            let actor = mailSystemActor time (maybesignatory sl) (getEmail sl) slid
            void $ dbUpdate $ InsertEvidenceEventWithAffectedSignatoryAndMsg
              SMSPinDeliveredEvidence
              (return ())
              (Just sl)
              (Just smsOrigMsisdn)
              (actor)
          _ -> return ()
    processEvent (eid, _ , eventType, Just (OtherDocumentSMS did), smsOrigMsisdn) =
      withDocumentID did $ do
        void $ dbUpdate $ MarkSMSEventAsRead eid
        case eventType of
          SMSEvent phone SMSDelivered ->
            logInfo "SMS delivered" $ object [
                identifier_ did
              , "recipient" .= phone
              , "sms_original_phone" .= smsOrigMsisdn
              ]
          _ -> return ()
    processEvent (eid, _ , _, _, _) = do
      _ <- dbUpdate $ MarkSMSEventAsRead eid
      return ()

handleDeliveredInvitation :: (CryptoRNG m, MonadThrow m, MonadLog m, DocumentMonad m, TemplatesMonad m) => SignatoryLinkID -> m ()
handleDeliveredInvitation signlinkid = logSignatory signlinkid $ do
  logInfo_ "handleDeliveredInvitation: logging info"
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just signlink -> do
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetSMSInvitationDeliveryStatus signlinkid Delivered actor
      return ()
    Nothing -> return ()

handleUndeliveredSMSInvitation :: (CryptoRNG m, MonadCatch m, MonadLog m, DocumentMonad m, TemplatesMonad m, MonadBase IO m) => String -> BrandedDomain -> String -> SignatoryLinkID -> m ()
handleUndeliveredSMSInvitation mailNoreplyAddress bd hostpart signlinkid = logSignatory signlinkid $ do
  logInfo_ "handleUndeliveredSMSInvitation: logging info"
  getSigLinkFor signlinkid <$> theDocument >>= \case
    Just signlink -> do
      time <- currentTime
      let actor = mailSystemActor time (maybesignatory signlink) (getEmail signlink) signlinkid
      _ <- dbUpdate $ SetSMSInvitationDeliveryStatus signlinkid Undelivered actor
      mail <- theDocument >>= \d -> smsUndeliveredInvitation mailNoreplyAddress bd hostpart d signlink
      theDocument >>= \d -> scheduleEmailSendout $ mail {
        to = [getMailAddress $ fromJust $ getAuthorSigLink d]
      }
      triggerAPICallbackIfThereIsOne =<< theDocument
    Nothing -> return ()

smsUndeliveredInvitation :: (TemplatesMonad m,MonadDB m,MonadThrow m) => String -> BrandedDomain -> String -> Document -> SignatoryLink -> m Mail
smsUndeliveredInvitation mailNoreplyAddress bd hostpart doc signlink = do
  theme <- dbQuery $ GetTheme $ bdMailTheme bd
  kontramail mailNoreplyAddress bd theme "invitationSMSUndelivered" $ do
    F.value "authorname" $ getFullName $ fromJust $ getAuthorSigLink doc
    F.value "documenttitle" $ documenttitle doc
    F.value "email" $ getEmail signlink
    F.value "name" $ getFullName signlink
    F.value "mobile" $ getMobile signlink
    F.value "unsigneddoclink" $ show $ LinkIssueDoc $ documentid doc
    F.value "ctxhostpart" hostpart
    brandingMailFields theme

module Doc.DocViewSMS (
      smsDocumentErrorAuthor
    , smsDocumentErrorSignatory
    , smsPartyProcessFinalizedNotification
    , smsInvitation
    , smsInvitationToAuthor
    , smsReminder
    , smsClosedNotification
    , smsRejectNotification
    , smsForwardSigningForAuthor
    , smsForwardSigningForNewSignatory
    , smsPinCodeSendout
    ) where

import Control.Monad.Catch
import Control.Monad.Trans
import Crypto.RNG
import Data.Time
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import DB
import Doc.DocInfo
import Doc.DocStateData hiding (DocumentStatus(..))
import Doc.DocViewMail (InvitationTo(..))
import Doc.Model.Update
import Doc.Types.SignatoryAccessToken
import KontraLink
import MagicHash
import MailContext
import MinutesTime
import SMS.SMS
import SMS.Types
import Templates
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Monoid

mkSMS
  :: (MonadDB m, MonadThrow m, MonadTime m, MailContextMonad m)
  => Document
  -> SignatoryLink
  -> [KontraInfoForSMS]
  -> Text
  -> m SMS
mkSMS doc sl kontraInfoForSMS msgBody = do
  mctx                    <- getMailContext
  (moriginator, provider) <- case getAuthorUserId doc of
    Nothing  -> return (Nothing, SMSDefault)
    Just uid -> do
      muser <- dbQuery $ GetUserByID uid
      case muser of
        Nothing   -> return (Nothing, SMSDefault)
        Just user -> do
          ugwp <- dbQuery . UserGroupGetWithParentsByUserID $ user ^. #id
          return (ugwpUI ugwp ^. #smsOriginator, ugwpSettings ugwp ^. #smsProvider)
  let originator = fromMaybe (mctx ^. #brandedDomain % #smsOriginator)
                             (justEmptyToNothing moriginator)
  return $ SMS (getMobile sl) kontraInfoForSMS msgBody originator provider

smsDocumentErrorAuthor
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => Document
  -> SignatoryLink
  -> m SMS
smsDocumentErrorAuthor doc sl = do
  mkSMS doc sl [OtherDocumentSMS $ documentid doc]
    =<< renderLocalTemplate doc "_smsDocumentErrorAuthor" (smsFields doc)

smsDocumentErrorSignatory
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => Document
  -> SignatoryLink
  -> m SMS
smsDocumentErrorSignatory doc sl = do
  mkSMS doc sl [OtherDocumentSMS $ documentid doc]
    =<< renderLocalTemplate doc "_smsDocumentErrorSignatory" (smsFields doc)

smsPartyProcessFinalizedNotification
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => Document
  -> SignatoryLink
  -> ProcessFinishedAction
  -> m SMS
smsPartyProcessFinalizedNotification doc sl action = do
  smsContents <-
    case (isClosed doc, documentsmsinvitetext doc, documentsmsconfirmtext doc) of
      (False, Just t, _) -> do
        link <- smsInvitationLink sl
        return $ t <+> link
      (True, _, Just t) -> do
        link <- fst <$> smsConfirmationLink sl
        return $ t <+> link
      _ -> do
        let template = case action of
              DocumentSigned   -> templateName "_smsDocumentSignedNotification"
              DocumentApproved -> templateName "_smsDocumentApprovedNotification"
            fields = do
              smsFields doc
              if isClosed doc
                then smsConfirmationLinkFields sl
                else smsInvitationLinkFields sl
        renderLocalTemplate doc (templateName template) fields
  mkSMS doc
        sl
        [DocumentPartyNotificationSMS (documentid doc) (signatorylinkid sl)]
        smsContents

smsInvitation
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => InvitationTo
  -> SignatoryLink
  -> Document
  -> m SMS
smsInvitation invitationTo sl doc = do
  smsContent <- case documentsmsinvitetext doc of
    Just t -> do
      link <- smsInvitationLink sl
      return $ t <+> link
    Nothing ->
      renderLocalTemplate doc template (smsFields doc >> smsInvitationLinkFields sl)
  mkSMS doc sl [DocumentInvitationSMS (documentid doc) (signatorylinkid sl)] smsContent
  where
    template = case invitationTo of
      Sign    -> templateName "_smsInvitationToSign"
      Approve -> templateName "_smsInvitationToApprove"
      View    -> templateName "_smsInvitationToView"

smsInvitationToAuthor
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadTime m
     , MonadThrow m
     , TemplatesMonad m
     )
  => Document
  -> SignatoryLink
  -> m SMS
smsInvitationToAuthor doc sl = do
  smsContent <- case documentsmsinvitetext doc of
    Just t -> do
      link <- smsInvitationLink sl
      return $ t <+> link
    Nothing -> renderLocalTemplate doc
                                   "_smsInvitationToAuthor"
                                   (smsFields doc >> smsInvitationLinkFields sl)
  mkSMS doc sl [DocumentInvitationSMS (documentid doc) (signatorylinkid sl)] smsContent

-- brittany-disable-next-binding
smsReminder
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => Bool
  -> Document
  -> SignatoryLink
  -> m SMS
smsReminder automatic doc sl = do
  smsContents <- case (isClosed doc ,documentsmsinvitetext doc, documentsmsconfirmtext doc) of
    (False, Just t,_) -> do
      link <- smsInvitationLink sl
      return $ t <+> link
    (True, _, Just t) -> do
      link <- fst <$> smsConfirmationLink sl
      return $ t <+> link
    _ -> do
      renderLocalTemplate doc template $ do
        smsFields doc
        if isClosed doc
          then smsConfirmationLinkFields sl
          else smsInvitationLinkFields sl
  mkSMS doc sl smstypesignatory smsContents
 where
  (smstypesignatory, template) = if
    | isSignatoryAndHasSigned sl || isApproverAndHasApproved sl
      -> ([OtherDocumentSMS $ documentid doc], templateName "_smsReminderSigned")
    | automatic && isApprover sl
      -> (invitation, templateName "_smsReminderApproveAutomatic")
    | automatic
      -> (invitation, templateName "_smsReminderAutomatic")
    | isApprover sl
      -> (invitation, templateName "_smsReminderApprove")
    | otherwise
      -> (invitation, templateName "_smsReminder")

  invitation = [DocumentInvitationSMS (documentid doc) (signatorylinkid sl)]

smsClosedNotification
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadTime m
     , MonadThrow m
     , TemplatesMonad m
     )
  => Document
  -> SignatoryLink
  -> Bool
  -> Bool
  -> m SMS
smsClosedNotification doc sl withEmail sealFixed = do
  smsContent <- case documentsmsconfirmtext doc of
    Just t -> do
      link <- fst <$> smsConfirmationLink sl
      return $ t <+> link
    Nothing ->
      renderLocalTemplate doc template (smsFields doc >> smsConfirmationLinkFields sl)
  mkSMS doc sl [OtherDocumentSMS $ documentid doc] smsContent
  where
    template = case (sealFixed, withEmail) of
      (True , True ) -> templateName "_smsCorrectedNotificationWithEmail"
      (True , False) -> templateName "_smsCorrectedNotification"
      (False, True ) -> templateName "_smsClosedNotificationWithEmail"
      (False, False) -> templateName "_smsClosedNotification"

smsRejectNotification
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadTime m
     , MonadThrow m
     , TemplatesMonad m
     )
  => Document
  -> SignatoryLink
  -> SignatoryLink
  -> m SMS
smsRejectNotification doc sl rejector = do
  mkSMS doc sl [OtherDocumentSMS $ documentid doc] =<< renderLocalTemplate
    doc
    "_smsRejectNotification"
    (smsFields doc >> F.value "rejectorName" (getSmartName rejector))

smsForwardSigningForAuthor
  :: (MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m, TemplatesMonad m)
  => SignatoryLink
  -> SignatoryLink
  -> Document
  -> m SMS
smsForwardSigningForAuthor originalsl newsl doc = do
  let alink = fromJust $ getAuthorSigLink doc
      doRender
        | signatoryrole newsl == SignatoryRoleSigningParty = renderLocalTemplate
          doc
          (templateName "_smsForwardSigningForAuthor")
        | otherwise = renderLocalTemplate
          doc
          (templateName "_smsForwardSigningForAuthorApproving")
  message <- doRender $ do
    smsFields doc
    F.value "fromName" (getSmartName originalsl)
    F.value "toName" (getSmartName newsl)
  mkSMS doc alink [OtherDocumentSMS $ documentid doc] message

smsForwardSigningForNewSignatory
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadTime m
     , MonadThrow m
     , TemplatesMonad m
     )
  => SignatoryLink
  -> SignatoryLink
  -> Document
  -> m SMS
smsForwardSigningForNewSignatory originalsl newsl doc = do
  let doRender
        | signatoryrole newsl == SignatoryRoleSigningParty = renderLocalTemplate
          doc
          (templateName "_smsForwardSigningForNewSignatory")
        | otherwise = renderLocalTemplate
          doc
          (templateName "_smsForwardSigningForNewSignatoryApproving")
  message <- doRender $ do
    smsFields doc
    smsInvitationLinkFields newsl
    F.value "fromName" (getSmartName originalsl)
    F.value "toName" (getSmartName newsl)
  mkSMS doc newsl [DocumentInvitationSMS (documentid doc) (signatorylinkid newsl)] message

smsPinCodeSendout
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadTime m
     , MonadThrow m
     , TemplatesMonad m
     )
  => Document
  -> SignatoryLink
  -> Text
  -> Text
  -> m SMS
smsPinCodeSendout doc sl phone pin = do
  sms <-
    mkSMS doc sl [DocumentPinSendoutSMS (documentid doc) (signatorylinkid sl)]
      =<< renderLocalTemplate doc "_smsPinSendout" (smsFields doc >> F.value "pin" pin)
  return sms { smsMSISDN = phone }

smsFields :: (TemplatesMonad m, MailContextMonad m) => Document -> Fields m ()
smsFields document = do
  mctx <- lift getMailContext
  F.value "creatorname" $ getSmartName <$> getAuthorSigLink document
  F.value "documenttitle" $ documenttitle document
  F.value "authorlink" $ (mctx ^. #brandedDomain % #url) <> showt
    (LinkIssueDoc (documentid document))

smsInvitationLink
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => SignatoryLink
  -> m Text
smsInvitationLink sl = do
  mctx <- getMailContext
  mh   <- dbUpdate $ NewSignatoryAccessToken (signatorylinkid sl)
                                             SignatoryAccessTokenForSMSBeforeClosing
                                             Nothing
  let link = LinkSignDocMagicHashShort (signatorylinkid sl) mh
  return $ mctx ^. #brandedDomain % #url <> showt link

smsInvitationLinkFields
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => SignatoryLink
  -> Fields m ()
smsInvitationLinkFields sl = do
  link <- lift $ smsInvitationLink sl
  F.value "link" link

smsConfirmationLink
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => SignatoryLink
  -> m (Text, UTCTime)
smsConfirmationLink sl = do
  mctx             <- getMailContext
  (mh, expiration) <- makeConfirmationMagicHash sl
  let link = LinkSignDocMagicHashShort (signatorylinkid sl) mh
  return (mctx ^. #brandedDomain % #url <> showt link, expiration)

smsConfirmationLinkFields
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => SignatoryLink
  -> Fields m ()
smsConfirmationLinkFields sl = do
  (link, expiration) <- lift $ smsConfirmationLink sl
  F.value "link" link
  F.value "availabledate" $ formatTimeYMD expiration

-- | Create a temporary hash valid for 30 days.
makeConfirmationMagicHash
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m)
  => SignatoryLink
  -> m (MagicHash, UTCTime)
makeConfirmationMagicHash sl = do
  now <- currentTime
  -- Make it valid until the end of the 30th day.
  let expiration = 30 `daysAfter` now
  mh <- dbUpdate $ NewSignatoryAccessToken (signatorylinkid sl)
                                           SignatoryAccessTokenForSMSAfterClosing
                                           (Just expiration)
  return (mh, expiration)

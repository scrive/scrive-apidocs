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

import BrandedDomain.BrandedDomain
import DB
import Doc.DocInfo
import Doc.DocStateData hiding (DocumentStatus(..))
import Doc.DocViewMail (InvitationTo(..))
import Doc.Model.Update
import Doc.Types.SignatoryAccessToken
import Doc.Types.SignatoryLink (ProcessFinishedAction(..))
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

mkSMS :: (MonadDB m, MonadThrow m, MonadTime m, MailContextMonad m) => Document -> SignatoryLink -> Maybe KontraInfoForSMS -> Text -> (m SMS)
mkSMS doc sl mkontraInfoForSMS msgBody = do
  mctx <- getMailContext
  (moriginator, provider) <- case maybesignatory =<< getAuthorSigLink doc of
    Nothing -> return (Nothing, SMSDefault)
    Just uid -> do
      muser <- dbQuery $ GetUserByID uid
      case muser of
        Nothing -> return (Nothing, SMSDefault)
        Just user -> do
          ugwp <- dbQuery . UserGroupGetWithParentsByUserID . userid $ user
          return
            ( get (uguiSmsOriginator . ugUI) . ugwpUG $ ugwp
            , get ugsSMSProvider . ugwpSettings $ ugwp)
  let originator = fromMaybe
        (get (bdSmsOriginator . mctxcurrentBrandedDomain) mctx)
        (justEmptyToNothing moriginator)
  return $ SMS (getMobile sl) mkontraInfoForSMS msgBody originator provider

smsDocumentErrorAuthor
  :: ( CryptoRNG m, MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m
     , TemplatesMonad m )
  => Document -> SignatoryLink -> m SMS
smsDocumentErrorAuthor doc sl = do
  mkSMS doc sl (Just $ OtherDocumentSMS $ documentid doc) =<< renderLocalTemplate doc "_smsDocumentErrorAuthor" (smsFields doc)

smsDocumentErrorSignatory
  :: ( CryptoRNG m, MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m
     , TemplatesMonad m )
  => Document -> SignatoryLink -> m SMS
smsDocumentErrorSignatory doc sl = do
  mkSMS doc sl (Just $ OtherDocumentSMS $ documentid doc) =<< renderLocalTemplate doc "_smsDocumentErrorSignatory" (smsFields doc)

smsPartyProcessFinalizedNotification ::
  ( CryptoRNG m
  , MailContextMonad m
  , MonadDB m
  , MonadThrow m
  , MonadTime m
  , TemplatesMonad m )
  => Document -> SignatoryLink -> ProcessFinishedAction -> m SMS
smsPartyProcessFinalizedNotification document signatoryLink action = do
  let
    info = DocumentPartyNotificationSMS
      (documentid document)
      (signatorylinkid signatoryLink)
    template = case action of
                  DocumentSigned -> templateName "_smsDocumentSignedNotification"
                  DocumentApproved -> templateName "_smsDocumentApprovedNotification"
    fields = do
      smsFields document
      if isClosed document
        then smsConfirmationLinkFields document signatoryLink
        else smsInvitationLinkFields document signatoryLink
  content <- renderLocalTemplate document (templateName template) fields
  mkSMS document signatoryLink (Just info) content

smsInvitation
  :: ( CryptoRNG m, MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m
     , TemplatesMonad m )
  => InvitationTo -> SignatoryLink -> Document -> m SMS
smsInvitation invitationTo sl doc = do
  mkSMS doc sl (Just $ DocumentInvitationSMS (documentid doc) (signatorylinkid sl)) =<<
    renderLocalTemplate doc template (smsFields doc >> smsInvitationLinkFields doc sl)
  where template = templateName $ case invitationTo of
                                    Sign    -> "_smsInvitationToSign"
                                    Approve -> "_smsInvitationToApprove"
                                    View    -> "_smsInvitationToView"

smsInvitationToAuthor
  :: ( CryptoRNG m, MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m
     , TemplatesMonad m )
  => Document -> SignatoryLink -> m SMS
smsInvitationToAuthor doc sl = do
  mkSMS doc sl (Just $ DocumentInvitationSMS (documentid doc) (signatorylinkid sl)) =<< renderLocalTemplate doc "_smsInvitationToAuthor" (smsFields doc >> smsInvitationLinkFields doc sl)

smsReminder :: ( CryptoRNG m, MailContextMonad m, MonadDB m
               , MonadThrow m, MonadTime m, TemplatesMonad m )
            => Bool -> Document -> SignatoryLink -> m SMS
smsReminder automatic doc sl = do
  contents <- renderLocalTemplate doc template $ do
    smsFields doc
    if isClosed doc
      then smsConfirmationLinkFields doc sl
      else smsInvitationLinkFields doc sl
  mkSMS doc sl smstypesignatory contents
  where
    (smstypesignatory, template) =
      if | isSignatoryAndHasSigned sl || isApproverAndHasApproved sl
                         -> ( Just $ (OtherDocumentSMS $ documentid doc)
                            , templateName "_smsReminderSigned" )
         | automatic && isApprover sl
                         -> ( invitation
                            , templateName "_smsReminderApproveAutomatic" )
         | automatic     -> ( invitation, templateName "_smsReminderAutomatic" )
         | isApprover sl -> ( invitation, templateName "_smsReminderApprove" )
         | otherwise     -> ( invitation, templateName "_smsReminder" )

    invitation = Just $ DocumentInvitationSMS
                        (documentid doc) (signatorylinkid sl)

smsClosedNotification
  :: ( CryptoRNG m, MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m
     , TemplatesMonad m )
  => Document -> SignatoryLink -> Bool -> Bool -> m SMS
smsClosedNotification doc sl withEmail sealFixed = do
  mkSMS doc sl (Just $ OtherDocumentSMS $ documentid doc) =<< (renderLocalTemplate doc template $ smsFields doc >> smsConfirmationLinkFields doc sl)
    where
      template = case (sealFixed, withEmail) of
        (True, True) -> templateName "_smsCorrectedNotificationWithEmail"
        (True, False) -> templateName "_smsCorrectedNotification"
        (False, True) -> templateName "_smsClosedNotificationWithEmail"
        (False, False) -> templateName "_smsClosedNotification"

smsRejectNotification
  :: ( CryptoRNG m, MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m
     , TemplatesMonad m )
  => Document -> SignatoryLink -> SignatoryLink -> m SMS
smsRejectNotification doc sl rejector = do
  mkSMS doc sl (Just $ OtherDocumentSMS $ documentid doc) =<< renderLocalTemplate doc "_smsRejectNotification" (smsFields doc >> F.value "rejectorName" (getSmartName rejector))

smsForwardSigningForAuthor
  :: (MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m
  , TemplatesMonad m) => SignatoryLink -> SignatoryLink -> Document -> m SMS
smsForwardSigningForAuthor originalsl newsl doc = do
  let alink = fromJust $ getAuthorSigLink doc
      doRender
        | signatoryrole newsl == SignatoryRoleSigningParty
        = renderLocalTemplate doc (templateName "_smsForwardSigningForAuthor")
        | otherwise
        = renderLocalTemplate doc (templateName "_smsForwardSigningForAuthorApproving")
  message <- doRender $ do
    smsFields doc
    F.value "fromName" (getSmartName originalsl)
    F.value "toName" (getSmartName newsl)
  mkSMS doc alink (Just $ OtherDocumentSMS $ documentid doc) message

smsForwardSigningForNewSignatory
  :: (CryptoRNG m, MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m
  , TemplatesMonad m) => SignatoryLink -> SignatoryLink -> Document -> m SMS
smsForwardSigningForNewSignatory originalsl newsl doc = do
  let doRender
        | signatoryrole newsl == SignatoryRoleSigningParty
        = renderLocalTemplate doc (templateName "_smsForwardSigningForNewSignatory")
        | otherwise
        = renderLocalTemplate doc (templateName "_smsForwardSigningForNewSignatoryApproving")
  message <- doRender $ do
    smsFields doc
    smsInvitationLinkFields doc newsl
    F.value "fromName" (getSmartName originalsl)
    F.value "toName" (getSmartName newsl)
  mkSMS doc newsl (Just $ DocumentInvitationSMS (documentid doc) (signatorylinkid newsl)) message

smsPinCodeSendout
  :: ( CryptoRNG m, MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m
     , TemplatesMonad m )
  => Document -> SignatoryLink -> Text -> Text -> m SMS
smsPinCodeSendout doc sl phone pin = do
  sms <- mkSMS doc sl (Just $ DocumentPinSendoutSMS (documentid doc) (signatorylinkid sl)) =<< renderLocalTemplate doc "_smsPinSendout" (smsFields doc >> F.value "pin" pin)
  return sms {smsMSISDN = phone}

smsFields :: (TemplatesMonad m, MailContextMonad m) => Document -> Fields m ()
smsFields document = do
  mctx <- lift $ getMailContext
  F.value "creatorname" $ getSmartName <$> getAuthorSigLink document
  F.value "documenttitle" $ documenttitle document
  F.value "authorlink" $
    get mctxDomainUrl mctx <> (showt (LinkIssueDoc (documentid document)))

smsInvitationLinkFields
  :: ( CryptoRNG m, MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m
     , TemplatesMonad m )
  => Document -> SignatoryLink -> Fields m ()
smsInvitationLinkFields doc sl = do
  mctx <- lift $ getMailContext
  link <- if signatoryisauthor sl
    then return $ LinkIssueDoc (documentid doc)
    else do
      mh <- lift $ dbUpdate $ NewSignatoryAccessToken
        (signatorylinkid sl) SignatoryAccessTokenForSMSBeforeClosing Nothing
      return $ LinkSignDocMagicHash (documentid doc) (signatorylinkid sl) mh
  F.value "link" $ get mctxDomainUrl mctx <> showt link

smsConfirmationLinkFields
  :: ( CryptoRNG m, MailContextMonad m, MonadDB m, MonadThrow m
     , MonadTime m, TemplatesMonad m )
  => Document -> SignatoryLink -> Fields m ()
smsConfirmationLinkFields doc sl = do
  mctx <- lift $ getMailContext
  (mh, expiration) <- lift $ makeConfirmationMagicHash sl
  F.value "link" $ get mctxDomainUrl mctx <>
    showt (LinkSignDocMagicHash (documentid doc) (signatorylinkid sl) mh)
  F.value "availabledate" $ formatTimeYMD expiration

-- | Create a temporary hash valid for 30 days.
makeConfirmationMagicHash
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m)
  => SignatoryLink -> m (MagicHash, UTCTime)
makeConfirmationMagicHash sl = do
  now <- currentTime
  -- Make it valid until the end of the 30th day.
  let expiration = (30 `daysAfter` now)
  mh <- dbUpdate $ NewSignatoryAccessToken (signatorylinkid sl)
    SignatoryAccessTokenForSMSAfterClosing (Just expiration)
  return (mh, expiration)

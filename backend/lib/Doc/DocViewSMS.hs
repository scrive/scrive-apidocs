module Doc.DocViewSMS (
      smsDocumentErrorAuthor
    , smsDocumentErrorSignatory
    , smsInvitation
    , smsInvitationToAuthor
    , smsReminder
    , smsClosedNotification
    , smsRejectNotification
    , smsPinCodeSendout
    ) where

import Control.Conditional ((<|), (|>))
import Control.Monad.Catch
import Control.Monad.Trans
import Text.StringTemplates.Templates
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.BrandedDomain
import DB
import Doc.DocStateData hiding (DocumentStatus(..))
import KontraLink
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

mkSMS :: (MonadDB m, MonadThrow m, MonadTime m, MailContextMonad m) => Document -> SignatoryLink -> Maybe KontraInfoForSMS -> String -> (m SMS)
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
            ( fmap T.unpack . get (uguiSmsOriginator . ugUI) . ugwpUG $ ugwp
            , get ugsSMSProvider . ugwpSettings $ ugwp)
  let originator = fromMaybe
        (get (bdSmsOriginator . mctxcurrentBrandedDomain) mctx)
        (justEmptyToNothing moriginator)
  return $ SMS (getMobile sl) mkontraInfoForSMS msgBody originator provider

smsDocumentErrorAuthor :: (MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorAuthor doc sl = do
  mkSMS doc sl (Just $ OtherDocumentSMS $ documentid doc) =<< renderLocalTemplate doc "_smsDocumentErrorAuthor" (smsFields doc sl)

smsDocumentErrorSignatory :: (MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorSignatory doc sl = do
  mkSMS doc sl (Just $ OtherDocumentSMS $ documentid doc) =<< renderLocalTemplate doc "_smsDocumentErrorSignatory" (smsFields doc sl)

smsInvitation :: (MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m, TemplatesMonad m) => SignatoryLink -> Document -> m SMS
smsInvitation sl doc = do
  mkSMS doc sl (Just $ DocumentInvitationSMS (documentid doc) (signatorylinkid sl)) =<<
    renderLocalTemplate doc (templateName "_smsInvitationToSign" <| isSignatory sl |> templateName "_smsInvitationToView") (smsFields doc sl)

smsInvitationToAuthor :: (MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsInvitationToAuthor doc sl = do
  mkSMS doc sl (Just $ DocumentInvitationSMS (documentid doc) (signatorylinkid sl)) =<< renderLocalTemplate doc "_smsInvitationToAuthor" (smsFields doc sl)

smsReminder :: ( MailContextMonad m, MonadDB m
               , MonadThrow m, MonadTime m, TemplatesMonad m )
            => Bool -> Document -> SignatoryLink -> m SMS
smsReminder automatic doc sl =
  mkSMS doc sl smstypesignatory
  =<< renderLocalTemplate doc template (smsFields doc sl)
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

smsClosedNotification :: (MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> Bool -> Bool -> m SMS
smsClosedNotification doc sl withEmail sealFixed = do
  mkSMS doc sl (Just $ OtherDocumentSMS $ documentid doc) =<< (renderLocalTemplate doc template $ smsFields doc sl)
  where template = case (sealFixed, withEmail) of
                     (True, True) -> templateName "_smsCorrectedNotificationWithEmail"
                     (True, False) -> templateName "_smsCorrectedNotification"
                     (False, True) -> templateName "_smsClosedNotificationWithEmail"
                     (False, False) -> templateName "_smsClosedNotification"

smsRejectNotification :: (MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> SignatoryLink -> m SMS
smsRejectNotification doc sl rejector = do
  mkSMS doc sl (Just $ OtherDocumentSMS $ documentid doc) =<< renderLocalTemplate doc "_smsRejectNotification" (smsFields doc sl >> F.value "rejectorName" (getSmartName rejector))

smsPinCodeSendout :: (MailContextMonad m, MonadDB m, MonadTime m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> String -> String -> m SMS
smsPinCodeSendout doc sl phone pin = do
  sms <- mkSMS doc sl (Just $ DocumentPinSendoutSMS (documentid doc) (signatorylinkid sl)) =<< renderLocalTemplate doc "_smsPinSendout" (smsFields doc sl >> F.value "pin" pin)
  return sms {smsMSISDN = phone}

smsFields :: (MailContextMonad m, MonadTime m, TemplatesMonad m) => Document -> SignatoryLink -> Fields m ()
smsFields document siglink = do
    mctx <- lift $ getMailContext
    F.value "creatorname" $ getSmartName <$> getAuthorSigLink document
    F.value "documenttitle" $ documenttitle document
    F.value "link" $ get mctxDomainUrl mctx ++
      show (LinkSignDoc (documentid document) siglink)
    now <- currentTime
    F.value "availabledate" $ formatTimeYMD $ 30 `daysAfter` now

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

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Maybe
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.BrandedDomain
import Company.Model
import Control.Logic
import DB
import Doc.DocStateData
import Doc.DocUtils
import KontraLink
import MailContext (MailContextMonad(..), MailContext(..))
import Mails.SendMail
import SMS.SMS
import Templates
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Monoid

mkSMS :: (MonadDB m, MonadThrow m, MailContextMonad m) => Document -> SignatoryLink -> MessageData -> String -> (m SMS)
mkSMS doc sl msgData msgBody = do
  mctx <- getMailContext
  moriginator <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
       Just uid -> fmap Just $ companysmsoriginator <$> companyinfo <$> (dbQuery $ GetCompanyByUserID uid)
       Nothing -> return Nothing
  let originator = fromMaybe (fromMaybe "Scrive" (bdsmsoriginator <$> mctxcurrentBrandedDomain mctx)) (joinEmpty  moriginator)
  return $ SMS (getMobile sl) msgData msgBody originator

smsDocumentErrorAuthor :: (MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorAuthor doc sl = do
  mkSMS doc sl None =<< renderLocalTemplate doc "_smsDocumentErrorAuthor" (smsFields doc sl)

smsDocumentErrorSignatory :: (MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorSignatory doc sl = do
  mkSMS doc sl None =<< renderLocalTemplate doc "_smsDocumentErrorSignatory" (smsFields doc sl)

smsInvitation :: (MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => SignatoryLink -> Document -> m SMS
smsInvitation sl doc = do
  mkSMS doc sl (Invitation (documentid doc) (signatorylinkid sl)) =<<
    renderLocalTemplate doc (templateName "_smsInvitationToSign" <| isSignatory sl |> templateName "_smsInvitationToView") (smsFields doc sl)

smsInvitationToAuthor :: (MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsInvitationToAuthor doc sl = do
  mkSMS doc sl (Invitation (documentid doc) (signatorylinkid sl)) =<< renderLocalTemplate doc "_smsInvitationToAuthor" (smsFields doc sl)

smsReminder :: (MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsReminder doc sl = mkSMS doc sl smsdata =<< renderLocalTemplate doc template (smsFields doc sl)
  where (smsdata, template) = case maybesigninfo sl of
          Nothing -> (Invitation (documentid doc) (signatorylinkid sl), templateName "_smsReminder")
          Just _  -> (None, templateName "_smsReminderSigned")

smsClosedNotification :: (MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> Bool -> Bool -> m SMS
smsClosedNotification doc sl withEmail sealFixed = do
  mkSMS doc sl None =<< (renderLocalTemplate doc (if sealFixed then templateName "_smsCorrectedNotification" else templateName "_smsClosedNotification") $ do
    smsFields doc sl
    F.value "withEmail" withEmail)

smsRejectNotification :: (MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> SignatoryLink -> m SMS
smsRejectNotification doc sl rejector = do
  mkSMS doc sl None =<< renderLocalTemplate doc "_smsRejectNotification" (smsFields doc sl >> F.value "rejectorName" (getSmartName rejector))

smsPinCodeSendout :: (MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => Document -> SignatoryLink -> String -> String -> m SMS
smsPinCodeSendout doc sl phone pin = do
  sms <- mkSMS doc sl None =<< renderLocalTemplate doc "_smsPinSendout" (smsFields doc sl >> F.value "pin" pin)
  return sms {smsMSISDN = phone, smsData = SMSPinSendout (signatorylinkid sl) }

smsFields :: (MailContextMonad m, TemplatesMonad m) => Document -> SignatoryLink -> Fields m ()
smsFields document siglink = do
    mctx <- lift $ getMailContext
    partylist <- lift $ renderListTemplateNormal $ map getSmartName $ filter isSignatory (documentsignatorylinks document)
    F.value "creatorname" $ getSmartName <$> getAuthorSigLink document
    F.value "personname" $ getSmartName siglink
    F.value "documenttitle" $ documenttitle document
    F.value "partylist" partylist
    F.value "link" $ mctxhostpart mctx ++ show (LinkSignDoc document siglink)

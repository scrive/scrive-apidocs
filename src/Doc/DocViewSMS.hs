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
import Data.String.Utils (strip)
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.BrandedDomain
import Company.CompanyUI
import Company.Model
import DB
import Doc.DocStateData
import Doc.DocUtils
import KontraLink
import KontraPrelude
import MailContext (MailContextMonad(..), MailContext(..))
import Mails.SendMail
import SMS.Data
import SMS.SMS
import Templates
import User.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Monoid

mkSMS :: (MonadDB m, MonadThrow m, MailContextMonad m) => Document -> SignatoryLink -> MessageData -> String -> (m SMS)
mkSMS doc sl msgData msgBody = do
  mctx <- getMailContext
  (moriginator, provider) <- case maybesignatory =<< getAuthorSigLink doc of
    Nothing -> return (Nothing, SMSDefault)
    Just uid -> do
      muser <- dbQuery $ GetUserByID uid
      case muser of
        Nothing -> return (Nothing, SMSDefault)
        Just user -> do
          orig <- companySmsOriginator <$> (dbQuery $ GetCompanyUI $ usercompany user)
          prov <- companysmsprovider . companyinfo <$> (dbQuery $ GetCompanyByUserID (userid user))
          return (orig, prov)
  let originator = fromMaybe (bdSmsOriginator $ mctxcurrentBrandedDomain mctx) (justEmptyToNothing moriginator)
  return $ SMS (getMobile sl) msgData msgBody originator provider

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
    F.value "partylist" $ strip partylist
    F.value "link" $ mctxhostpart mctx ++ show (LinkSignDoc (documentid document) siglink)

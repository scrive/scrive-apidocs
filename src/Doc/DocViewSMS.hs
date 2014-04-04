module Doc.DocViewSMS (
      smsMismatchSignatory
    , smsMismatchAuthor
    , smsDocumentErrorAuthor
    , smsDocumentErrorSignatory
    , smsInvitation
    , smsInvitationToAuthor
    , smsReminder
    , smsClosedNotification
    , smsRejectNotification
    ) where

import Control.Logic
import Control.Applicative
import Doc.DocStateData
import Doc.DocUtils
import KontraLink
import MailContext (MailContextMonad(..), MailContext(..))
import Mails.SendMail
import Text.StringTemplates.Templates
import Templates
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Text.StringTemplates.Fields as F
import SMS.SMS
import Control.Monad.Trans
import DB
import Data.Maybe
import Control.Monad
import Company.Model
import BrandedDomains
import Utils.Monoid

mkSMS :: (MonadDB m, MailContextMonad m) => Document -> SignatoryLink -> MessageData -> String -> (m SMS)
mkSMS doc sl msgData msgBody = do
  mctx <- getMailContext
  moriginator <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
       Just uid -> fmap Just $ companysmsoriginator <$> companyinfo <$> (dbQuery $ GetCompanyByUserID uid)
       Nothing -> return Nothing
  let originator = fromMaybe (fromMaybe "Scrive" (bdsmsoriginator <$> mctxcurrentBrandedDomain mctx)) (joinEmpty  moriginator)
  return $ SMS (getMobile sl) msgData msgBody originator



smsMismatchSignatory :: (MailContextMonad m, MonadDB m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsMismatchSignatory doc sl = do
  mkSMS doc sl None =<< renderLocalTemplate doc "_smsMismatchSignatory" (smsFields doc sl)

smsMismatchAuthor :: (MailContextMonad m, MonadDB m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsMismatchAuthor doc sl = do
  mkSMS doc sl None =<< renderLocalTemplate doc "_smsMismatchAuthor" (smsFields doc sl)

smsDocumentErrorAuthor :: (MailContextMonad m, MonadDB m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorAuthor doc sl = do
  mkSMS doc sl None =<< renderLocalTemplate doc "smsDocumentErrorAuthor" (smsFields doc sl)

smsDocumentErrorSignatory :: (MailContextMonad m, MonadDB m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorSignatory doc sl = do
  mkSMS doc sl None =<< renderLocalTemplate doc "smsDocumentErrorSignatory" (smsFields doc sl)

smsInvitation :: (MailContextMonad m, MonadDB m, TemplatesMonad m) => SignatoryLink -> Document -> m SMS
smsInvitation sl doc = do
  mkSMS doc sl (Invitation (documentid doc) (signatorylinkid sl)) =<<
    renderLocalTemplate doc (templateName "_smsInvitationToSign" <| isSignatory sl |> templateName "_smsInvitationToView") (smsFields doc sl)

smsInvitationToAuthor :: (MailContextMonad m, MonadDB m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsInvitationToAuthor doc sl = do
  mkSMS doc sl (Invitation (documentid doc) (signatorylinkid sl)) =<< renderLocalTemplate doc "_smsInvitationToAuthor" (smsFields doc sl)

smsReminder :: (MailContextMonad m, MonadDB m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsReminder doc sl = mkSMS doc sl smsdata =<< renderLocalTemplate doc template (smsFields doc sl)
  where (smsdata, template) = case maybesigninfo sl of
          Nothing -> (Invitation (documentid doc) (signatorylinkid sl), "_smsReminder")
          Just _  -> (None, "_smsReminderSigned")

smsClosedNotification :: (MailContextMonad m, MonadDB m, TemplatesMonad m) => Document -> SignatoryLink -> Bool -> Bool -> m SMS
smsClosedNotification doc sl withEmail sealFixed = do
  mkSMS doc sl None =<< (renderLocalTemplate doc (if sealFixed then templateName "_smsCorrectedNotification" else templateName "_smsClosedNotification") $ do
    smsFields doc sl
    F.value "withEmail" withEmail)

smsRejectNotification :: (MailContextMonad m, MonadDB m, TemplatesMonad m) => Document -> SignatoryLink -> SignatoryLink -> m SMS
smsRejectNotification doc sl rejector = do
  mkSMS doc sl None =<< renderLocalTemplate doc "_smsRejectNotification" (smsFields doc sl >> F.value "rejectorName" (getSmartName rejector))

smsFields :: (MailContextMonad m, TemplatesMonad m) => Document -> SignatoryLink -> Fields m ()
smsFields document siglink = do
    mctx <- lift $ getMailContext
    partylist <- lift $ renderListTemplateNormal $ map getSmartName $ filter isSignatory (documentsignatorylinks document)
    F.value "creatorname" $ getSmartName <$> getAuthorSigLink document
    F.value "personname" $ getSmartName siglink
    F.value "documenttitle" $ documenttitle document
    F.value "partylist" partylist
    F.value "link" $ mctxhostpart mctx ++ show (LinkSignDoc document siglink)


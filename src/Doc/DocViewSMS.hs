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
import Kontra
import KontraLink
import Mails.SendMail
import Text.StringTemplates.Templates
import Templates
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Text.StringTemplates.Fields as F
import SMS.SMS
import Control.Monad.Trans
import DB

smsMismatchSignatory :: (MonadDB m, KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsMismatchSignatory doc sl = do
  mkSMS sl None =<< renderLocalTemplate doc "_smsMismatchSignatory" (smsFields doc sl)

smsMismatchAuthor :: (MonadDB m, KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsMismatchAuthor doc sl = do
  mkSMS sl None =<< renderLocalTemplate doc "_smsMismatchAuthor" (smsFields doc sl)

smsDocumentErrorAuthor :: (MonadDB m, KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorAuthor doc sl = do
  mkSMS sl None =<< renderLocalTemplate doc "smsDocumentErrorAuthor" (smsFields doc sl)

smsDocumentErrorSignatory :: (MonadDB m, KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorSignatory doc sl = do
  mkSMS sl None =<< renderLocalTemplate doc "smsDocumentErrorSignatory" (smsFields doc sl)

smsInvitation :: (MonadDB m, KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsInvitation doc sl = do
  mkSMS sl (Invitation (documentid doc) (signatorylinkid sl)) =<< renderLocalTemplate doc ("_smsInvitationToSign" <| isSignatory sl |> "_smsInvitationToView") (smsFields doc sl)

smsInvitationToAuthor :: (MonadDB m, KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsInvitationToAuthor doc sl = do
  mkSMS sl (Invitation (documentid doc) (signatorylinkid sl)) =<< renderLocalTemplate doc "_smsInvitationToAuthor" (smsFields doc sl)

smsReminder :: (MonadDB m, KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsReminder doc sl = do
  mkSMS sl (Invitation (documentid doc) (signatorylinkid sl)) =<< renderLocalTemplate doc "_smsReminder" (smsFields doc sl)

smsClosedNotification :: (MonadDB m, HasMailContext c, TemplatesMonad m) => c -> Document -> SignatoryLink -> Bool -> Bool -> m SMS
smsClosedNotification ctx doc sl withEmail sealFixed = do
  (mkSMS sl None =<<) $ renderLocalTemplate doc (if sealFixed then "_smsCorrectedNotification" else "_smsClosedNotification") $ do
    smsFields' ctx doc sl
    F.value "withEmail" withEmail

smsRejectNotification :: (MonadDB m, KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> SignatoryLink -> m SMS
smsRejectNotification doc sl rejector = do
  mkSMS sl None =<< renderLocalTemplate doc "_smsRejectNotification" (smsFields doc sl >> F.value "rejectorName" (getSmartName rejector))

smsFields :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> Fields m ()
smsFields document siglink = do
  ctx <- lift getContext
  smsFields' ctx document siglink

smsFields' :: (TemplatesMonad m, HasMailContext c) => c -> Document -> SignatoryLink -> Fields m ()
smsFields' ctx document siglink = do
    F.value "creatorname" $ getSmartName <$> getAuthorSigLink document
    F.value "personname" $ getSmartName siglink
    F.value "documenttitle" $ documenttitle document
    F.value "partylist" $ map getSmartName $ partyList document
    F.value "link" $ mctxhostpart (mailContext ctx) ++ show (LinkSignDoc document siglink)


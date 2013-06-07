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
import Doc.DocStateData
import Doc.DocUtils
import Kontra
import KontraLink
import Mails.SendMail
import Text.StringTemplates.Templates
import Templates
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Data.Functor
import qualified Text.StringTemplates.Fields as F
import SMS.SMS
import Control.Monad.Trans

smsMismatchSignatory :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsMismatchSignatory doc sl = do
  (SMS (getMobile sl) None) <$> renderLocalTemplate doc "_smsMismatchSignatory" (smsFields doc sl)

smsMismatchAuthor :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsMismatchAuthor doc sl = do
  (SMS (getMobile sl) None) <$> renderLocalTemplate doc "_smsMismatchAuthor" (smsFields doc sl)

smsDocumentErrorAuthor :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorAuthor doc sl = do
  (SMS (getMobile sl) None) <$> renderLocalTemplate doc "smsDocumentErrorAuthor" (smsFields doc sl)

smsDocumentErrorSignatory :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorSignatory doc sl = do
  (SMS (getMobile sl) None) <$> renderLocalTemplate doc "smsDocumentErrorSignatory" (smsFields doc sl)

smsInvitation :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsInvitation doc sl = do
  (SMS (getMobile sl) (Invitation (documentid doc) (signatorylinkid sl))) <$> renderLocalTemplate doc ("_smsInvitationToSign" <| isSignatory sl |> "_smsInvitationToView") (smsFields doc sl)

smsInvitationToAuthor :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsInvitationToAuthor doc sl = do
  (SMS (getMobile sl) (Invitation (documentid doc) (signatorylinkid sl))) <$> renderLocalTemplate doc "_smsInvitationToAuthor" (smsFields doc sl)

smsReminder :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsReminder doc sl = do
  (SMS (getMobile sl) (Invitation (documentid doc) (signatorylinkid sl))) <$> renderLocalTemplate doc "_smsReminder" (smsFields doc sl)

smsClosedNotification :: (HasMailContext c, TemplatesMonad m) => c -> Document -> SignatoryLink -> Bool -> Bool -> m SMS
smsClosedNotification ctx doc sl withEmail sealFixed = do
  (SMS (getMobile sl) None <$>) $ renderLocalTemplate doc "_smsClosedNotification" $ do
    smsFields' ctx doc sl
    F.value "withEmail" withEmail
    F.value "sealFixed" sealFixed

smsRejectNotification :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> SignatoryLink -> m SMS
smsRejectNotification doc sl rejector = do
  (SMS (getMobile sl) None) <$> renderLocalTemplate doc "_smsRejectNotification" (smsFields doc sl >> F.value "rejectorName" (getSmartName rejector))

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


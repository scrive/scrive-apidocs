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

import Data.Char
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
import Util.HasSomeCompanyInfo
import Util.SignatoryLinkUtils
import qualified Text.StringTemplates.Fields as F
import SMS.SMS
import Control.Monad.Trans

mkSMS :: Document -> SignatoryLink -> MessageData -> String -> SMS
mkSMS doc sl msgData msgBody = SMS (getMobile sl) msgData msgBody msgOriginator
  where defaultOriginator = "Scrive"
        msgOriginator = case getAuthorSigLink doc of
                          Nothing -> defaultOriginator
                          Just asl -> let companyName = getCompanyName asl
                                     in if all isSpace companyName then
                                            defaultOriginator
                                        else
                                            companyName

smsMismatchSignatory :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsMismatchSignatory doc sl = do
  mkSMS doc sl None <$> renderLocalTemplate doc "_smsMismatchSignatory" (smsFields doc sl)

smsMismatchAuthor :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsMismatchAuthor doc sl = do
  mkSMS doc sl None <$> renderLocalTemplate doc "_smsMismatchAuthor" (smsFields doc sl)

smsDocumentErrorAuthor :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorAuthor doc sl = do
  mkSMS doc sl None <$> renderLocalTemplate doc "smsDocumentErrorAuthor" (smsFields doc sl)

smsDocumentErrorSignatory :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsDocumentErrorSignatory doc sl = do
  mkSMS doc sl None <$> renderLocalTemplate doc "smsDocumentErrorSignatory" (smsFields doc sl)

smsInvitation :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsInvitation doc sl = do
  mkSMS doc sl (Invitation (documentid doc) (signatorylinkid sl)) <$> renderLocalTemplate doc ("_smsInvitationToSign" <| isSignatory sl |> "_smsInvitationToView") (smsFields doc sl)

smsInvitationToAuthor :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsInvitationToAuthor doc sl = do
  mkSMS doc sl (Invitation (documentid doc) (signatorylinkid sl)) <$> renderLocalTemplate doc "_smsInvitationToAuthor" (smsFields doc sl)

smsReminder :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> m SMS
smsReminder doc sl = do
  mkSMS doc sl (Invitation (documentid doc) (signatorylinkid sl)) <$> renderLocalTemplate doc "_smsReminder" (smsFields doc sl)

smsClosedNotification :: (HasMailContext c, TemplatesMonad m) => c -> Document -> SignatoryLink -> Bool -> Bool -> m SMS
smsClosedNotification ctx doc sl withEmail sealFixed = do
  (mkSMS doc sl None <$>) $ renderLocalTemplate doc (if sealFixed then "_smsCorrectedNotification" else "_smsClosedNotification") $ do
    smsFields' ctx doc sl
    F.value "withEmail" withEmail

smsRejectNotification :: (KontraMonad m, TemplatesMonad m) => Document -> SignatoryLink -> SignatoryLink -> m SMS
smsRejectNotification doc sl rejector = do
  mkSMS doc sl None <$> renderLocalTemplate doc "_smsRejectNotification" (smsFields doc sl >> F.value "rejectorName" (getSmartName rejector))

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


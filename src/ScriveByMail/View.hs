module ScriveByMail.View where

import Templates.Templates
import Templates.TemplatesUtils
import Context
import Doc.DocStateData
import Util.HasSomeUserInfo
import Doc.DocUtils
import Mails.MailsData
import Doc.DocViewMail
import Data.Maybe
import Doc.DocProcess
import KontraLink
import MagicHash
import MinutesTime
import ScriveByMail.Model
import FlashMessage

import Control.Applicative
import Control.Monad.IO.Class
import Data.Int
import qualified Data.ByteString.UTF8 as BS

mailMailAPIConfirm :: TemplatesMonad m
                      => Context
                      -> Document
                      -> SignatoryLink
                      -> m Mail
mailMailAPIConfirm ctx document siglink = do
  let issignatory = (elem SignatoryPartner . signatoryroles) siglink
  documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailconfirmbymailapi)  $ do
        fieldM "footer" $ mailFooterForDocument ctx document
        fieldM "timetosigninfo" $ do
            case (documenttimeouttime document) of
                 Just time -> renderLocalTemplateFM document "timetosigninfo" $ do
                                  field "time" $ show time
                 Nothing -> return ""
        fieldM "partnersinfo" $ do
             renderLocalListTemplate document $ map (BS.toString . getSmartName) $ partyList document
        fieldM "whohadsignedinfo" $ do
             do
                   signedlist <- if (not $ null $ partySignedList document)
                                    then fmap Just $ renderLocalListTemplate document $  map (BS.toString . getSmartName) $ partySignedList document
                                    else return Nothing
                   renderLocalTemplateForProcess document processwhohadsignedinfoformail $ do
                       field "signedlist" signedlist
        field "issignatory" $ issignatory
        field "isattachments" $ False
        field "hassigattachments" $ False
        field "ctxhostpart" $ ctxhostpart ctx
        field "link" $ ctxhostpart ctx ++ (show $  LinkIssueDoc (documentid document))

mailMailApiError:: TemplatesMonad m =>
                   Context ->
                   String ->
                   m Mail
mailMailApiError ctx err =
  kontramail "mailMailAPIError" $ do
    field "errormsg" err
    field "ctxhostpart" (ctxhostpart ctx)

mailMailApiDelayAdmin :: TemplatesMonad m => Context -> String -> String -> Int64 -> MagicHash -> MinutesTime -> m Mail
mailMailApiDelayAdmin ctx adminemail email delayid key expires =
  kontramail "mailMailAPIDelayAdmin" $ do
    field "ctxhostpart" $ ctxhostpart ctx
    field "confirmationlink" $ ctxhostpart ctx ++ (show $ LinkMailAPIDelayConfirmation adminemail delayid key)
    field "email" email
    field "expires" $ showDateDMY expires
    
mailMailApiDelayUser :: TemplatesMonad m => Context -> String -> m Mail
mailMailApiDelayUser _ctx email =
  kontramail "mailMailAPIDelayUser" $ do
    field "email" email

mailAPIInfoFields :: MonadIO m => MailAPIInfo -> Fields m
mailAPIInfoFields info = do
  field "mailapikey"   $ show $ umapiKey        info
  field "mailapilimit" $ show $ umapiDailyLimit info
  field "mailapisent"  $ show $ umapiSentToday  info

modalDenyDelay :: TemplatesMonad m => m FlashMessage
modalDenyDelay =
  toModal <$> renderTemplateM "modalDenyDelay" ()

modalConfirmDelay :: TemplatesMonad m => String -> m FlashMessage
modalConfirmDelay email =
  toModal <$> renderTemplateFM "modalConfirmDelay" (field "email" email)

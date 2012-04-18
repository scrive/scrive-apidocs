module ScriveByMail.View where

import Templates.Templates
import Templates.TemplatesUtils
import Context
import DB
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
import qualified Templates.Fields as F

import Control.Applicative
import Data.Int

mailMailAPIConfirm :: (MonadDB m, TemplatesMonad m)
                   => Context
                   -> Document
                   -> SignatoryLink
                   -> m Mail
mailMailAPIConfirm ctx document siglink = do
  let issignatory = (elem SignatoryPartner . signatoryroles) siglink
  documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailconfirmbymailapi)  $ do
        F.valueM "footer" $ mailFooterForDocument ctx document
        F.valueM "timetosigninfo" $ do
            case (documenttimeouttime document) of
                 Just time -> renderLocalTemplate document "timetosigninfo" $ do
                                  F.value "time" $ show time
                 Nothing -> return ""
        F.valueM "partnersinfo" $ do
             renderLocalListTemplate document $ map getSmartName $ partyList document
        F.valueM "whohadsignedinfo" $ do
             do
                   signedlist <- if (not $ null $ partySignedList document)
                                    then fmap Just $ renderLocalListTemplate document $  map getSmartName $ partySignedList document
                                    else return Nothing
                   renderLocalTemplateForProcess document processwhohadsignedinfoformail $ do
                       F.value "signedlist" signedlist
        F.value "issignatory" $ issignatory
        F.value "isattachments" $ False
        F.value "hassigattachments" $ False
        F.value "ctxhostpart" $ ctxhostpart ctx
        F.value "link" $ ctxhostpart ctx ++ (show $  LinkIssueDoc (documentid document))

mailMailApiError :: TemplatesMonad m => Context -> String -> m Mail
mailMailApiError ctx err = kontramail "mailMailAPIError" $ do
  F.value "errormsg" err
  F.value "ctxhostpart" (ctxhostpart ctx)

mailMailApiDelayAdmin :: TemplatesMonad m => Context -> String -> String -> Int64 -> MagicHash -> MinutesTime -> m Mail
mailMailApiDelayAdmin ctx adminemail email delayid key expires =
  kontramail "mailMailAPIDelayAdmin" $ do
    F.value "ctxhostpart" $ ctxhostpart ctx
    F.value "confirmationlink" $ ctxhostpart ctx ++ (show $ LinkMailAPIDelayConfirmation adminemail delayid key)
    F.value "email" email
    F.value "expires" $ showDateDMY expires

mailMailApiDelayUser :: TemplatesMonad m => Context -> String -> m Mail
mailMailApiDelayUser _ctx email =
  kontramail "mailMailAPIDelayUser" $ do
    F.value "email" email

mailAPIInfoFields :: Monad m => MailAPIInfo -> Fields m ()
mailAPIInfoFields info = do
  F.value "mailapikey"   $ show $ umapiKey        info
  F.value "mailapilimit" $ show $ umapiDailyLimit info
  F.value "mailapisent"  $ show $ umapiSentToday  info

modalDenyDelay :: TemplatesMonad m => m FlashMessage
modalDenyDelay = toModal <$> renderTemplate_ "modalDenyDelay"

modalConfirmDelay :: TemplatesMonad m => String -> m FlashMessage
modalConfirmDelay email =
  toModal <$> renderTemplate "modalConfirmDelay" (F.value "email" email)

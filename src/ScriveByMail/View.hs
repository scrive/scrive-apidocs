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
--import Mails.SendMail

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
             renderLocalListTemplate document $ map getSmartName $ partyList document
        fieldM "whohadsignedinfo" $ do
             do
                   signedlist <- if (not $ null $ partySignedList document)
                                    then fmap Just $ renderLocalListTemplate document $  map getSmartName $ partySignedList document
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

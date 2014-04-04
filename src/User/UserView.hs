{-# LANGUAGE ExtendedDefaultRules #-}
module User.UserView (
    -- pages
    userJSON,
    companyJSON,
    documentSignviewBrandingJSON,
    signviewBrandingJSON,
    showAccount,
    pageAcceptTOS,
    pageDoYouWantToChangeEmail,

    -- mails
    newUserMail,
    mailNewAccountCreatedByAdmin,
    accessNewAccountMail,
    resetPasswordMail,
    mailEmailChangeRequest,

    -- flash messages
    flashMessageLoginRedirectReason,
    flashMessageUserDetailsSaved,
    flashMessageMustAcceptTOS,
    flashMessagePasswordsDontMatch,
    flashMessageUserPasswordChanged,
    flashMessagePasswordChangeLinkNotValid,
    flashMessageAccessNewAccountLinkNotValid,
    flashMessageUserWithSameEmailExists,
    flashMessageUserActivated,
    flashMessageNewActivationLinkSend,
    flashMessageProblemWithEmailChange,
    flashMessageProblemWithPassword,
    flashMessageYourEmailHasChanged,

    userStatsToJSON,
    companyStatsToJSON,
    ) where

import Control.Applicative ((<$>))
import Data.Maybe
import Company.Model
import Company.CompanyUI
import Kontra
import KontraLink
import Mails.SendMail(Mail, kontramail, kontramaillocal)
import Text.StringTemplates.Templates
import Text.StringTemplate.GenericStandard()
import FlashMessage
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import User.Model
import User.Email
import MinutesTime
import Data.List
import Text.JSON
import Text.JSON.Gen
import qualified Text.StringTemplates.Fields as F
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Base64 as B64
import DB
import BrandedDomains.BrandedDomains
import Doc.DocViewMail
import Doc.DocStateData
import Control.Monad

showAccount :: TemplatesMonad m => User -> m String
showAccount user = renderTemplate "showAccount" $ do
    F.value "companyAdmin" $ useriscompanyadmin user

userJSON :: Monad m => Context -> User -> (Company,CompanyUI) ->  m JSValue
userJSON ctx user (company,companyui) = runJSONGenT $ do
    value "id" $ show $ userid user
    value "fstname" $ getFirstName user
    value "sndname" $ getLastName user
    value "email" $ getEmail user
    value "personalnumber" $ getPersonalNumber user
    value "phone" $ userphone $ userinfo user
    value "companyadmin" $ useriscompanyadmin user
    value "companyposition" $ usercompanyposition $ userinfo user
    value "lang"   $ codeFromLang $ getLang user
    valueM "company" $ companyJSON ctx company companyui

companyUIJson :: Monad m => Context -> CompanyUI -> m JSValue
companyUIJson ctx companyui = runJSONGenT $ do
    value "companyemaillogo" $ fromMaybe "" $ ((++) "data:image/png;base64,")  <$> BS.toString . B64.encode . unBinary <$> (companyemaillogo $ companyui)
    value "companysignviewlogo" $ fromMaybe ""  $ ((++) "data:image/png;base64,")  <$> BS.toString .  B64.encode . unBinary <$> (companysignviewlogo $ companyui)
    value "companyemailfont" $ fromMaybe "" $ companyemailfont $ companyui
    value "companyemailbordercolour" $ fromMaybe "" $ companyemailbordercolour $ companyui
    value "companyemailbuttoncolour" $ fromMaybe "" $ companyemailbuttoncolour $ companyui
    value "companyemailemailbackgroundcolour" $ fromMaybe "" $ companyemailemailbackgroundcolour $ companyui
    value "companyemailbackgroundcolour" $ fromMaybe "" $ companyemailbackgroundcolour $ companyui
    value "companyemailtextcolour" $ fromMaybe "" $ companyemailtextcolour $ companyui
    value "companysignviewtextcolour" $ fromMaybe "" $ companysignviewtextcolour $ companyui
    value "companysignviewtextfont" $ fromMaybe "" $ companysignviewtextfont $ companyui
    value "companysignviewprimarycolour" $ fromMaybe "" $ companysignviewprimarycolour $ companyui
    value "companysignviewprimarytextcolour" $ fromMaybe "" $ companysignviewprimarytextcolour $ companyui
    value "companysignviewsecondarycolour" $ fromMaybe "" $ companysignviewsecondarycolour $ companyui
    value "companysignviewsecondarytextcolour" $ fromMaybe "" $ companysignviewsecondarytextcolour $ companyui
    value "companysignviewbarscolour" $ fromMaybe "" $ companysignviewbarscolour $ companyui
    value "companysignviewbarstextcolour" $ fromMaybe "" $ companysignviewbarstextcolour $ companyui
    value "companysignviewbackgroundcolour" $ fromMaybe "" $ companysignviewbackgroundcolour $ companyui
    value "companycustomlogo" $ fromMaybe ""  $ ((++) "data:image/png;base64,")  <$> BS.toString .  B64.encode . unBinary <$> (companycustomlogo $ companyui)
    value "companycustombarscolour" $ fromMaybe "" $ companycustombarscolour $ companyui
    value "companycustombarstextcolour" $ fromMaybe "" $ companycustombarstextcolour $ companyui
    value "companycustombarssecondarycolour" $ fromMaybe "" $ companycustombarssecondarycolour $ companyui
    value "companycustombackgroundcolour" $ fromMaybe "" $ companycustombackgroundcolour $ companyui
    value "domaincustomlogo" $ fromMaybe "" $ bdlogolink <$> currentBrandedDomain ctx
    value "domainbarscolour" $ fromMaybe "" $ bdbarscolour <$> currentBrandedDomain ctx
    value "domainbarstextcolour" $ fromMaybe "" $ bdbarstextcolour <$> currentBrandedDomain ctx
    value "domainbarssecondarycolour" $ fromMaybe "" $ bdbarssecondarycolour <$> currentBrandedDomain ctx
    value "domainbackgroundcolour" $ fromMaybe "" $ bdbackgroundcolour <$> currentBrandedDomain ctx
    value "domainmailsbackgroundcolor" $ fromMaybe "" $ bdmailsbackgroundcolor <$> currentBrandedDomain ctx
    value "domainmailsbuttoncolor" $ fromMaybe "" $ bdmailsbuttoncolor <$> currentBrandedDomain ctx
    value "domainmailstextcolor" $ fromMaybe "" $ bdmailstextcolor <$> currentBrandedDomain ctx
    value "domainsignviewprimarycolour" $ fromMaybe "" $ bdsignviewprimarycolour <$> currentBrandedDomain ctx
    value "domainsignviewprimarytextcolour" $ fromMaybe "" $ bdsignviewprimarytextcolour <$> currentBrandedDomain ctx
    value "domainsignviewsecondarycolour" $ fromMaybe "" $ bdsignviewsecondarycolour <$> currentBrandedDomain ctx
    value "domainsignviewsecondarytextcolour" $ fromMaybe "" $ bdsignviewsecondarytextcolour <$> currentBrandedDomain ctx
    value "servicelinkcolour" $ fromMaybe "" $ bdservicelinkcolour <$> currentBrandedDomain ctx


companyJSON :: Monad m => Context -> Company -> CompanyUI -> m JSValue
companyJSON ctx company companyui = runJSONGenT $ do
    value "companyid" $ show $ companyid company
    value "address" $ companyaddress $ companyinfo company
    value "city" $ companycity $ companyinfo company
    value "country" $ companycountry $ companyinfo company
    value "zip" $ companyzip $ companyinfo company
    value "companyname" $ getCompanyName company
    value "companynumber" $ getCompanyNumber company
    value "smsoriginator" $ companysmsoriginator $ companyinfo company
    value "ipaddressmasklist" $ intercalate "," $ fmap show $ companyipaddressmasklist $ companyinfo company
    valueM "companyui" $ companyUIJson ctx companyui

documentSignviewBrandingJSON :: Monad m => Context -> User -> Company -> CompanyUI -> Document -> JSONGenT m ()
documentSignviewBrandingJSON ctx user company companyui document = do
    signviewBrandingJSON ctx user company companyui
    value "showheader" $ documentshowheader $ document
    value "showpdfdownload" $ documentshowpdfdownload $ document
    value "showrejectoption" $ documentshowrejectoption $ document
    value "showfooter" $ documentshowfooter $ document

signviewBrandingJSON :: Monad m => Context -> User -> Company -> CompanyUI -> JSONGenT m ()
signviewBrandingJSON ctx user company companyui = do
    let mdb = currentBrandedDomain ctx
    value "fullname" $ getFullName user
    value "email" $ getEmail user
    value "company" $ getCompanyName company
    value "phone" $ userphone $ userinfo user
    value "position" $ usercompanyposition$ userinfo $ user
    value "signviewlogo" $ if ((isJust $ companysignviewlogo companyui))
                                    then Just (show (LinkCompanySignViewLogo $ companyid $ company))
                                    else (bdlogolink <$> mdb)
    value "signviewtextcolour" $ companysignviewtextcolour $ companyui
    value "signviewtextfont" $ companysignviewtextfont   $ companyui
    value "signviewprimarycolour" $ (companysignviewprimarycolour $ companyui) `mplus` (bdsignviewprimarycolour <$> mdb)
    value "signviewprimarytextcolour" $ (companysignviewprimarytextcolour   $ companyui) `mplus` (bdsignviewprimarytextcolour <$> mdb)
    value "signviewsecondarycolour" $ (companysignviewsecondarycolour   $ companyui) `mplus` (bdsignviewsecondarycolour <$> mdb)
    value "signviewsecondarytextcolour" $ (companysignviewsecondarytextcolour   $ companyui) `mplus` (bdsignviewsecondarytextcolour <$> mdb)
    value "signviewbarscolour" $ (companysignviewbarscolour  $ companyui) `mplus` (bdbarscolour <$> mdb)
    value "signviewbarstextcolour" $ (companysignviewbarstextcolour $ companyui) `mplus` (bdbarstextcolour <$> mdb)
    value "signviewbackgroundcolour" $ (companysignviewbackgroundcolour $ companyui) `mplus` (bdbackgroundcolour <$> mdb)

userStatsToJSON :: (MinutesTime -> String) -> [UserUsageStats] -> [JSValue]
userStatsToJSON formatTime uuss = map tojson uuss
  where
    tojson uus = runJSONGen . object "fields" $ do
      value "date" (formatTime (fst (uusTimeSpan uus)))
      value "closed" (uusDocumentsClosed uus)
      value "sent" (uusDocumentsSent uus)
      value "signatures" (uusSignaturesClosed uus)

companyStatsToJSON :: (MinutesTime -> String) -> String -> [UserUsageStats] -> [JSValue]
companyStatsToJSON formatTime totalText uuss = map f summarized
  where
    uusGrouped :: [[UserUsageStats]]
    uusGrouped = groupBy sameTimespan uuss
    summarized :: [UserUsageStats]
    summarized = map summarize uusGrouped
    summarize :: [UserUsageStats] -> UserUsageStats
    summarize uuss' = foldl1' addTwo uuss'
    addTwo u1 u2 = u1 { uusDocumentsSent    = uusDocumentsSent u1    + uusDocumentsSent u2
                      , uusDocumentsClosed  = uusDocumentsClosed u1  + uusDocumentsClosed u2
                      , uusSignaturesClosed = uusSignaturesClosed u1 + uusSignaturesClosed u2
                      }
    sameTimespan u1 u2 = uusTimeSpan u1 == uusTimeSpan u2
    f uus = runJSONGen $ do
      object "fields" $ do
        value "date" (formatTime (fst (uusTimeSpan uus)))
        value "closed" (uusDocumentsClosed uus)
        value "sent" (uusDocumentsSent uus)
        value "signatures" (uusSignaturesClosed uus)
        value "name" totalText
      objects "subfields" $ do
         [do value "date" (formatTime (fst (uusTimeSpan uus')))
             value "closed" (uusDocumentsClosed uus')
             value "sent" (uusDocumentsSent uus')
             value "signatures" (uusSignaturesClosed uus')
             value "name" ((\(_,_,n) -> n) <$> uusUser uus')
             value "email" ((\(_,e,_) -> e) <$> uusUser uus')
           | uus' <- uuss,
             uusTimeSpan uus' == uusTimeSpan uus,
             ((uusDocumentsClosed uus' > 0) || (uusDocumentsSent uus' > 0) || (uusSignaturesClosed uus' > 0))
             ]



pageAcceptTOS :: TemplatesMonad m => m String
pageAcceptTOS = renderTemplate_ "pageAcceptTOS"

accessNewAccountMail :: TemplatesMonad m => Context -> User -> KontraLink -> m Mail
accessNewAccountMail ctx user setpasslink = do
  kontramail (ctxmailsconfig ctx) (currentBrandedDomain ctx)  "accessNewAccountMail" $ do
    F.value "personname"   $ getFullName user
    F.value "personemail"  $ getEmail user
    F.value "passwordlink" $ show setpasslink
    F.value "ctxhostpart"  $ ctxhostpart ctx
    brandingMailFields (currentBrandedDomain ctx) Nothing

resetPasswordMail :: TemplatesMonad m => Context -> User -> KontraLink -> m Mail
resetPasswordMail ctx user setpasslink = do
  kontramail (ctxmailsconfig ctx) (currentBrandedDomain ctx)   "passwordChangeLinkMail" $ do
    F.value "personname"   $ getFullName user
    F.value "personemail"  $ getEmail user
    F.value "passwordlink" $ show setpasslink
    F.value "ctxhostpart"  $ ctxhostpart ctx
    brandingMailFields (currentBrandedDomain ctx) Nothing

newUserMail :: TemplatesMonad m => Context -> String -> String -> KontraLink -> m Mail
newUserMail ctx emailaddress personname activatelink = do
  kontramail (ctxmailsconfig ctx) (currentBrandedDomain ctx)  "newUserMail" $ do
    F.value "personname"   $ personname
    F.value "email"        $ emailaddress
    F.value "activatelink" $ show activatelink
    F.value "ctxhostpart"  $ ctxhostpart ctx
    brandingMailFields (currentBrandedDomain ctx) Nothing


mailNewAccountCreatedByAdmin :: (HasLang a, TemplatesMonad m) => Context -> a -> String -> String -> KontraLink -> Maybe String -> m Mail
mailNewAccountCreatedByAdmin ctx lang personname email setpasslink custommessage = do
  kontramaillocal (ctxmailsconfig ctx) (currentBrandedDomain ctx)  lang "mailNewAccountCreatedByAdmin" $ do
    F.value "personname"    $ personname
    F.value "email"         $ email
    F.value "passwordlink"  $ show setpasslink
    F.value "creatorname"   $ maybe "" getSmartName (ctxmaybeuser ctx)
    F.value "ctxhostpart"   $ ctxhostpart ctx
    F.value "custommessage"   custommessage
    brandingMailFields (currentBrandedDomain ctx) Nothing


mailEmailChangeRequest :: (TemplatesMonad m, HasSomeUserInfo a) => Context -> a -> Email -> KontraLink -> m Mail
mailEmailChangeRequest ctx user newemail link = do
  kontramail (ctxmailsconfig ctx) (currentBrandedDomain ctx)  "mailRequestChangeEmail" $ do
    F.value "fullname" $ getFullName user
    F.value "newemail" $ unEmail newemail
    F.value "ctxhostpart" $ ctxhostpart ctx
    F.value "link" $ show link
    brandingMailFields (currentBrandedDomain ctx) Nothing

-------------------------------------------------------------------------------

pageDoYouWantToChangeEmail :: TemplatesMonad m => Email -> m String
pageDoYouWantToChangeEmail newemail =
  renderTemplate "pageDoYouWantToChangeEmail" $ do
                 F.value "newemail" $ unEmail newemail

flashMessageLoginRedirectReason :: TemplatesMonad m => LoginRedirectReason -> m (Maybe FlashMessage)
flashMessageLoginRedirectReason reason =
  case reason of
       LoginTry             -> return Nothing
       NotLogged            -> render "notlogged"
       NotLoggedAsSuperUser -> render "notsu"
       InvalidLoginInfo _   -> render "invloginfo"
  where
    render msg = Just . toFlashMsg OperationFailed <$>
      (renderTemplate "flashMessageLoginPageRedirectReason" $ F.value msg True)

flashMessageUserDetailsSaved :: TemplatesMonad m => m FlashMessage
flashMessageUserDetailsSaved =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageUserDetailsSaved"

flashMessageMustAcceptTOS :: TemplatesMonad m => m FlashMessage
flashMessageMustAcceptTOS =
  toFlashMsg SigningRelated <$> renderTemplate_ "flashMessageMustAcceptTOS"

flashMessagePasswordsDontMatch :: TemplatesMonad m => m FlashMessage
flashMessagePasswordsDontMatch =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessagePasswordsDontMatch"


flashMessageUserPasswordChanged :: TemplatesMonad m => m FlashMessage
flashMessageUserPasswordChanged =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageUserPasswordChanged"

flashMessagePasswordChangeLinkNotValid :: TemplatesMonad m => m FlashMessage
flashMessagePasswordChangeLinkNotValid =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessagePasswordChangeLinkNotValid"

flashMessageAccessNewAccountLinkNotValid :: TemplatesMonad m => m FlashMessage
flashMessageAccessNewAccountLinkNotValid =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageAccessNewAccountLinkNotValid"


flashMessageUserWithSameEmailExists :: TemplatesMonad m => m FlashMessage
flashMessageUserWithSameEmailExists =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageUserWithSameEmailExists"


flashMessageUserActivated :: TemplatesMonad m => m FlashMessage
flashMessageUserActivated =
  toFlashMsg SigningRelated <$> renderTemplate_ "flashMessageUserActivated"


flashMessageNewActivationLinkSend :: TemplatesMonad m => m FlashMessage
flashMessageNewActivationLinkSend =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageNewActivationLinkSend"


flashMessageProblemWithEmailChange :: TemplatesMonad m => m FlashMessage
flashMessageProblemWithEmailChange =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageProblemWithEmailChange"

flashMessageProblemWithPassword :: TemplatesMonad m => m FlashMessage
flashMessageProblemWithPassword =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageProblemWithPassword"

flashMessageYourEmailHasChanged :: TemplatesMonad m => m FlashMessage
flashMessageYourEmailHasChanged =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageYourEmailHasChanged"

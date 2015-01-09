{-# LANGUAGE ExtendedDefaultRules #-}
module User.UserView (
    -- pages
    userJSON,
    companyJSON,
    documentSignviewBrandingJSON,
    signviewBrandingJSON,
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
import Control.Monad.Catch
import Data.List
import Text.JSON
import Text.JSON.Gen
import Text.StringTemplate.GenericStandard()
import Text.StringTemplate.GenericStandard()
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.BrandedDomain
import Company.Model
import DB
import Doc.DocStateData
import Doc.DocViewMail
import FlashMessage
import Kontra
import KontraLink
import Mails.SendMail(Mail, kontramail, kontramaillocal)
import MinutesTime
import Theme.Model
import User.Email
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo

userJSON :: Monad m => User -> Company ->  m JSValue
userJSON user company = runJSONGenT $ do
    value "id" $ show $ userid user
    value "fstname" $ getFirstName user
    value "sndname" $ getLastName user
    value "email" $ getEmail user
    value "personalnumber" $ getPersonalNumber user
    value "phone" $ userphone $ userinfo user
    value "companyadmin" $ useriscompanyadmin user
    value "companyposition" $ usercompanyposition $ userinfo user
    value "lang"   $ codeFromLang $ getLang user
    valueM "company" $ companyJSON company

companyJSON :: Monad m => Company -> m JSValue
companyJSON company = runJSONGenT $ do
    value "companyid" $ show $ companyid company
    value "address" $ companyaddress $ companyinfo company
    value "city" $ companycity $ companyinfo company
    value "country" $ companycountry $ companyinfo company
    value "zip" $ companyzip $ companyinfo company
    value "companyname" $ getCompanyName company
    value "companynumber" $ getCompanyNumber company
    value "cgidisplayname" $ companycgidisplayname $ companyinfo company
    value "ipaddressmasklist" $ intercalate "," $ fmap show $ companyipaddressmasklist $ companyinfo company
    value "allowsavesafetycopy" $ companyallowsavesafetycopy (companyinfo company)
    value "idledoctimeout" $ companyidledoctimeout $ companyinfo company

documentSignviewBrandingJSON :: Monad m => User -> Company -> Document -> JSONGenT m ()
documentSignviewBrandingJSON user company document = do
    signviewBrandingJSON user company
    value "showheader" $ documentshowheader $ document
    value "showpdfdownload" $ documentshowpdfdownload $ document
    value "showrejectoption" $ documentshowrejectoption $ document
    value "showfooter" $ documentshowfooter $ document

signviewBrandingJSON :: Monad m => User -> Company -> JSONGenT m ()
signviewBrandingJSON user company = do
    value "fullname" $ getFullName user
    value "email" $ getEmail user
    value "company" $ getCompanyName company
    value "phone" $ userphone $ userinfo user
    value "position" $ usercompanyposition$ userinfo $ user
    value "allowsavesafetycopy" $ companyallowsavesafetycopy (companyinfo company)

userStatsToJSON :: (UTCTime -> String) -> [UserUsageStats] -> [JSValue]
userStatsToJSON formatTime uuss = map tojson uuss
  where
    tojson uus = runJSONGen . object "fields" $ do
      value "date" (formatTime (fst (uusTimeSpan uus)))
      value "closed" (uusDocumentsClosed uus)
      value "sent" (uusDocumentsSent uus)
      value "signatures" (uusSignaturesClosed uus)

companyStatsToJSON :: (UTCTime -> String) -> String -> [UserUsageStats] -> [JSValue]
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

accessNewAccountMail :: (TemplatesMonad m,MonadDB m,MonadThrow m) => Context -> User -> KontraLink -> m Mail
accessNewAccountMail ctx user setpasslink = do
  theme <- dbQuery $ GetTheme $ bdMailTheme (ctxbrandeddomain ctx)
  kontramail (ctxbrandeddomain ctx) theme "accessNewAccountMail" $ do
    F.value "personname"   $ getFullName user
    F.value "personemail"  $ getEmail user
    F.value "passwordlink" $ show setpasslink
    F.value "ctxhostpart"  $ ctxhostpart ctx
    brandingMailFields theme

resetPasswordMail :: (TemplatesMonad m,MonadDB m,MonadThrow m) => Context -> User -> KontraLink -> m Mail
resetPasswordMail ctx user setpasslink = do
  theme <- dbQuery $ GetTheme $ bdMailTheme (ctxbrandeddomain ctx)
  kontramail (ctxbrandeddomain ctx) theme  "passwordChangeLinkMail" $ do
    F.value "personname"   $ getFullName user
    F.value "personemail"  $ getEmail user
    F.value "passwordlink" $ show setpasslink
    F.value "ctxhostpart"  $ ctxhostpart ctx
    brandingMailFields theme

newUserMail :: (TemplatesMonad m,MonadDB m,MonadThrow m) => Context -> String -> String -> KontraLink -> m Mail
newUserMail ctx emailaddress personname activatelink = do
  theme <- dbQuery $ GetTheme $ bdMailTheme (ctxbrandeddomain ctx)
  kontramail (ctxbrandeddomain ctx) theme  "newUserMail" $ do
    F.value "personname"   $ personname
    F.value "email"        $ emailaddress
    F.value "activatelink" $ show activatelink
    F.value "ctxhostpart"  $ ctxhostpart ctx
    brandingMailFields theme


mailNewAccountCreatedByAdmin :: (HasLang a,MonadDB m,MonadThrow m, TemplatesMonad m) => Context -> a -> String -> String -> KontraLink -> m Mail
mailNewAccountCreatedByAdmin ctx lang personname email setpasslink = do
  theme <- dbQuery $ GetTheme $ bdMailTheme (ctxbrandeddomain ctx)
  kontramaillocal (ctxbrandeddomain ctx) theme lang "mailNewAccountCreatedByAdmin" $ do
    F.value "personname"    $ personname
    F.value "email"         $ email
    F.value "passwordlink"  $ show setpasslink
    F.value "creatorname"   $ maybe "" getSmartName (ctxmaybeuser ctx)
    F.value "ctxhostpart"   $ ctxhostpart ctx
    brandingMailFields theme 


mailEmailChangeRequest :: (TemplatesMonad m, HasSomeUserInfo a,MonadDB m,MonadThrow m) => Context -> a -> Email -> KontraLink -> m Mail
mailEmailChangeRequest ctx user newemail link = do
  theme <- dbQuery $ GetTheme $ bdMailTheme (ctxbrandeddomain ctx)
  kontramail (ctxbrandeddomain ctx) theme "mailRequestChangeEmail" $ do
    F.value "fullname" $ getFullName user
    F.value "newemail" $ unEmail newemail
    F.value "ctxhostpart" $ ctxhostpart ctx
    F.value "link" $ show link
    brandingMailFields theme

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
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageMustAcceptTOS"

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
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageUserActivated"


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

{-# LANGUAGE ExtendedDefaultRules #-}
module User.UserView (
    -- pages
    userJSON,
    companyJSON,
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
    flashMessageUserPasswordChanged,
    flashMessagePasswordChangeLinkNotValid,
    flashMessageAccessNewAccountLinkNotValid,
    flashMessageUserActivated,
    flashMessageNewActivationLinkSend,
    flashMessageProblemWithEmailChange,
    flashMessageProblemWithPassword,
    flashMessageYourEmailHasChanged,

    userStatsToJSON,
    companyStatsToJSON,
    ) where

import Control.Monad.Catch
import Text.JSON
import Text.JSON.Gen
import Text.StringTemplate.GenericStandard()
import Text.StringTemplate.GenericStandard()
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.BrandedDomain
import Company.Model
import DB
import Doc.DocViewMail
import FlashMessage
import Kontra
import KontraLink
import KontraPrelude
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
    value "smsprovider" $ show . companysmsprovider . companyinfo $ company

userStatsToJSON :: (UTCTime -> String) -> [UserUsageStats] -> JSValue
userStatsToJSON formatTime uuss = runJSONGen . objects "stats" . for uuss $ \uus -> do
  let DocumentStats{..} = uusDocumentStats uus
  value "date" . formatTime $ uusTimeWindowStart uus
  value "sent" dsDocumentsSent
  value "closed" dsDocumentsClosed
  value "signatures" dsSignaturesClosed

companyStatsToJSON :: (UTCTime -> String) -> String -> [UserUsageStats] -> JSValue
companyStatsToJSON formatTime textName uuss = runJSONGen . objects "stats" . for uussGrouped $ \uusGroup -> do
  let summary = foldMap uusDocumentStats uusGroup
  value "date" . formatTime . uusTimeWindowStart $ $head uusGroup
  value "name" textName
  value "sent" $ dsDocumentsSent summary
  value "closed" $ dsDocumentsClosed summary
  value "signatures" $ dsSignaturesClosed summary
  objects "user_stats" . for uusGroup $ \uus -> do
    let DocumentStats{..} = uusDocumentStats uus
    value "date" . formatTime $ uusTimeWindowStart uus
    value "email" $ uusUserEmail uus
    value "name" $ uusUserName uus
    value "sent" dsDocumentsSent
    value "closed" dsDocumentsClosed
    value "signatures" dsSignaturesClosed
  where
    uussGrouped :: [[UserUsageStats]]
    uussGrouped = groupBy sameTimeWindow uuss
      where
        sameTimeWindow u1 u2 = uusTimeWindowStart u1 == uusTimeWindowStart u2

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

flashMessageUserPasswordChanged :: TemplatesMonad m => m FlashMessage
flashMessageUserPasswordChanged =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageUserPasswordChanged"

flashMessagePasswordChangeLinkNotValid :: TemplatesMonad m => m FlashMessage
flashMessagePasswordChangeLinkNotValid =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessagePasswordChangeLinkNotValid"

flashMessageAccessNewAccountLinkNotValid :: TemplatesMonad m => m FlashMessage
flashMessageAccessNewAccountLinkNotValid =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageAccessNewAccountLinkNotValid"

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

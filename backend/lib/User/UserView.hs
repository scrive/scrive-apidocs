{-# LANGUAGE ExtendedDefaultRules #-}
module User.UserView (
    -- pages
    pageAcceptTOS,
    pageDoYouWantToChangeEmail,

    -- mails
    newUserMail,
    mailNewAccountCreatedByAdmin,
    resetPasswordMail,
    mailEmailChangeRequest,
    mailEmailChangeRequestedNotification,

    -- flash messages
    flashMessageLoginRedirect,
    flashMessagePasswordChangeLinkNotValid,
    flashMessageNewActivationLinkSend,
    flashMessageProblemWithEmailChange,
    flashMessageProblemWithPassword,
    flashMessageYourEmailHasChanged,
    flashMessageUserAccountRequestExpired,
    flashMessageUserAccountRequestExpiredCompany,
    flashMessageTotpMustBeActivated
    ) where

import Control.Monad.Catch
import Text.StringTemplate.GenericStandard ()
import Text.StringTemplate.GenericStandard ()
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import AppView
import DB
import Doc.DocViewMail
import FlashMessage
import Kontra
import KontraLink
import Mails.SendMail (Mail, kontramail, kontramaillocal)
import Templates (renderTextTemplate)
import Theme.Model
import User.Email
import User.Model
import Util.HasSomeUserInfo

pageAcceptTOS :: TemplatesMonad m => Context -> m Text
pageAcceptTOS ctx = renderTextTemplate "pageAcceptTOS" $ entryPointFields ctx

resetPasswordMail
  :: (TemplatesMonad m, MonadDB m, MonadThrow m)
  => Context
  -> User
  -> KontraLink
  -> m Mail
resetPasswordMail ctx user setpasslink = do
  theme <- dbQuery . GetTheme $ ctx ^. #brandedDomain % #mailTheme
  kontramail (ctx ^. #mailNoreplyAddress)
             (ctx ^. #brandedDomain)
             theme
             "passwordChangeLinkMail"
    $ do
        F.value "personemail" $ getEmail user
        F.value "passwordlink" $ show setpasslink
        F.value "ctxhostpart" $ ctx ^. #brandedDomain % #url
        brandingMailFields theme

newUserMail
  :: (TemplatesMonad m, MonadDB m, MonadThrow m)
  => Context
  -> Text
  -> KontraLink
  -> m Mail
newUserMail ctx emailaddress activatelink = do
  theme <- dbQuery $ GetTheme $ ctx ^. #brandedDomain % #mailTheme
  kontramail (ctx ^. #mailNoreplyAddress) (ctx ^. #brandedDomain) theme "newUserMail" $ do
    F.value "email" $ emailaddress
    F.value "activatelink" $ show activatelink
    F.value "ctxhostpart" $ ctx ^. #brandedDomain % #url
    brandingMailFields theme


mailNewAccountCreatedByAdmin
  :: (HasLang a, MonadDB m, MonadThrow m, TemplatesMonad m)
  => Context
  -> a
  -> Text
  -> KontraLink
  -> m Mail
mailNewAccountCreatedByAdmin ctx lang email setpasslink = do
  theme <- dbQuery $ GetTheme $ ctx ^. #brandedDomain % #mailTheme
  kontramaillocal (ctx ^. #mailNoreplyAddress)
                  (ctx ^. #brandedDomain)
                  theme
                  lang
                  "mailNewAccountCreatedByAdmin"
    $ do
        F.value "email" $ email
        F.value "passwordlink" $ show setpasslink
        F.value "creatorname" $ maybe "" getSmartName (ctx ^. #maybeUser)
        F.value "ctxhostpart" $ ctx ^. #brandedDomain % #url
        brandingMailFields theme


mailEmailChangeRequest
  :: (TemplatesMonad m, MonadDB m, MonadThrow m)
  => Context
  -> Maybe User
  -> User
  -> Email
  -> KontraLink
  -> m Mail
mailEmailChangeRequest ctx requestingUser changedUser newemail link = do
  theme <- dbQuery $ GetTheme $ ctx ^. #brandedDomain % #mailTheme
  kontramail (ctx ^. #mailNoreplyAddress)
             (ctx ^. #brandedDomain)
             theme
             "mailRequestChangeEmail"
    $ do
        F.value "fullname" $ getFullName changedUser
        F.value "newemail" $ unEmail newemail
        F.value "ctxhostpart" $ ctx ^. #brandedDomain % #url
        F.value "link" $ show link
        F.value "requestedby" $ getSmartName <$> requestingUser
        brandingMailFields theme

mailEmailChangeRequestedNotification
  :: (TemplatesMonad m, MonadDB m, MonadThrow m)
  => Context
  -> Bool
  -> User
  -> Email
  -> m Mail
mailEmailChangeRequestedNotification ctx requestedByAdmin changedUser newemail = do
  theme <- dbQuery $ GetTheme $ ctx ^. #brandedDomain % #mailTheme
  kontramail (ctx ^. #mailNoreplyAddress)
             (ctx ^. #brandedDomain)
             theme
             "mailEmailChangeRequestedNotification"
    $ do
        F.value "fullname" $ getFullName changedUser
        F.value "newemail" $ unEmail newemail
        F.value "requestedbyadmin" $ requestedByAdmin
        brandingMailFields theme

-------------------------------------------------------------------------------

pageDoYouWantToChangeEmail :: TemplatesMonad m => Context -> Email -> m Text
pageDoYouWantToChangeEmail ctx newemail =
  renderTextTemplate "pageDoYouWantToChangeEmail" $ do
    F.value "newemail" $ unEmail newemail
    entryPointFields ctx

flashMessageLoginRedirect :: TemplatesMonad m => m FlashMessage
flashMessageLoginRedirect =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageLoginPageRedirectReason"

flashMessagePasswordChangeLinkNotValid :: TemplatesMonad m => m FlashMessage
flashMessagePasswordChangeLinkNotValid =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessagePasswordChangeLinkNotValid"

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

flashMessageUserAccountRequestExpiredCompany :: TemplatesMonad m => m FlashMessage
flashMessageUserAccountRequestExpiredCompany = toFlashMsg OperationFailed
  <$> renderTemplate_ "flashMessageUserAccountRequestExpiredCompany"

flashMessageUserAccountRequestExpired :: TemplatesMonad m => m FlashMessage
flashMessageUserAccountRequestExpired =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageUserAccountRequestExpired"

flashMessageTotpMustBeActivated :: TemplatesMonad m => m FlashMessage
flashMessageTotpMustBeActivated =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageTotpMustBeActivated"

module CompanyAccounts.CompanyAccountsView (
    -- pages
    viewCompanyAccounts,

    -- mails
    mailNewCompanyUserInvite,
    mailTakeoverPrivateUserInvite,
    mailTakeoverCompanyUserInfo,

    -- modals
    modalDoYouWantToBeCompanyAccount,

    -- flash messages
    flashMessageCompanyAccountInviteSent,
    flashMessageCompanyAccountInviteResent,
    flashMessageUserHasBecomeCompanyAccount,
    flashMessageUserHasLiveDocs,
    flashMessageCompanyAccountDeleted
    ) where

import Control.Applicative ((<$>))

import FlashMessage
import KontraLink
import ListUtil
import Mails.SendMail(Mail)
import Templates.Templates
import Templates.TemplatesUtils
import Text.StringTemplate.GenericStandard()
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo

viewCompanyAccounts :: TemplatesMonad m => m String
viewCompanyAccounts =
  renderTemplateFM "viewCompanyAccounts" $
    field "currentlink" $ show $ LinkCompanyAccounts emptyListParams

----------------------------------------------------------------------------

mailNewCompanyUserInvite :: (TemplatesMonad m,  HasSomeUserInfo a, HasLocale a, HasSomeUserInfo b, HasSomeCompanyInfo c) =>
                               String -> a -> b -> c -> KontraLink -> m Mail
mailNewCompanyUserInvite hostpart invited inviter company link =
  kontramail "mailNewCompanyUserInvite" $ do
    basicCompanyInviteFields invited inviter company
    basicLinkFields hostpart link

mailTakeoverPrivateUserInvite :: (TemplatesMonad m,  HasSomeUserInfo a, HasLocale a, HasSomeUserInfo b, HasSomeCompanyInfo c) =>
                               String -> a -> b -> c -> KontraLink -> m Mail
mailTakeoverPrivateUserInvite hostpart invited inviter company link =
  --invite in the language of the existing user rather than in the inviter's language
  kontramaillocal invited  "mailTakeoverPrivateUserInvite" $ do
    basicCompanyInviteFields invited inviter company
    basicLinkFields hostpart link

mailTakeoverCompanyUserInfo :: (TemplatesMonad m,  HasSomeUserInfo a, HasLocale a, HasSomeUserInfo b, HasSomeCompanyInfo c) =>
                               a -> b -> c -> m Mail
mailTakeoverCompanyUserInfo invited inviter company =
  --send info in the language of the existing user rather than in the inviter's language
  kontramaillocal invited  "mailTakeoverCompanyUserInfo" $
    basicCompanyInviteFields invited inviter company

basicCompanyInviteFields :: (TemplatesMonad m, HasSomeUserInfo a, HasSomeUserInfo b, HasSomeCompanyInfo c) =>
                            a -> b -> c -> Fields m
basicCompanyInviteFields invited inviter company = do
  field "invitedname" $ getFullName invited
  field "invitedemail" $ getEmail invited
  field "invitername" $ getSmartName inviter
  field "companyname" $ getCompanyName company

basicLinkFields :: TemplatesMonad m => String -> KontraLink -> Fields m
basicLinkFields hostpart link = do
  field "ctxhostpart" hostpart
  field "link" $ show link

-------------------------------------------------------------------------------

modalDoYouWantToBeCompanyAccount :: (TemplatesMonad m,  HasSomeCompanyInfo c) => c -> m FlashMessage
modalDoYouWantToBeCompanyAccount company =
  toModal <$> renderTemplateFM "modalDoYouWantToBeCompanyAccount"
                 (field "companyname" $ getCompanyName company)

-------------------------------------------------------------------------------

flashMessageCompanyAccountInviteSent :: TemplatesMonad m => m FlashMessage
flashMessageCompanyAccountInviteSent =
  toFlashMsg OperationDone <$> renderTemplateM "flashCompanyAccountInviteSent" ()

flashMessageCompanyAccountInviteResent :: TemplatesMonad m => m FlashMessage
flashMessageCompanyAccountInviteResent =
  toFlashMsg OperationDone <$> renderTemplateM "flashCompanyAccountInviteResent" ()

flashMessageUserHasBecomeCompanyAccount :: (TemplatesMonad m,  HasSomeCompanyInfo c) => c -> m FlashMessage
flashMessageUserHasBecomeCompanyAccount company =
  toFlashMsg OperationDone <$> renderTemplateFM "flashMessageUserHasBecomeCompanyAccount"
                                  (field "companyname" $ getCompanyName company)

flashMessageUserHasLiveDocs :: TemplatesMonad m => m FlashMessage
flashMessageUserHasLiveDocs =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageUserHasLiveDocs" ()

flashMessageCompanyAccountDeleted :: TemplatesMonad m => m FlashMessage
flashMessageCompanyAccountDeleted =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageCompanyAccountDeleted" ()

-------------------------------------------------------------------------------

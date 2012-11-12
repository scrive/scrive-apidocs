module CompanyAccounts.CompanyAccountsView (
    -- mails
    mailNewCompanyUserInvite,
    mailTakeoverPrivateUserInvite,

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
import Mails.SendMail(Mail)
import Templates.Templates
import Templates.TemplatesUtils
import User.Model
import Company.Model
import Doc.DocViewMail
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import qualified Templates.Fields as F

----------------------------------------------------------------------------

mailNewCompanyUserInvite :: (TemplatesMonad m, HasSomeUserInfo a, HasLang a, HasSomeUserInfo b) =>
                               String -> a -> b -> Company -> KontraLink -> m Mail
mailNewCompanyUserInvite hostpart invited inviter company link =
  kontramail "mailNewCompanyUserInvite" $ do
    basicCompanyInviteFields invited inviter company
    basicLinkFields hostpart link
    F.object "companybrand" $ companyBrandFields company


mailTakeoverPrivateUserInvite :: (TemplatesMonad m,  HasSomeUserInfo a, HasLang a, HasSomeUserInfo b) =>
                               String -> a -> b -> Company -> KontraLink -> m Mail
mailTakeoverPrivateUserInvite hostpart invited inviter company link =
  --invite in the language of the existing user rather than in the inviter's language
  kontramaillocal invited  "mailTakeoverPrivateUserInvite" $ do
    basicCompanyInviteFields invited inviter company
    basicLinkFields hostpart link
    F.object "companybrand" $ companyBrandFields company

basicCompanyInviteFields :: (TemplatesMonad m, HasSomeUserInfo a, HasSomeUserInfo b, HasSomeCompanyInfo c) => a -> b -> c -> Fields m ()
basicCompanyInviteFields invited inviter company = do
  F.value "invitedname" $ getFullName invited
  F.value "invitedemail" $ getEmail invited
  F.value "invitername" $ getSmartName inviter
  F.value "companyname" $ getCompanyName company

basicLinkFields :: TemplatesMonad m => String -> KontraLink -> Fields m ()
basicLinkFields hostpart link = do
  F.value "ctxhostpart" hostpart
  F.value "link" $ show link


-------------------------------------------------------------------------------

modalDoYouWantToBeCompanyAccount :: (TemplatesMonad m,  HasSomeCompanyInfo c) => c -> m FlashMessage
modalDoYouWantToBeCompanyAccount company =
  toModal <$> renderTemplate "modalDoYouWantToBeCompanyAccount"
                 (F.value "companyname" $ getCompanyName company)

-------------------------------------------------------------------------------

flashMessageCompanyAccountInviteSent :: TemplatesMonad m => m FlashMessage
flashMessageCompanyAccountInviteSent =
  toFlashMsg OperationDone <$> renderTemplate_ "flashCompanyAccountInviteSent"

flashMessageCompanyAccountInviteResent :: TemplatesMonad m => m FlashMessage
flashMessageCompanyAccountInviteResent =
  toFlashMsg OperationDone <$> renderTemplate_ "flashCompanyAccountInviteResent"

flashMessageUserHasBecomeCompanyAccount :: (TemplatesMonad m,  HasSomeCompanyInfo c) => c -> m FlashMessage
flashMessageUserHasBecomeCompanyAccount company =
  toFlashMsg OperationDone <$> renderTemplate "flashMessageUserHasBecomeCompanyAccount"
                                  (F.value "companyname" $ getCompanyName company)

flashMessageUserHasLiveDocs :: TemplatesMonad m => m FlashMessage
flashMessageUserHasLiveDocs =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageUserHasLiveDocs"

flashMessageCompanyAccountDeleted :: TemplatesMonad m => m FlashMessage
flashMessageCompanyAccountDeleted =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageCompanyAccountDeleted"

-------------------------------------------------------------------------------

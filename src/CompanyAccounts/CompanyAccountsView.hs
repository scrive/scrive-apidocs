module CompanyAccounts.CompanyAccountsView (
    -- mails
    mailNewCompanyUserInvite,
    mailTakeoverPrivateUserInvite,

    -- pages
    pageDoYouWantToBeCompanyAccount,

    -- flash messages
    flashMessageUserHasBecomeCompanyAccount
    ) where

import Control.Applicative ((<$>))

import FlashMessage
import KontraLink
import Mails.SendMail(Mail, kontramail, kontramaillocal)
import Text.StringTemplates.Templates
import User.Model
import Company.Model
import Doc.DocViewMail
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import qualified Text.StringTemplates.Fields as F
import Context

----------------------------------------------------------------------------

mailNewCompanyUserInvite :: (TemplatesMonad m, HasSomeUserInfo a, HasLang a, HasSomeUserInfo b) =>
                               Context -> a -> b -> Company -> KontraLink -> m Mail
mailNewCompanyUserInvite ctx invited inviter company link =
  kontramail "mailNewCompanyUserInvite" $ do
    basicCompanyInviteFields invited inviter company
    basicLinkFields (ctxhostpart ctx) link
    F.object "companybrand" $ brandingMailFields (currentBrandedDomain ctx) (Just company)
    F.value "creatorname" $ getSmartName inviter


mailTakeoverPrivateUserInvite :: (TemplatesMonad m,  HasSomeUserInfo a, HasLang a, HasSomeUserInfo b) =>
                               Context -> a -> b -> Company -> KontraLink -> m Mail
mailTakeoverPrivateUserInvite ctx invited inviter company link =
  --invite in the language of the existing user rather than in the inviter's language
  kontramaillocal invited  "mailTakeoverPrivateUserInvite" $ do
    basicCompanyInviteFields invited inviter company
    basicLinkFields (ctxhostpart ctx) link
    F.object "companybrand" $ brandingMailFields (currentBrandedDomain ctx) (Just company)

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

pageDoYouWantToBeCompanyAccount :: (TemplatesMonad m,  HasSomeCompanyInfo c) => c -> m String
pageDoYouWantToBeCompanyAccount company =
  renderTemplate "pageDoYouWantToBeCompanyAccount" $ do
      F.value "companyname" $ getCompanyName company

-------------------------------------------------------------------------------

flashMessageUserHasBecomeCompanyAccount :: (TemplatesMonad m,  HasSomeCompanyInfo c) => c -> m FlashMessage
flashMessageUserHasBecomeCompanyAccount company =
  toFlashMsg OperationDone <$> renderTemplate "flashMessageUserHasBecomeCompanyAccount"
                                  (F.value "companyname" $ getCompanyName company)

-------------------------------------------------------------------------------

module CompanyAccounts.CompanyAccountsView (
    -- mails
    mailNewCompanyUserInvite,
    mailTakeoverSingleUserInvite,

    -- pages
    pageDoYouWantToBeCompanyAccount,

    -- flash messages
    flashMessageUserHasBecomeCompanyAccount,
    flashMessageBecomeCompanyLogInDifferentUser
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Catch
import Data.Maybe
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.BrandedDomain
import Company.CompanyUI
import Company.Model
import Context
import DB
import Doc.DocViewMail
import FlashMessage
import KontraLink
import Mails.SendMail(Mail, kontramail, kontramaillocal)
import Theme.Model
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo

----------------------------------------------------------------------------

mailNewCompanyUserInvite :: (TemplatesMonad m, MonadDB m,MonadThrow m, HasSomeUserInfo a, HasLang a, HasSomeUserInfo b) =>
                               Context -> a -> b -> Company -> CompanyUI -> KontraLink -> m Mail
mailNewCompanyUserInvite ctx invited inviter company companyui link = do
  theme <- dbQuery $ GetTheme $ fromMaybe (bdMailTheme (ctxbrandeddomain ctx)) (companyMailTheme companyui)
  kontramail (ctxbrandeddomain ctx) theme "mailNewCompanyUserInvite" $ do
    basicCompanyInviteFields invited inviter company
    basicLinkFields (ctxhostpart ctx) link
    brandingMailFields theme
    F.value "creatorname" $ getSmartName inviter


mailTakeoverSingleUserInvite :: (TemplatesMonad m, MonadDB m,MonadThrow m, HasSomeUserInfo a, HasLang a, HasSomeUserInfo b) =>
                               Context -> a -> b -> Company -> CompanyUI -> KontraLink -> m Mail
mailTakeoverSingleUserInvite ctx invited inviter company companyui link = do
  theme <- dbQuery $ GetTheme $ fromMaybe (bdMailTheme (ctxbrandeddomain ctx)) (companyMailTheme companyui)
  --invite in the language of the existing user rather than in the inviter's language
  kontramaillocal (ctxbrandeddomain ctx) theme invited  "mailTakeoverSingleUserInvite" $ do
    basicCompanyInviteFields invited inviter company
    basicLinkFields (ctxhostpart ctx) link
    brandingMailFields theme 

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

flashMessageBecomeCompanyLogInDifferentUser :: (TemplatesMonad m) => m FlashMessage
flashMessageBecomeCompanyLogInDifferentUser  =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageBecomeCompanyLogInDifferentUser"

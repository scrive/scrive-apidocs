module UserGroupAccounts.UserGroupAccountsView (
    -- mails
    mailNewUserGroupUserInvite,
    mailTakeoverSingleUserInvite,

    -- pages
    pageDoYouWantToBeCompanyAccount,

    -- flash messages
    flashMessageUserHasBecomeCompanyAccount,
    flashMessageBecomeCompanyLogInDifferentUser
    ) where

import Control.Monad.Catch
import Data.Time
import Text.StringTemplates.Templates
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import AppView
import BrandedDomain.BrandedDomain
import Context
import DB
import Doc.DocViewMail
import FlashMessage
import KontraLink
import Mails.SendMail (Mail, kontramail, kontramaillocal)
import MinutesTime
import Theme.Model
import User.Model
import UserGroup.Data
import Util.HasSomeUserInfo

----------------------------------------------------------------------------

mailNewUserGroupUserInvite :: (TemplatesMonad m, MonadDB m,MonadThrow m, HasSomeUserInfo a, HasLang a, HasSomeUserInfo b) =>
                               Context -> a -> b -> UserGroup -> KontraLink -> UTCTime -> m Mail
mailNewUserGroupUserInvite ctx invited inviter ug link expires = do
  theme <- dbQuery . GetTheme . fromMaybe (get (bdMailTheme . ctxbrandeddomain) ctx) . get (uguiMailTheme . ugUI) $ ug
  kontramail (get ctxmailnoreplyaddress ctx) (get ctxbrandeddomain ctx) theme "mailNewCompanyUserInvite" $ do
    basicUserGroupInviteFields invited inviter ug
    basicLinkFields (get ctxDomainUrl ctx) link
    brandingMailFields theme
    F.value "creatorname" $ getSmartName inviter
    F.value "expiredate" $ formatTimeYMD expires


mailTakeoverSingleUserInvite :: (TemplatesMonad m, MonadDB m,MonadThrow m, HasSomeUserInfo a, HasLang a, HasSomeUserInfo b) =>
                               Context -> a -> b -> UserGroup -> KontraLink -> m Mail
mailTakeoverSingleUserInvite ctx invited inviter ug link = do
  theme <- dbQuery . GetTheme . fromMaybe (get (bdMailTheme . ctxbrandeddomain) ctx) . get (uguiMailTheme . ugUI) $ ug
  --invite in the language of the existing user rather than in the inviter's language
  kontramaillocal (get ctxmailnoreplyaddress ctx) (get ctxbrandeddomain ctx) theme invited  "mailTakeoverSingleUserInvite" $ do
    basicUserGroupInviteFields invited inviter ug
    basicLinkFields (get ctxDomainUrl ctx) link
    brandingMailFields theme

basicUserGroupInviteFields :: (TemplatesMonad m, HasSomeUserInfo a, HasSomeUserInfo b) => a -> b -> UserGroup -> Fields m ()
basicUserGroupInviteFields invited inviter ug = do
  F.value "invitedname" $ getFullName invited
  F.value "invitedemail" $ getEmail invited
  F.value "invitername" $ getSmartName inviter
  F.value "companyname" . get ugName $ ug

basicLinkFields :: TemplatesMonad m => String -> KontraLink -> Fields m ()
basicLinkFields hostpart link = do
  F.value "ctxhostpart" hostpart
  F.value "link" $ show link


-------------------------------------------------------------------------------

pageDoYouWantToBeCompanyAccount :: (TemplatesMonad m) => Context -> UserGroup -> m String
pageDoYouWantToBeCompanyAccount ctx ug =
  renderTemplate "pageDoYouWantToBeCompanyAccount" $ do
    F.value "companyname" . get ugName $ ug
    entryPointFields ctx
-------------------------------------------------------------------------------

flashMessageUserHasBecomeCompanyAccount :: (TemplatesMonad m) => UserGroup -> m FlashMessage
flashMessageUserHasBecomeCompanyAccount ug =
  toFlashMsg OperationDone <$> renderTemplate "flashMessageUserHasBecomeCompanyAccount"
                                  (F.value "companyname" . T.unpack . get ugName $ ug)

-------------------------------------------------------------------------------

flashMessageBecomeCompanyLogInDifferentUser :: (TemplatesMonad m) => m FlashMessage
flashMessageBecomeCompanyLogInDifferentUser  =
  toFlashMsg OperationFailed <$> renderTemplate_ "flashMessageBecomeCompanyLogInDifferentUser"

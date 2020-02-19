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
import Context
import DB
import Doc.DocViewMail
import FlashMessage
import KontraLink
import Mails.SendMail (Mail, kontramaillocal)
import MinutesTime
import Templates (renderTextTemplate)
import Theme.Model
import User.Model
import UserGroup.Types
import Util.HasSomeUserInfo

----------------------------------------------------------------------------

mailNewUserGroupUserInvite
  :: ( TemplatesMonad m
     , MonadDB m
     , MonadThrow m
     , HasSomeUserInfo a
     , HasLang a
     , HasSomeUserInfo b
     )
  => Context
  -> a
  -> b
  -> UserGroup
  -> KontraLink
  -> UTCTime
  -> m Mail
mailNewUserGroupUserInvite ctx invited inviter ug link expires = do
  theme <- dbQuery . GetTheme $ fromMaybe (ctx ^. #brandedDomain % #mailTheme)
                                          (ug ^. #ui % #mailTheme)
  kontramaillocal (ctx ^. #mailNoreplyAddress)
                  (ctx ^. #brandedDomain)
                  theme
                  invited
                  "mailNewCompanyUserInvite"
    $ do
        basicUserGroupInviteFields invited inviter ug
        basicLinkFields (ctx ^. #brandedDomain % #url) link
        brandingMailFields theme
        F.value "creatorname" $ getSmartName inviter
        F.value "expiredate" $ formatTimeYMD expires


mailTakeoverSingleUserInvite
  :: ( TemplatesMonad m
     , MonadDB m
     , MonadThrow m
     , HasSomeUserInfo a
     , HasLang a
     , HasSomeUserInfo b
     )
  => Context
  -> a
  -> b
  -> UserGroup
  -> KontraLink
  -> m Mail
mailTakeoverSingleUserInvite ctx invited inviter ug link = do
  theme <- dbQuery . GetTheme $ fromMaybe (ctx ^. #brandedDomain % #mailTheme)
                                          (ug ^. #ui % #mailTheme)
  --invite in the language of the existing user rather than in the inviter's language
  kontramaillocal (ctx ^. #mailNoreplyAddress)
                  (ctx ^. #brandedDomain)
                  theme
                  invited
                  "mailTakeoverSingleUserInvite"
    $ do
        basicUserGroupInviteFields invited inviter ug
        basicLinkFields (ctx ^. #brandedDomain % #url) link
        brandingMailFields theme

basicUserGroupInviteFields
  :: (TemplatesMonad m, HasSomeUserInfo a, HasSomeUserInfo b)
  => a
  -> b
  -> UserGroup
  -> Fields m ()
basicUserGroupInviteFields invited inviter ug = do
  F.value "invitedname" $ getFullName invited
  F.value "invitedemail" $ getEmail invited
  F.value "invitername" $ getSmartName inviter
  F.value "companyname" $ ug ^. #name

basicLinkFields :: TemplatesMonad m => Text -> KontraLink -> Fields m ()
basicLinkFields hostpart link = do
  F.value "ctxhostpart" hostpart
  F.value "link" $ show link


-------------------------------------------------------------------------------

pageDoYouWantToBeCompanyAccount :: (TemplatesMonad m) => Context -> UserGroup -> m Text
pageDoYouWantToBeCompanyAccount ctx ug =
  renderTextTemplate "pageDoYouWantToBeCompanyAccount" $ do
    F.value "companyname" $ ug ^. #name
    entryPointFields ctx
-------------------------------------------------------------------------------

flashMessageUserHasBecomeCompanyAccount
  :: (TemplatesMonad m) => UserGroup -> m FlashMessage
flashMessageUserHasBecomeCompanyAccount ug = toFlashMsg OperationDone <$> renderTemplate
  "flashMessageUserHasBecomeCompanyAccount"
  (F.value "companyname" . T.unpack $ ug ^. #name)

-------------------------------------------------------------------------------

flashMessageBecomeCompanyLogInDifferentUser :: (TemplatesMonad m) => m FlashMessage
flashMessageBecomeCompanyLogInDifferentUser = toFlashMsg OperationFailed
  <$> renderTemplate_ "flashMessageBecomeCompanyLogInDifferentUser"

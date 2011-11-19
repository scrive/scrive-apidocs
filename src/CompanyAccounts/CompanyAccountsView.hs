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
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

import Company.Model
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
  renderTemplateFM "viewCompanyAccounts" $ do
    field "currentlink" $ show $ LinkCompanyAccounts $ emptyListParams

----------------------------------------------------------------------------

mailNewCompanyUserInvite :: TemplatesMonad m => String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> KontraLink-> m Mail
mailNewCompanyUserInvite hostpart invitername companyname emailaddress personname setpasslink = do
  kontramail "mailNewCompanyUserInvite" $ do
    field "personname"     $ BS.toString personname
    field "email"          $ BS.toString emailaddress
    field "passwordlink"   $ show setpasslink
    field "invitername"    $ BS.toString invitername
    field "companyname"    $ BS.toString companyname
    field "ctxhostpart"    $ hostpart

mailTakeoverPrivateUserInvite :: TemplatesMonad m => String -> User -> User -> Company -> m Mail
mailTakeoverPrivateUserInvite hostpart invited inviter company = do
  --invite in the language of the existing user rather than in the inviter's language
  kontramaillocal invited  "mailTakeoverPrivateUserInvite" $ do
                   field "invitedname" $ getFullName invited
                   field "invitername" $ getFullName inviter
                   field "companyname" $ getCompanyName company
                   field "linktojoin"  $ hostpart ++ (show $ LinkCompanyTakeover (companyid company))

mailTakeoverCompanyUserInfo :: TemplatesMonad m => User -> User -> Company -> m Mail
mailTakeoverCompanyUserInfo invited inviter company = do
  --send info in the language of the existing user rather than in the inviter's language
  kontramaillocal invited  "mailTakeoverCompanyUserInfo" $ do
                   field "invitedname" $ getFullName invited
                   field "invitername" $ getFullName inviter
                   field "companyname" $ getCompanyName company

-------------------------------------------------------------------------------

modalDoYouWantToBeCompanyAccount :: TemplatesMonad m => Company -> m FlashMessage
modalDoYouWantToBeCompanyAccount company =
  toModal <$> (renderTemplateFM "modalDoYouWantToBeCompanyAccount" $ do
            field "companyname" $ getCompanyName company)

-------------------------------------------------------------------------------

flashMessageCompanyAccountInviteSent :: TemplatesMonad m => m FlashMessage
flashMessageCompanyAccountInviteSent =
  toFlashMsg OperationDone <$> renderTemplateM "flashCompanyAccountInviteSent" ()

flashMessageCompanyAccountInviteResent :: TemplatesMonad m => m FlashMessage
flashMessageCompanyAccountInviteResent =
  toFlashMsg OperationDone <$> renderTemplateM "flashCompanyAccountInviteResent" ()

flashMessageUserHasBecomeCompanyAccount :: TemplatesMonad m => Company -> m FlashMessage
flashMessageUserHasBecomeCompanyAccount company =
  toFlashMsg OperationDone <$> (renderTemplateFM "flashMessageUserHasBecomeCompanyAccount" $ do
    field "companyname" $ getCompanyName company)

flashMessageUserHasLiveDocs :: TemplatesMonad m => m FlashMessage
flashMessageUserHasLiveDocs =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageUserHasLiveDocs" ()

flashMessageCompanyAccountDeleted :: TemplatesMonad m => m FlashMessage
flashMessageCompanyAccountDeleted =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageCompanyAccountDeleted" ()

-------------------------------------------------------------------------------

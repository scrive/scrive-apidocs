module User.UserView (
    -- pages
    viewCompanyAccounts,
    viewFriends,
    showUser,
    showUserSecurity,
    showUserMailAPI,
    pageAcceptTOS,
    activatePageViewNotValidLink,

    -- mails
    newUserMail,
    inviteCompanyAccountMail,
    viralInviteMail,
    mailNewAccountCreatedByAdmin,
    mailAccountCreatedBySigningContractReminder,
    mailAccountCreatedBySigningOfferReminder,
    mailAccountCreatedBySigningOrderReminder,
    resetPasswordMail,

    mailInviteUserAsCompanyAccount,
    mailCompanyAccountAccepted,

    -- modals
    modalWelcomeToSkrivaPa,
    modalAccountSetup,
    modalAccountRemoval,
    modalAccountRemoved,
    modalInviteUserAsCompanyAccount,
    modalDoYouWantToBeCompanyAccount,

    -- flash messages
    flashMessageLoginRedirectReason,
    flashMessageUserDetailsSaved,
    flashMessageCompanyCreated,
    flashMessageNoAccountType,
    flashMessageInvalidAccountType,
    flashMessageMustAcceptTOS,
    flashMessageBadOldPassword,
    flashMessagePasswordsDontMatch,
    flashMessageUserPasswordChanged,
    flashMessagePasswordChangeLinkNotValid,
    flashMessageUserWithSameEmailExists,
    flashMessageViralInviteSent,
    flashMessageOtherUserSentInvitation,
    flashMessageNoRemainedInvitationEmails,
    flashMessageActivationLinkNotValid,
    flashMessageUserActivated,
    flashMessageUserAlreadyActivated,
    flashMessageChangePasswordEmailSend,
    flashMessageNoRemainedPasswordReminderEmails,
    flashMessageNewActivationLinkSend,
    flashMessageUserSignupDone,
    flashMessageThanksForTheQuestion,
    flashMessageWeWillCallYouSoon,
    flashUserIsAlreadyCompanyAccount,
    flashMessageUserInvitedAsCompanyAccount,
    flashMessageUserHasBecomeCompanyAccount,
    flashMessageUserHasLiveDocs,
    flashMessageAccountsDeleted,

    --modals
    modalNewPasswordView,
    modalUserSignupDone,
    
    --utils
    userBasicFields,

    -- friends list
    friendSortSearchPage
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Maybe
import ActionSchedulerState
import Company.Model
import DB.Types
import Kontra
import KontraLink
import Mails.SendMail(Mail)
import Templates.Templates
import Templates.TemplatesUtils
import Text.StringTemplate.GenericStandard()
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import ListUtil
import FlashMessage
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import User.Model
import Data.List

showUser :: TemplatesMonad m => User -> Maybe Company -> Bool -> m String
showUser user mcompany createcompany = renderTemplateFM "showUser" $ do
    userFields user
    companyFields mcompany
    field "createcompany" $ createcompany
    field "linkaccount" $ show (LinkAccount False)

companyFields :: MonadIO m => Maybe Company -> Fields m
companyFields mcompany = do
    field "companyid" $ show $ fmap companyid mcompany
    field "address" $ fmap (companyaddress . companyinfo) mcompany
    field "city" $ fmap (companycity . companyinfo) mcompany
    field "country" $ fmap (companycountry . companyinfo) mcompany
    field "zip" $ fmap (companyzip . companyinfo) mcompany
    field "companyname" $ getCompanyName mcompany
    field "companynumber" $ getCompanyNumber mcompany
    field "companyimagelink" False

userFields :: MonadIO m => User -> Fields m
userFields user = do
    let fullname          = BS.toString $ getFullName user
        fullnameOrEmail   = BS.toString $ getSmartName user
        fullnamePlusEmail = if null fullname
                            then              "<" ++ (BS.toString $ getEmail user) ++ ">"
                            else fullname ++ " <" ++ (BS.toString $ getEmail user) ++ ">"
    field "id" $ show $ userid user
    field "fstname" $ getFirstName user
    field "sndname" $ getLastName user
    field "email" $ getEmail user
    field "personalnumber" $ getPersonalNumber user
    field "phone" $ userphone $ userinfo user
    field "mobile" $ usermobile $ userinfo user
    field "companyposition" $ usercompanyposition $ userinfo user
    field "userimagelink" False
    field "fullname" $ fullname
    field "fullnameOrEmail" $ fullnameOrEmail
    field "fullnamePlusEmail" $ fullnamePlusEmail
    field "iscompanyaccount" $ isJust $ usercompany user

    --field "invoiceaddress" $ BS.toString $ useraddress $ userinfo user
    menuFields user

showUserSecurity :: TemplatesMonad m => User -> m String
showUserSecurity user = renderTemplateFM "showUserSecurity" $ do
    field "linksecurity" $ show LinkAccountSecurity
    field "fstname" $ getFirstName user
    field "sndname" $ getLastName user
    field "userimagelink" False
    fieldF "region" $ do
        field "se" $ REGION_SE == (getRegion user)
        field "gb" $ REGION_GB == (getRegion user)
    fieldF "lang" $ do
        field "en" $ LANG_EN == (getLang user)
        field "se" $ LANG_SE == (getLang user)
    menuFields user

showUserMailAPI :: TemplatesMonad m => User -> Maybe UserMailAPI -> m String
showUserMailAPI user mapi =
    renderTemplateFM "showUserMailAPI" $ do
        field "linkmailapi" $ show LinkUserMailAPI
        field "mailapienabled" $ isJust mapi
        field "mailapikey" $ show . umapiKey <$> mapi
        field "mapidailylimit" $ show . umapiDailyLimit <$> mapi
        field "mapisenttoday" $ show . umapiSentToday <$> mapi
        menuFields user

pageAcceptTOS :: TemplatesMonad m => m String
pageAcceptTOS = renderTemplateM "pageAcceptTOS" ()

viewFriends :: TemplatesMonad m => PagedList User -> User -> m String
viewFriends friends user =
  renderTemplateFM "viewFriends" $ do
    fieldFL "friends" $ markParity $ map userFields $ list friends
    field "currentlink" $ show $ LinkSharing $ params friends
    menuFields user
    pagedListFields friends

menuFields :: MonadIO m => User -> Fields m
menuFields user = do
    field "iscompanyadmin" $ useriscompanyadmin user

viewCompanyAccounts :: TemplatesMonad m => User -> PagedList User -> m String
viewCompanyAccounts user companyusers =
  renderTemplateFM "viewCompanyAccounts" $ do
    fieldFL "companyaccounts" $ markParity $ map userFields $ list companyusers
    field "currentlink" $ show $ LinkCompanyAccounts $ params companyusers
    fieldF "user" $ userFields user
    pagedListFields companyusers

activatePageViewNotValidLink :: TemplatesMonad m => String -> m String
activatePageViewNotValidLink email =
  renderTemplateFM "activatePageViewNotValidLink" $ field "email" email

resetPasswordMail :: TemplatesMonad m => String -> User -> KontraLink -> m Mail
resetPasswordMail hostname user setpasslink = do
  kontramail "passwordChangeLinkMail" $ do
    field "personname"   $ getFullName user
    field "passwordlink" $ show setpasslink
    field "ctxhostpart"  $ hostname

newUserMail :: TemplatesMonad m => String -> BS.ByteString -> BS.ByteString -> KontraLink -> Bool -> m Mail
newUserMail hostpart emailaddress personname activatelink vip = do
  kontramail "newUserMail" $ do
    field "personname"   $ BS.toString personname
    field "email"        $ BS.toString emailaddress
    field "activatelink" $ show activatelink
    field "ctxhostpart"  $ hostpart
    field "vip"            vip



inviteCompanyAccountMail :: TemplatesMonad m => String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> KontraLink-> m Mail
inviteCompanyAccountMail hostpart supervisorname companyname emailaddress personname setpasslink = do
  kontramail "inviteCompanyAccountMail" $ do
    field "personname"     $ BS.toString personname
    field "email"          $ BS.toString emailaddress
    field "passwordlink"   $ show setpasslink
    field "supervisorname" $ BS.toString supervisorname
    field "companyname"    $ BS.toString companyname
    field "ctxhostpart"    $ hostpart

viralInviteMail :: TemplatesMonad m => Context -> BS.ByteString -> KontraLink -> m Mail
viralInviteMail ctx invitedemail setpasslink = do
  let invitername = BS.toString $ maybe BS.empty getSmartName (ctxmaybeuser ctx)
  kontramail "mailViralInvite" $ do
    field "email"        $ BS.toString invitedemail
    field "invitername"  $ invitername
    field "ctxhostpart"  $ ctxhostpart ctx
    field "passwordlink" $ show setpasslink


mailNewAccountCreatedByAdmin :: (HasLocale a, TemplatesMonad m) => Context -> a -> BS.ByteString -> BS.ByteString -> KontraLink -> Maybe String -> m Mail
mailNewAccountCreatedByAdmin ctx locale personname email setpasslink custommessage = do
  kontramaillocal locale "mailNewAccountCreatedByAdmin" $ do
    field "personname"    $ BS.toString personname
    field "email"         $ BS.toString email
    field "passwordlink"  $ show setpasslink
    field "creatorname"   $ BS.toString $ maybe BS.empty getSmartName (ctxmaybeuser ctx)
    field "ctxhostpart"   $ ctxhostpart ctx
    field "custommessage"   custommessage

mailAccountCreatedBySigningContractReminder :: TemplatesMonad m => String -> BS.ByteString -> BS.ByteString -> KontraLink -> m Mail
mailAccountCreatedBySigningContractReminder =
    mailAccountCreatedBySigning' "mailAccountBySigningContractReminder"

mailAccountCreatedBySigningOfferReminder :: TemplatesMonad m => String -> BS.ByteString -> BS.ByteString -> KontraLink -> m Mail
mailAccountCreatedBySigningOfferReminder =
    mailAccountCreatedBySigning' "mailAccountBySigningOfferReminder"


mailAccountCreatedBySigningOrderReminder :: TemplatesMonad m => String -> BS.ByteString -> BS.ByteString -> KontraLink -> m Mail
mailAccountCreatedBySigningOrderReminder =
    mailAccountCreatedBySigning' "mailAccountBySigningOrderReminder"

mailAccountCreatedBySigning' :: TemplatesMonad m => String -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> m Mail
mailAccountCreatedBySigning' mail_template hostpart doctitle personname activationlink = do
   kontramail mail_template $ do
        field "personname"     $ BS.toString personname
        field "ctxhostpart"    $ hostpart
        field "documenttitle"  $ BS.toString doctitle
        field "activationlink" $ show activationlink

mailInviteUserAsCompanyAccount :: TemplatesMonad m => Context -> User -> User -> m Mail
mailInviteUserAsCompanyAccount ctx invited supervisor = do
  kontramail  "mailInviteUserAsCompanyAccount" $ do
                   field "hostpart" (ctxhostpart ctx)
                   fieldF "supervisor" $ userFields supervisor
                   fieldF "invited" $ userFields invited

mailCompanyAccountAccepted :: TemplatesMonad m => Context -> User -> User -> m Mail
mailCompanyAccountAccepted ctx invited supervisor = do
  kontramail "mailCompanyAccountAccepted" $ do
                   field "hostpart" $ ctxhostpart ctx
                   fieldF "user" $ userFields supervisor
                   fieldF "invited" $ userFields invited

-------------------------------------------------------------------------------

modalInviteUserAsCompanyAccount :: TemplatesMonad m => String -> String -> String -> m FlashMessage
modalInviteUserAsCompanyAccount fstname sndname email =
    toModal <$> (renderTemplateFM "modalInviteUserAsCompanyAccount" $ do
      field "email" email
      field "fstname" fstname
      field "sndname" sndname)

modalWelcomeToSkrivaPa :: TemplatesMonad m => m FlashMessage
modalWelcomeToSkrivaPa =
    toModal <$> (renderTemplateFM "modalWelcomeToSkrivaPa" $ do
      field "phonelink" $ show LinkRequestPhoneCall)

modalAccountSetup :: MonadIO m => KontraLink -> String -> String -> m FlashMessage
modalAccountSetup signuplink fstname sndname = do
  return $ toFlashTemplate Modal "modalAccountSetup" $
    [ ("signuplink", show signuplink)
    , ("fstname", fstname)
    , ("sndname", sndname) ]

modalAccountRemoval :: TemplatesMonad m => BS.ByteString -> KontraLink -> KontraLink -> m FlashMessage
modalAccountRemoval doctitle activationlink removallink = do
    toModal <$> (renderTemplateFM "modalAccountRemoval" $ do
        field "documenttitle"  $ BS.toString doctitle
        field "activationlink" $ show activationlink
        field "removallink"    $ show removallink)

modalAccountRemoved :: TemplatesMonad m => BS.ByteString -> m FlashMessage
modalAccountRemoved doctitle = do
    toModal <$> (renderTemplateFM "modalAccountRemoved" $ do
        field "documenttitle"  $ BS.toString doctitle)

flashMessageThanksForTheQuestion :: TemplatesMonad m => m FlashMessage
flashMessageThanksForTheQuestion =
    toFlashMsg OperationDone <$> renderTemplateM "flashMessageThanksForTheQuestion" ()

flashMessageWeWillCallYouSoon :: TemplatesMonad m => String -> m FlashMessage
flashMessageWeWillCallYouSoon phone =
    toFlashMsg OperationDone <$> (renderTemplateFM "flashMessageWeWillCallYouSoon" $ do
        field "phone" phone)

flashMessageLoginRedirectReason :: TemplatesMonad m => LoginRedirectReason -> m (Maybe FlashMessage)
flashMessageLoginRedirectReason reason =
  case reason of
       LoginTry             -> return Nothing
       NotLogged            -> render "notlogged"
       NotLoggedAsSuperUser -> render "notsu"
       InvalidLoginInfo _   -> render "invloginfo"
  where
    render msg = Just . toFlashMsg OperationFailed <$>
      (renderTemplateFM "flashMessageLoginPageRedirectReason" $ field msg True)

flashMessageUserDetailsSaved :: TemplatesMonad m => m FlashMessage
flashMessageUserDetailsSaved =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageUserDetailsSaved" ()

flashMessageCompanyCreated :: TemplatesMonad m => m FlashMessage
flashMessageCompanyCreated =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageCompanyCreated" ()


flashMessageNoAccountType :: TemplatesMonad m => m FlashMessage
flashMessageNoAccountType =
    toFlashMsg OperationFailed <$> renderTemplateM "flashMessageNoAccountType" ()

flashMessageInvalidAccountType :: TemplatesMonad m => m FlashMessage
flashMessageInvalidAccountType =
    toFlashMsg OperationFailed <$> renderTemplateM "flashMessageInvalidAccountType" ()

flashMessageMustAcceptTOS :: TemplatesMonad m => m FlashMessage
flashMessageMustAcceptTOS =
  toFlashMsg SigningRelated <$> renderTemplateM "flashMessageMustAcceptTOS" ()


flashMessageBadOldPassword :: TemplatesMonad m => m FlashMessage
flashMessageBadOldPassword =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageBadOldPassword" ()


flashMessagePasswordsDontMatch :: TemplatesMonad m => m FlashMessage
flashMessagePasswordsDontMatch =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessagePasswordsDontMatch" ()


flashMessageUserPasswordChanged :: TemplatesMonad m => m FlashMessage
flashMessageUserPasswordChanged =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageUserPasswordChanged" ()

flashMessageUserHasBecomeCompanyAccount :: TemplatesMonad m => User -> m FlashMessage
flashMessageUserHasBecomeCompanyAccount supervisor =
  toFlashMsg OperationDone <$> (renderTemplateFM "flashMessageUserHasBecomeCompanyAccount" $ do
    fieldF "supervisor" $ userFields supervisor)

flashMessageUserHasLiveDocs :: TemplatesMonad m => m FlashMessage
flashMessageUserHasLiveDocs =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageUserHasLiveDocs" ()

flashMessageAccountsDeleted :: TemplatesMonad m => m FlashMessage
flashMessageAccountsDeleted =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageAccountsDeleted" ()

flashMessagePasswordChangeLinkNotValid :: TemplatesMonad m => m FlashMessage
flashMessagePasswordChangeLinkNotValid =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessagePasswordChangeLinkNotValid" ()


flashMessageUserWithSameEmailExists :: TemplatesMonad m => m FlashMessage
flashMessageUserWithSameEmailExists =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageUserWithSameEmailExists" ()


flashMessageViralInviteSent :: TemplatesMonad m => m FlashMessage
flashMessageViralInviteSent =
  toFlashMsg SigningRelated <$> renderTemplateM "flashMessageViralInviteSent" ()

flashMessageOtherUserSentInvitation :: TemplatesMonad m => m FlashMessage
flashMessageOtherUserSentInvitation =
    toFlashMsg OperationFailed <$> renderTemplateM "flashMessageOtherUserSentInvitation" ()

flashMessageNoRemainedInvitationEmails :: TemplatesMonad m => m FlashMessage
flashMessageNoRemainedInvitationEmails =
    toFlashMsg OperationFailed <$> renderTemplateM "flashMessageNoRemainedInvitationEmails" ()

flashMessageActivationLinkNotValid :: TemplatesMonad m => m FlashMessage
flashMessageActivationLinkNotValid =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageActivationLinkNotValid" ()


flashMessageUserActivated :: TemplatesMonad m => m FlashMessage
flashMessageUserActivated =
  toFlashMsg SigningRelated <$> renderTemplateM "flashMessageUserActivated" ()


flashMessageUserAlreadyActivated :: TemplatesMonad m => m FlashMessage
flashMessageUserAlreadyActivated =
  toFlashMsg OperationFailed <$> renderTemplateM "flashMessageUserAlreadyActivated" ()

flashMessageChangePasswordEmailSend :: TemplatesMonad m => m FlashMessage
flashMessageChangePasswordEmailSend =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageChangePasswordEmailSend" ()

flashMessageNoRemainedPasswordReminderEmails :: TemplatesMonad m => m FlashMessage
flashMessageNoRemainedPasswordReminderEmails =
    toFlashMsg OperationFailed <$> renderTemplateM "flashMessageNoRemainedPasswordReminderEmails" ()

flashMessageNewActivationLinkSend :: TemplatesMonad m => m FlashMessage
flashMessageNewActivationLinkSend =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageNewActivationLinkSend" ()


flashMessageUserSignupDone :: TemplatesMonad m => m FlashMessage
flashMessageUserSignupDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageUserSignupDone" ()

flashUserIsAlreadyCompanyAccount :: TemplatesMonad m => m FlashMessage
flashUserIsAlreadyCompanyAccount =
  toFlashMsg OperationFailed <$> renderTemplateM "flashUserIsAlreadyCompanyAccount" ()

flashMessageUserInvitedAsCompanyAccount :: TemplatesMonad m => m FlashMessage
flashMessageUserInvitedAsCompanyAccount =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageUserInvitedAsCompanyAccount" ()

modalNewPasswordView :: TemplatesMonad m => ActionID -> MagicHash -> m FlashMessage
modalNewPasswordView aid hash = do
  toModal <$> (renderTemplateFM "modalNewPasswordView" $ do
            field "linkchangepassword" $ show $ LinkPasswordReminder aid hash)

modalDoYouWantToBeCompanyAccount :: TemplatesMonad m => m FlashMessage
modalDoYouWantToBeCompanyAccount =
  toModal <$> renderTemplateM "modalDoYouWantToBeCompanyAccount" ()

modalUserSignupDone :: TemplatesMonad m => m FlashMessage
modalUserSignupDone =
  toModal <$> renderTemplateM "modalUserSignupDone" ()

-------------------------------------------------------------------------------

{- | Basic fields for the user  -}
userBasicFields :: MonadIO m => User -> Maybe Company -> Fields m
userBasicFields u mc = do
    field "id" $ show $ userid u
    field "fullname" $ getFullName u
    field "email" $ getEmail u
    field "company" $ getCompanyName mc
    field "phone" $ userphone $ userinfo u
    field "iscompanyadmin" $ useriscompanyadmin u
    field "TOSdate" $ maybe "-" show (userhasacceptedtermsofservice u)

-- list stuff for friends

-- Friends currently only use the email
friendSortSearchPage :: ListParams -> [User] -> PagedList User
friendSortSearchPage = listSortSearchPage friendSortFunc friendSearchFunc friendPageSize

friendPageSize :: Int
friendPageSize = 20

friendSortFunc :: SortingFunction User
friendSortFunc _ u1 u2 = compare (getEmail u1) (getEmail u2)

friendSearchFunc :: SearchingFunction User
friendSearchFunc s u = s `isInfixOf` (BS.toString $ getEmail u)



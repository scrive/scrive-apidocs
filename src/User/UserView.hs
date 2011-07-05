module User.UserView (
    -- pages
    viewSubaccounts,
    viewFriends,
    showUser,
    showUserSecurity,
    showUserMailAPI,
    pageAcceptTOS,
    activatePageViewNotValidLink,

    -- mails
    newUserMail,
    inviteSubaccountMail,
    viralInviteMail,
    mailNewAccountCreatedByAdmin,
    mailAccountCreatedBySigningContractReminder,
    mailAccountCreatedBySigningOfferReminder,
    resetPasswordMail,

    mailInviteUserAsSubaccount,
    mailSubaccountAccepted,

    -- modals
    modalWelcomeToSkrivaPa,
    modalAccountSetup,
    modalAccountRemoval,
    modalAccountRemoved,
    modalInviteUserAsSubaccount,
    modalDoYouWantToBeSubaccount,

    -- flash messages
    flashMessageLoginRedirectReason,
    flashMessageUserDetailsSaved,
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
    flashMessageUserInvitedAsSubaccount,
    flashMessageUserHasBecomeSubaccount,
    flashMessageUserHasLiveDocs,
    flashMessageAccountsDeleted,

    --modals
    modalNewPasswordView,

    --utils
    userBasicFields) where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Maybe
import ActionSchedulerState
import Happstack.State (query)
import Kontra
import KontraLink
import Mails.SendMail(Mail, emptyMail, title, content)
import Misc
import Templates.Templates
import Templates.TemplatesUtils
import Text.StringTemplate.GenericStandard()
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import ListUtil
import FlashMessage
import Util.HasSomeUserInfo

showUser :: KontrakcjaTemplates -> User -> IO String
showUser templates user = renderTemplate templates "showUser" $ do
    userFields user
    field "linkaccount" $ show LinkAccount

userFields :: User -> Fields
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
    field "address" $ useraddress $ userinfo user
    field "city" $ usercity $ userinfo user
    field "country" $ usercountry $ userinfo user
    field "zip" $ userzip $ userinfo user
    field "phone" $ userphone $ userinfo user
    field "mobile" $ usermobile $ userinfo user
    field "companyname" $ getCompanyName user
    field "companyposition" $ usercompanyposition $ userinfo user
    field "companynumber" $ getCompanyNumber user
    field "userimagelink" False
    field "companyimagelink" False
    field "fullname" $ fullname
    field "fullnameOrEmail" $ fullnameOrEmail
    field "fullnamePlusEmail" $ fullnamePlusEmail
    field "hassupervisor" $ isJust $ usersupervisor user

    --field "invoiceaddress" $ BS.toString $ useraddress $ userinfo user
    menuFields user

showUserSecurity :: KontrakcjaTemplates -> User -> IO String
showUserSecurity templates user = renderTemplate templates "showUserSecurity" $ do
    field "linksecurity" $ show LinkSecurity
    field "fstname" $ getFirstName user
    field "sndname" $ getLastName user
    field "userimagelink" False
    field "lang" $ do
        field "en" $ LANG_EN == (lang $ usersettings user)
        field "se" $ LANG_SE == (lang $ usersettings user)
    menuFields user

showUserMailAPI :: KontrakcjaTemplates -> User -> IO String
showUserMailAPI templates user@User{usermailapi} =
    renderTemplate templates "showUserMailAPI" $ do
        field "linkmailapi" $ show LinkUserMailAPI
        field "mailapienabled" $ maybe False (const True) usermailapi
        field "mailapikey" $ show . umapiKey <$> usermailapi
        field "mapidailylimit" $ umapiDailyLimit <$> usermailapi
        field "mapisenttoday" $ umapiSentToday <$> usermailapi
        menuFields user

pageAcceptTOS :: KontrakcjaTemplates -> IO String
pageAcceptTOS templates =
  renderTemplate templates "pageAcceptTOS" ()

viewFriends :: KontrakcjaTemplates -> PagedList User -> User -> IO String
viewFriends templates friends user =
  renderTemplate templates "viewFriends" $ do
    field "friends" $ markParity $ map userFields $ list friends
    field "currentlink" $ show $ LinkSharing $ params friends
    menuFields user
    pagedListFields friends

menuFields :: User -> Fields
menuFields user = do
    field "issubaccounts" $ isAbleToHaveSubaccounts user

viewSubaccounts :: (TemplatesMonad m) => User -> PagedList User -> m String
viewSubaccounts user subusers =
  renderTemplateM "viewSubaccounts" $ do
    field "subaccounts" $ markParity $ map userFields $ list subusers
    field "currentlink" $ show $ LinkSubaccount $ params subusers
    field "user" $ userFields user
    pagedListFields subusers

activatePageViewNotValidLink :: KontrakcjaTemplates -> String -> IO String
activatePageViewNotValidLink templates email =
  renderTemplate templates "activatePageViewNotValidLink" $ field "email" email


resetPasswordMail :: KontrakcjaTemplates -> String -> User -> KontraLink -> IO Mail
resetPasswordMail templates hostname user setpasslink = do
  title   <- renderTemplate templates "passwordChangeLinkMailTitle" ()
  content <- (renderTemplate templates "passwordChangeLinkMailContent" $ do
    field "personname"   $ getFullName user
    field "passwordlink" $ show setpasslink
    field "ctxhostpart"  $ hostname
    ) >>= wrapHTML templates
  return $ emptyMail { title = BS.fromString title, content = BS.fromString content }


newUserMail :: KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> Bool -> IO Mail
newUserMail templates hostpart emailaddress personname activatelink vip = do
  title   <- renderTemplate templates "newUserMailTitle" ()
  content <- (renderTemplate templates "newUserMailContent" $ do
    field "personname"   $ BS.toString personname
    field "email"        $ BS.toString emailaddress
    field "activatelink" $ show activatelink
    field "ctxhostpart"  $ hostpart
    field "vip"            vip
    ) >>= wrapHTML templates
  return $ emptyMail { title = BS.fromString title, content = BS.fromString content }


inviteSubaccountMail :: KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> KontraLink-> IO Mail
inviteSubaccountMail  templates hostpart supervisorname companyname emailaddress personname setpasslink = do
  title   <- renderTemplate templates "inviteSubaccountMailTitle" ()
  content <- (renderTemplate templates "inviteSubaccountMailContent" $ do
    field "personname"     $ BS.toString personname
    field "email"          $ BS.toString emailaddress
    field "passwordlink"   $ show setpasslink
    field "supervisorname" $ BS.toString supervisorname
    field "companyname"    $ BS.toString companyname
    field "ctxhostpart"    $ hostpart
    ) >>= wrapHTML templates
  return $ emptyMail { title = BS.fromString title, content = BS.fromString content }


viralInviteMail :: KontrakcjaTemplates -> Context -> BS.ByteString -> KontraLink -> IO Mail
viralInviteMail templates ctx invitedemail setpasslink = do
  let invitername = BS.toString $ maybe BS.empty getSmartName (ctxmaybeuser ctx)
  title   <- renderTemplate templates "mailViralInviteTitle" $ field "invitername" invitername
  content <- (renderTemplate templates "mailViralInviteContent" $ do
    field "email"        $ BS.toString invitedemail
    field "invitername"  $ invitername
    field "ctxhostpart"  $ ctxhostpart ctx
    field "passwordlink" $ show setpasslink
    ) >>= wrapHTML templates
  return $ emptyMail { title = BS.fromString title, content = BS.fromString content }


mailNewAccountCreatedByAdmin :: KontrakcjaTemplates -> Context-> BS.ByteString -> BS.ByteString -> KontraLink -> Maybe String -> IO Mail
mailNewAccountCreatedByAdmin templates ctx personname email setpasslink custommessage = do
  title   <- renderTemplate templates "mailNewAccountCreatedByAdminTitle" ()
  content <- (renderTemplate templates "mailNewAccountCreatedByAdminContent" $ do
    field "personname"    $ BS.toString personname
    field "email"         $ BS.toString email
    field "passwordlink"  $ show setpasslink
    field "creatorname"   $ BS.toString $ maybe BS.empty getSmartName (ctxmaybeuser ctx)
    field "ctxhostpart"   $ ctxhostpart ctx
    field "custommessage"   custommessage
    ) >>= wrapHTML templates
  return $ emptyMail { title = BS.fromString title, content = BS.fromString content }

mailAccountCreatedBySigningContractReminder :: KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> IO Mail
mailAccountCreatedBySigningContractReminder =
    mailAccountCreatedBySigning' "mailAccountBySigningContractReminderTitle"
                                 "mailAccountBySigningContractReminderContent"

mailAccountCreatedBySigningOfferReminder :: KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> IO Mail
mailAccountCreatedBySigningOfferReminder =
    mailAccountCreatedBySigning' "mailAccountBySigningOfferReminderTitle"
                                 "mailAccountBySigningOfferReminderContent"

mailAccountCreatedBySigning' :: String -> String -> KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> IO Mail
mailAccountCreatedBySigning' title_template content_template templates hostpart doctitle personname activationlink = do
    title <- renderTemplate templates title_template ()
    content <- (renderTemplate templates content_template $ do
        field "personname"     $ BS.toString personname
        field "ctxhostpart"    $ hostpart
        field "documenttitle"  $ BS.toString doctitle
        field "activationlink" $ show activationlink
        ) >>= wrapHTML templates
    return $ emptyMail { title = BS.fromString title, content = BS.fromString content }

mailInviteUserAsSubaccount :: (TemplatesMonad m) => Context -> User -> User -> m Mail
mailInviteUserAsSubaccount ctx invited supervisor = do
    templates <- getTemplates
    title <- renderTemplateM "mailInviteUserAsSubaccountTitle" ()
    content <- (liftIO $ renderTemplate templates "mailInviteUserAsSubaccountContent" $ do
                   field "hostpart" (ctxhostpart ctx)
                   field "supervisor" $ userFields supervisor
                   field "invited" $ userFields invited
        ) >>= (liftIO . wrapHTML templates)
    return $ emptyMail { title = BS.fromString title, content = BS.fromString content }

mailSubaccountAccepted :: (TemplatesMonad m) => Context -> User -> User -> m Mail
mailSubaccountAccepted ctx invited supervisor = do
    templates <- getTemplates
    title <- renderTemplateM "mailSubaccountAcceptedTitle" ()
    content <- (liftIO $ renderTemplate templates "mailSubaccountAcceptedContent" $ do
                   field "hostpart" (ctxhostpart ctx)
                   field "user" $ userFields supervisor
                   field "invited" $ userFields invited
        ) >>= (liftIO . wrapHTML templates)
    return $ emptyMail { title = BS.fromString title, content = BS.fromString content }

-------------------------------------------------------------------------------

modalInviteUserAsSubaccount :: TemplatesMonad m => String -> String -> String -> m FlashMessage
modalInviteUserAsSubaccount fstname sndname email =
    toModal <$> (renderTemplateM "modalInviteUserAsSubaccount" $ do
      field "email" email
      field "fstname" fstname
      field "sndname" sndname)

modalWelcomeToSkrivaPa :: TemplatesMonad m => m FlashMessage
modalWelcomeToSkrivaPa =
    toModal <$> renderTemplateM "modalWelcomeToSkrivaPa" ()

modalAccountSetup :: Maybe User -> KontraLink -> IO FlashMessage
modalAccountSetup muser signuplink = do
    msupervisor <- case msupervisorid of
        Just sid -> query $ GetUserByUserID $ UserID $ unSupervisorID sid
        Nothing  -> return Nothing
    return $ toFlashTemplate Modal "modalAccountSetup" $
        supervisorfields msupervisor ++ [
              ("fstname", showUserField userfstname)
            , ("sndname", showUserField usersndname)
            , ("companyname", showUserField usercompanyname)
            , ("companyposition", showUserField usercompanyposition)
            , ("phone", showUserField userphone)
            , ("signuplink", show signuplink)
            ]
    where
        showUserField f = maybe "" (BS.toString . f . userinfo) muser
        msupervisorid = join (usersupervisor <$> muser)
        supervisorfields Nothing = []
        supervisorfields (Just svis) = [
              ("hassupervisor", "true")
            , ("supervisorcompany", BS.toString $ getCompanyName svis)
            , ("supervisoraccounttype", supervisoraccounttype)
            , (supervisoraccounttype, "true")
            ]
            where
                supervisoraccounttype = show $ accounttype $ usersettings svis

modalAccountRemoval :: TemplatesMonad m => BS.ByteString -> KontraLink -> KontraLink -> m FlashMessage
modalAccountRemoval doctitle activationlink removallink = do
    toModal <$> (renderTemplateM "modalAccountRemoval" $ do
        field "documenttitle"  $ BS.toString doctitle
        field "activationlink" $ show activationlink
        field "removallink"    $ show removallink)

modalAccountRemoved :: TemplatesMonad m => BS.ByteString -> m FlashMessage
modalAccountRemoved doctitle = do
    toModal <$> (renderTemplateM "modalAccountRemoved" $ do
        field "documenttitle"  $ BS.toString doctitle)

flashMessageThanksForTheQuestion :: KontrakcjaTemplates -> IO FlashMessage
flashMessageThanksForTheQuestion templates =
    toFlashMsg OperationDone <$> renderTemplate templates "flashMessageThanksForTheQuestion" ()

flashMessageLoginRedirectReason :: KontrakcjaTemplates -> LoginRedirectReason -> IO (Maybe FlashMessage)
flashMessageLoginRedirectReason templates reason =
  case reason of
       LoginTry             -> return Nothing
       NotLogged            -> render "notlogged"
       NotLoggedAsSuperUser -> render "notsu"
       InvalidLoginInfo _   -> render "invloginfo"
  where
    render msg = Just . toFlashMsg OperationFailed <$>
      (renderTemplate templates "flashMessageLoginPageRedirectReason" $ field msg True)


flashMessageUserDetailsSaved :: KontrakcjaTemplates -> IO FlashMessage
flashMessageUserDetailsSaved templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageUserDetailsSaved" ()


flashMessageNoAccountType :: KontrakcjaTemplates -> IO FlashMessage
flashMessageNoAccountType templates =
    toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageNoAccountType" ()

flashMessageInvalidAccountType :: KontrakcjaTemplates -> IO FlashMessage
flashMessageInvalidAccountType templates =
    toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageInvalidAccountType" ()

flashMessageMustAcceptTOS :: KontrakcjaTemplates -> IO FlashMessage
flashMessageMustAcceptTOS templates =
  toFlashMsg SigningRelated <$> renderTemplate templates "flashMessageMustAcceptTOS" ()


flashMessageBadOldPassword :: KontrakcjaTemplates -> IO FlashMessage
flashMessageBadOldPassword templates =
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageBadOldPassword" ()


flashMessagePasswordsDontMatch :: KontrakcjaTemplates -> IO FlashMessage
flashMessagePasswordsDontMatch templates =
  toFlashMsg OperationFailed <$> renderTemplate templates"flashMessagePasswordsDontMatch" ()


flashMessageUserPasswordChanged :: KontrakcjaTemplates -> IO FlashMessage
flashMessageUserPasswordChanged templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageUserPasswordChanged" ()

flashMessageUserHasBecomeSubaccount :: KontrakcjaTemplates -> User -> IO FlashMessage
flashMessageUserHasBecomeSubaccount templates supervisor =
  toFlashMsg OperationDone <$> (renderTemplate templates "flashMessageUserHasBecomeSubaccount" $ do
    field "supervisor" $ userFields supervisor)

flashMessageUserHasLiveDocs :: KontrakcjaTemplates -> IO FlashMessage
flashMessageUserHasLiveDocs templates =
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageUserHasLiveDocs" ()

flashMessageAccountsDeleted :: KontrakcjaTemplates -> IO FlashMessage
flashMessageAccountsDeleted templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageAccountsDeleted" ()

flashMessagePasswordChangeLinkNotValid :: KontrakcjaTemplates -> IO FlashMessage
flashMessagePasswordChangeLinkNotValid templates =
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessagePasswordChangeLinkNotValid" ()


flashMessageUserWithSameEmailExists :: KontrakcjaTemplates -> IO FlashMessage
flashMessageUserWithSameEmailExists templates =
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageUserWithSameEmailExists" ()


flashMessageViralInviteSent :: KontrakcjaTemplates -> IO FlashMessage
flashMessageViralInviteSent templates =
  toFlashMsg SigningRelated <$> renderTemplate templates "flashMessageViralInviteSent" ()

flashMessageOtherUserSentInvitation :: KontrakcjaTemplates -> IO FlashMessage
flashMessageOtherUserSentInvitation templates =
    toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageOtherUserSentInvitation" ()

flashMessageNoRemainedInvitationEmails :: KontrakcjaTemplates -> IO FlashMessage
flashMessageNoRemainedInvitationEmails templates =
    toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageNoRemainedInvitationEmails" ()

flashMessageActivationLinkNotValid :: KontrakcjaTemplates -> IO FlashMessage
flashMessageActivationLinkNotValid templates =
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageActivationLinkNotValid" ()


flashMessageUserActivated :: KontrakcjaTemplates -> IO FlashMessage
flashMessageUserActivated templates =
  toFlashMsg SigningRelated <$> renderTemplate templates "flashMessageUserActivated" ()


flashMessageUserAlreadyActivated :: KontrakcjaTemplates -> IO FlashMessage
flashMessageUserAlreadyActivated templates =
  toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageUserAlreadyActivated" ()

flashMessageChangePasswordEmailSend :: KontrakcjaTemplates -> IO FlashMessage
flashMessageChangePasswordEmailSend templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageChangePasswordEmailSend" ()

flashMessageNoRemainedPasswordReminderEmails :: KontrakcjaTemplates -> IO FlashMessage
flashMessageNoRemainedPasswordReminderEmails templates =
    toFlashMsg OperationFailed <$> renderTemplate templates "flashMessageNoRemainedPasswordReminderEmails" ()

flashMessageNewActivationLinkSend :: KontrakcjaTemplates -> IO FlashMessage
flashMessageNewActivationLinkSend templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageNewActivationLinkSend" ()


flashMessageUserSignupDone :: KontrakcjaTemplates -> IO FlashMessage
flashMessageUserSignupDone templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageUserSignupDone" ()

flashMessageUserInvitedAsSubaccount :: KontrakcjaTemplates -> IO FlashMessage
flashMessageUserInvitedAsSubaccount templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageUserInvitedAsSubaccount" ()

modalNewPasswordView :: TemplatesMonad m => ActionID -> MagicHash -> m FlashMessage
modalNewPasswordView aid hash = do
  toModal <$> (renderTemplateM "modalNewPasswordView" $ do
            field "linkchangepassword" $ show $ LinkPasswordReminder aid hash)

modalDoYouWantToBeSubaccount :: TemplatesMonad m => m FlashMessage
modalDoYouWantToBeSubaccount =
  toModal <$> renderTemplateM "modalDoYouWantToBeSubaccount" ()


-------------------------------------------------------------------------------

{- | Basic fields for the user  -}
userBasicFields :: User -> Fields
userBasicFields u = do
    field "id" $ show $ userid u
    field "fullname" $ getFullName u
    field "email" $ getEmail u
    field "company" $ getCompanyName u
    field "phone" $ userphone $ userinfo u
    field "TOSdate" $ maybe "-" show (userhasacceptedtermsofservice u)

{-# OPTIONS_GHC -Wall #-}
module User.UserView (
    -- pages
    viewSubaccounts,
    viewFriends,
    showUser,
    showUserSecurity,
    pageAcceptTOS,
    activatePageView,
    activatePageViewNotValidLink,
    
    -- mails  
    newUserMail,
    inviteSubaccountMail,
    viralInviteMail,
    mailNewAccountCreatedByAdmin,
    mailAccountCreatedBySigning,
    mailAccountCreatedBySigningReminder,
    mailAccountCreatedBySigningLastReminder,
    resetPasswordMail,

    -- modals
    modalWelcomeToSkrivaPa,
    modalAccountRemoval,
    modalAccountRemoved,

    -- flash messages
    flashMessageLoginRedirectReason,
    flashMessageUserDetailsSaved,
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
    flashMessageAccountRequestSend,
    flashMessageThanksForTheQuestion,
    
    --modals
    modalNewPasswordView,

    --utils  
    prettyName,
    userSmallView,
    UserSmallView(..)) where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Control.Monad.Trans (lift)
import Data.Data
import Data.Maybe
import ActionSchedulerState
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

showUser :: KontrakcjaTemplates -> User -> IO String 
showUser templates user = renderTemplate templates "showUser" $ do
    userFields user
    field "linkaccount" $ show LinkAccount

userFields::User -> Fields
userFields user = do
    field "id" $ show $ userid user 
    field "fstname" $ BS.toString $ userfstname $ userinfo user 
    field "sndname" $ BS.toString $ usersndname $ userinfo user 
    field "email" $ BS.toString $ unEmail $ useremail $ userinfo user
    field "personalnumber" $ BS.toString $ userpersonalnumber $ userinfo user
    field "address" $ BS.toString $ useraddress $ userinfo user
    field "city" $ BS.toString $ usercity $ userinfo user
    field "country" $ BS.toString $ usercountry $ userinfo user
    field "zip" $ BS.toString $ userzip $ userinfo user
    field "phone" $ BS.toString $ userphone $ userinfo user
    field "mobile" $ BS.toString $ usermobile $ userinfo user
    field "companyname" $ BS.toString $ usercompanyname $ userinfo user
    field "companyposition" $ BS.toString $ usercompanyposition $ userinfo user
    field "companynumber" $ BS.toString $ usercompanynumber $ userinfo user
    field "userimagelink" False
    field "companyimagelink" False
    --field "invoiceaddress" $ BS.toString $ useraddress $ userinfo user

showUserSecurity :: KontrakcjaTemplates -> User -> IO String
showUserSecurity templates user = renderTemplate templates "showUserSecurity" $ do
    field "linksecurity" $ show LinkSecurity 
    field "fstname" $ BS.toString $ userfstname $ userinfo user 
    field "sndname" $ BS.toString $ usersndname $ userinfo user
    field "userimagelink" False
    
pageAcceptTOS :: KontrakcjaTemplates ->  BS.ByteString -> IO String
pageAcceptTOS templates tostext = 
  renderTemplate templates "pageAcceptTOS" $ field "tostext" (BS.toString tostext)

viewFriends :: KontrakcjaTemplates -> PagedList User -> IO String
viewFriends templates friends =  
  renderTemplate templates "viewFriends" $ do
    field "friends" $ markParity $ map userFields $ list friends
    field "currentlink" $ show $ LinkSharing $ params friends
    pagedListFields friends


viewSubaccounts :: KontrakcjaTemplates -> PagedList User -> IO String
viewSubaccounts templates subusers =  
  renderTemplate templates "viewSubaccounts" $ do
    field "subaccounts" $ markParity $ map userFields $ list subusers
    field "currentlink" $ show $ LinkSubaccount $ params subusers
    pagedListFields subusers



activatePageView::KontrakcjaTemplates -> String -> Maybe User -> IO String
activatePageView templates tostext muser = 
    renderTemplate templates "activatePageView" $ do
        field "tostext" tostext
        when (isJust muser) $ userFields $ fromJust muser


activatePageViewNotValidLink :: KontrakcjaTemplates -> String -> IO String
activatePageViewNotValidLink templates email =
  renderTemplate templates "activatePageViewNotValidLink" $ field "email" email


resetPasswordMail :: KontrakcjaTemplates -> String -> User -> KontraLink -> IO Mail
resetPasswordMail templates hostname user setpasslink = do
  title   <- renderTemplate templates "passwordChangeLinkMailTitle" ()
  content <- (renderTemplate templates "passwordChangeLinkMailContent" $ do
    field "personname"   $ userfullname user
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
  let invitername = BS.toString $ maybe BS.empty prettyName (ctxmaybeuser ctx)
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
    field "creatorname"   $ BS.toString $ maybe BS.empty prettyName (ctxmaybeuser ctx)
    field "ctxhostpart"   $ ctxhostpart ctx
    field "custommessage"   custommessage
    ) >>= wrapHTML templates
  return $ emptyMail { title = BS.fromString title, content = BS.fromString content }

mailAccountCreatedBySigning :: KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> KontraLink -> IO Mail
mailAccountCreatedBySigning =
    mailAccountCreatedBySigning' "mailAccountBySigningTitle"
                                 "mailAccountBySigningContent"

mailAccountCreatedBySigningReminder :: KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> KontraLink -> IO Mail
mailAccountCreatedBySigningReminder =
    mailAccountCreatedBySigning' "mailAccountBySigningReminderTitle"
                                 "mailAccountBySigningReminderContent"

mailAccountCreatedBySigningLastReminder :: KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> KontraLink -> IO Mail
mailAccountCreatedBySigningLastReminder =
    mailAccountCreatedBySigning' "mailAccountBySigningLastReminderTitle"
                                 "mailAccountBySigningLastReminderContent"

mailAccountCreatedBySigning' :: String -> String -> KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> KontraLink -> IO Mail
mailAccountCreatedBySigning' title_template content_template templates hostpart doctitle personname activationlink removallink = do
    title <- renderTemplate templates title_template ()
    content <- (renderTemplate templates content_template $ do
        field "personname"     $ BS.toString personname
        field "ctxhostpart"    $ hostpart
        field "documenttitle"  $ BS.toString doctitle
        field "activationlink" $ show activationlink
        field "removallink"    $ show removallink
        ) >>= wrapHTML templates
    return $ emptyMail { title = BS.fromString title, content = BS.fromString content }

-------------------------------------------------------------------------------

modalWelcomeToSkrivaPa :: KontrakcjaTemplates -> KontraModal
modalWelcomeToSkrivaPa templates =
    lift $ renderTemplate templates "modalWelcomeToSkrivaPa" ()

modalAccountRemoval :: KontrakcjaTemplates -> BS.ByteString -> KontraLink -> KontraLink -> KontraModal
modalAccountRemoval templates doctitle activationlink removallink = do
    lift $ renderTemplate templates "modalAccountRemoval" $ do
        field "documenttitle"  $ BS.toString doctitle
        field "activationlink" $ show activationlink
        field "removallink"    $ show removallink

modalAccountRemoved :: KontrakcjaTemplates -> BS.ByteString -> KontraModal
modalAccountRemoved templates doctitle = do
    lift $ renderTemplate templates "modalAccountRemoved" $ do
        field "documenttitle"  $ BS.toString doctitle

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


flashMessageAccountRequestSend :: KontrakcjaTemplates -> IO FlashMessage
flashMessageAccountRequestSend templates =
  toFlashMsg OperationDone <$> renderTemplate templates "flashMessageAccountRequestSend" ()

modalNewPasswordView :: ActionID -> MagicHash -> KontraModal
modalNewPasswordView aid hash = do
  templates <- ask
  lift $ renderTemplate templates "modalNewPasswordView" $ do
            field "linkchangepassword" $ (show $ LinkPasswordReminder aid hash)

-------------------------------------------------------------------------------

{- Same as personname (username or email) from DocView but works on User -}
prettyName :: User -> BS.ByteString
prettyName u =
  if BS.null $ userfullname u
     then unEmail . useremail $ userinfo u
     else userfullname u

{- View Utills  -}

{-| Users simple view (for templates) -}
data UserSmallView =
  UserSmallView {
      usvId       :: String
    , usvFullname :: String
    , usvEmail    :: String
    , usvCompany  :: String
    } deriving (Data, Typeable)

{-| Conversion from 'User' to 'Option', for select box UserSmallView  -}      
userSmallView :: User -> UserSmallView 
userSmallView u =
  UserSmallView {
      usvId       = show $ userid u
    , usvFullname = BS.toString $ userfullname u
    , usvEmail    = BS.toString . unEmail . useremail $ userinfo u
    , usvCompany  = BS.toString . usercompanyname $ userinfo u
    }

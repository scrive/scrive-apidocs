{-# OPTIONS_GHC -Wall #-}
module User.UserView (
    -- pages
    viewSubaccounts,
    showUser,
    pageAcceptTOS,
    newPasswordPageView,
    activatePageView,
    activatePageViewNotValidLink,
    
    -- mails  
    passwordChangeMail,
    newUserMail,
    inviteSubaccountMail,
    viralInviteMail,
    mailNewAccountCreatedByAdmin,
    resetPasswordMail,

    -- flash messages
    flashMessageLoginRedirectReason,
    flashMessageUserDetailsSaved,
    flashMessageMustAcceptTOS,
    flashMessagePasswordNotStrong,
    flashMessageBadOldPassword,
    flashMessagePasswordsDontMatch,
    flashMessageUserPasswordChanged,
    flashMessagePasswordChangeLinkNotValid,
    flashMessageUserWithSameEmailExists,
    flashMessageViralInviteSent,
    flashMessageActivationLinkNotValid,
    flashMessageUserActivated,
    flashMessageUserAlreadyActivated,
    flashMessageNoSuchUserExists,
    flashMessageChangePasswordEmailSend,  
    flashMessageNewActivationLinkSend,
    flashMessageUserSignupDone,
    flashMessageAccountRequestSend,
    
    --utils  
    prettyName,
    userSmallView,
    UserSmallView(..)) where

import Control.Applicative ((<$>))
import Data.Data
import Kontra
import KontraLink
import Mails.SendMail(Mail, emptyMail, title, content)
import Templates.Templates 
import Templates.TemplatesUtils
import Text.StringTemplate.GenericStandard()
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS


showUser :: KontrakcjaTemplates -> User -> [User] -> IO String 
showUser templates user viewers = renderTemplate templates "showUser" $
                                                        (setAttribute "fname" $ BS.toString $ userfstname $ userinfo user ) .
                                                        (setAttribute "lname" $ BS.toString $ usersndname $ userinfo user ) .
                                                        (setAttribute "email" $ BS.toString $ unEmail $ useremail $ userinfo user) .
                                                        (setAttribute "companyname" $ BS.toString $ usercompanyname $ userinfo user) .
                                                        (setAttribute "companyposition" $ BS.toString $ usercompanyposition $ userinfo user) .
                                                        (setAttribute "companynumber" $ BS.toString $ usercompanynumber $ userinfo user) .
                                                        (setAttribute "invoiceaddress" $ BS.toString $ useraddress $ userinfo user) .
                                                        (setAttribute "viewers" $ map (BS.toString . prettyName)  viewers) .
                                                        (setAttribute "linkaccount" $ show LinkAccount) .
                                                        (setAttribute "linkaccountpassword" $ show LinkAccountPassword) .
                                                        (setAttribute "linksubaccount" $ show LinkSubaccount) 
    
pageAcceptTOS :: KontrakcjaTemplates ->  BS.ByteString -> IO String
pageAcceptTOS templates tostext = 
  renderTemplate templates "pageAcceptTOS" $ field "tostext" (BS.toString tostext)


viewSubaccounts :: KontrakcjaTemplates -> [User] -> IO String
viewSubaccounts templates subusers =
  renderTemplate templates "viewSubaccounts" $
    (setAttribute "subusers" $ map userSmallView $ subusers) .
    (setAttribute "subaccountslink" $ show LinkSubaccount)



activatePageView::KontrakcjaTemplates -> String -> String ->  IO String
activatePageView templates tostext name = 
    renderTemplate templates "activatePageView" $ do
        field "tostext" tostext
        field "fname" fname
        field "lname" $ drop 1 sname
  where (fname,sname) = span (/= ' ') name    

activatePageViewNotValidLink :: KontrakcjaTemplates -> String -> IO String
activatePageViewNotValidLink templates email =
  renderTemplate templates "activatePageViewNotValidLink" $ field "email" email

newPasswordPageView :: KontrakcjaTemplates -> IO String
newPasswordPageView templates =
  renderTemplate templates "newPasswordPage" ()


resetPasswordMail :: KontrakcjaTemplates -> String -> User -> KontraLink -> IO Mail
resetPasswordMail templates hostname user setpasslink = do
  title   <- renderTemplate templates "passwordChangeLinkMailTitle" ()
  content <- (renderTemplate templates "passwordChangeLinkMailContent" $ do
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


passwordChangeMail :: KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> IO Mail
passwordChangeMail templates hostpart emailaddress personname setpasslink = do
  title   <- renderTemplate templates "passwordChangeMailTitle" ()
  content <- (renderTemplate templates "passwordChangeMailContent" $ do
    field "personname"   $ BS.toString personname
    field "email"        $ BS.toString emailaddress
    field "passwordlink" $ show setpasslink
    field "ctxhostpart"  $ hostpart
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

-------------------------------------------------------------------------------

flashMessageLoginRedirectReason :: KontrakcjaTemplates -> LoginRedirectReason -> IO (Maybe String)
flashMessageLoginRedirectReason templates reason =
  case reason of
       NoReason             -> return Nothing
       NotLogged            -> render "notlogged"
       NotLoggedAsSuperUser -> render "notsu"
       InvalidEmail         -> render "invemail"
       InvalidPassword _    -> render "invpass"
  where
    render msg = Just <$> (renderTemplate templates "loginPageRedirectReason" $ field msg True)


flashMessageUserDetailsSaved :: KontrakcjaTemplates -> IO String
flashMessageUserDetailsSaved templates =
  renderTemplate templates "flashMessageUserDetailsSaved" () 


flashMessageMustAcceptTOS :: KontrakcjaTemplates -> IO String
flashMessageMustAcceptTOS templates =
  renderTemplate templates "flashMessageMustAcceptTOS" ()


flashMessagePasswordNotStrong :: KontrakcjaTemplates -> IO String
flashMessagePasswordNotStrong templates =
  renderTemplate templates "flashMessagePasswordNotStrong" ()


flashMessageBadOldPassword :: KontrakcjaTemplates -> IO String
flashMessageBadOldPassword templates =
  renderTemplate templates "flashMessageBadOldPassword" ()


flashMessagePasswordsDontMatch :: KontrakcjaTemplates -> IO String
flashMessagePasswordsDontMatch templates = renderTemplate templates"flashMessagePasswordsDontMatch" ()


flashMessageUserPasswordChanged :: KontrakcjaTemplates -> IO String
flashMessageUserPasswordChanged templates =
  renderTemplate templates "flashMessageUserPasswordChanged" ()


flashMessagePasswordChangeLinkNotValid :: KontrakcjaTemplates -> IO String
flashMessagePasswordChangeLinkNotValid templates =
  renderTemplate templates "flashMessagePasswordChangeLinkNotValid" ()


flashMessageUserWithSameEmailExists :: KontrakcjaTemplates -> IO String
flashMessageUserWithSameEmailExists templates =
  renderTemplate templates "flashMessageUserWithSameEmailExists" ()


flashMessageViralInviteSent :: KontrakcjaTemplates -> IO String
flashMessageViralInviteSent templates =
  renderTemplate templates "flashMessageViralInviteSent" ()


flashMessageActivationLinkNotValid :: KontrakcjaTemplates -> IO String
flashMessageActivationLinkNotValid templates =
  renderTemplate templates "flashMessageActivationLinkNotValid" ()


flashMessageUserActivated :: KontrakcjaTemplates -> IO String
flashMessageUserActivated templates =
  renderTemplate templates "flashMessageUserActivated" ()


flashMessageUserAlreadyActivated :: KontrakcjaTemplates -> IO String
flashMessageUserAlreadyActivated templates =
  renderTemplate templates "flashMessageUserAlreadyActivated" ()


flashMessageNoSuchUserExists :: KontrakcjaTemplates -> IO String
flashMessageNoSuchUserExists templates =
  renderTemplate templates "flashMessageNoSuchUserExists" ()


flashMessageChangePasswordEmailSend :: KontrakcjaTemplates -> IO String
flashMessageChangePasswordEmailSend templates =
  renderTemplate templates "flashMessageChangePasswordEmailSend" ()


flashMessageNewActivationLinkSend :: KontrakcjaTemplates -> IO String
flashMessageNewActivationLinkSend templates =
  renderTemplate templates "flashMessageNewActivationLinkSend" ()


flashMessageUserSignupDone :: KontrakcjaTemplates -> IO String
flashMessageUserSignupDone templates =
  renderTemplate templates "flashMessageUserSignupDone" ()


flashMessageAccountRequestSend :: KontrakcjaTemplates -> IO String
flashMessageAccountRequestSend templates =
  renderTemplate templates "flashMessageAccountRequestSend" ()

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

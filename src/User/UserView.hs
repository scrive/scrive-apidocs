{-# OPTIONS_GHC -F -pgmFtrhsx -Wall #-}
module User.UserView(
   --pages
    viewSubaccounts,
    showUser,
    pageAcceptTOS,
    newPasswordPageView,
    activatePageView,
    activatePageViewNotValidLink,
    --mails  
    passwordChangeMail,
    newUserMail,
    inviteSubaccountMail,
    viralInviteMail,
    mailNewAccountCreatedByAdmin,
    resetPasswordMail,

    --flashmessages
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
    --utils  
    prettyName,
    userSmallView,
    userSmallViewWithDocsCount,
    UserSmallView(..)) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import User.UserState
import Kontra
import KontraLink
import Mails.SendMail(Mail,emptyMail,title,content)
import Templates.Templates 
import Templates.TemplatesUtils
import Data.Typeable
import Data.Data
import Text.StringTemplate.GenericStandard()

showUser :: KontrakcjaTemplates -> User -> [User] -> IO String 
showUser templates user viewers = renderTemplate templates "showUser" $
                                                        (setAttribute "fullname" $ BS.toString $ userfullname user ) .
                                                        (setAttribute "email" $ BS.toString $ unEmail $ useremail $ userinfo user) .
                                                        (setAttribute "companyname" $ BS.toString $ usercompanyname $ userinfo user) .
                                                        (setAttribute "companynumber" $ BS.toString $ usercompanynumber $ userinfo user) .
                                                        (setAttribute "invoiceaddress" $ BS.toString $ useraddress $ userinfo user) .
                                                        (setAttribute "viewers" $ map (BS.toString . prettyName)  viewers) .
                                                        (setAttribute "linkaccount" $ show LinkAccount) .
                                                        (setAttribute "linkaccountpassword" $ show LinkAccountPassword) .
                                                        (setAttribute "linksubaccount" $ show LinkSubaccount) 
    
pageAcceptTOS :: KontrakcjaTemplates ->  BS.ByteString -> IO String
pageAcceptTOS templates tostext = 
    renderTemplate templates "pageAcceptTOS" [("tostext", BS.toString tostext)]

  
viewSubaccounts :: KontrakcjaTemplates -> [User] -> IO String
viewSubaccounts templates subusers = renderTemplate templates "viewSubaccounts" $
                                                        (setAttribute "subusers" $ map userSmallView $ subusers) .
                                                        (setAttribute "subaccountslink" $ show LinkSubaccount) 

activatePageView::KontrakcjaTemplates -> String -> String ->  IO String
activatePageView templates tostext name = renderTemplate templates "activatePageView" [("tostext",tostext),("name",name)]

activatePageViewNotValidLink::KontrakcjaTemplates -> String ->  IO String
activatePageViewNotValidLink templates email = renderTemplate templates "activatePageViewNotValidLink" [("email",email)]


newPasswordPageView::KontrakcjaTemplates -> IO String
newPasswordPageView templates = renderTemplate templates "newPasswordPage" ()

resetPasswordMail::KontrakcjaTemplates -> String -> User -> KontraLink -> IO Mail
resetPasswordMail templates hostname user setpasslink =  do
           title <- renderTemplate templates "passwordChangeLinkMailTitle" ()
           content <- wrapHTML templates =<< renderTemplate templates "passwordChangeLinkMailContent" 
                                                                [("passwordlink",show setpasslink),
                                                                 ("ctxhostpart",hostname)]
           return $ emptyMail {title=BS.fromString title, content = BS.fromString content} 

newUserMail :: KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> KontraLink -> IO Mail
newUserMail templates hostpart emailaddress personname activatelink=
    do 
    title <- renderTemplate templates "newUserMailTitle" ()
    content <- wrapHTML templates =<< renderTemplate templates "newUserMailContent" [("personname",BS.toString $ personname),
                                                                 ("email",BS.toString $ emailaddress),
                                                                 ("activatelink",show activatelink),  
                                                                 ("ctxhostpart",hostpart)]
    return $ emptyMail {title=BS.fromString title, content = BS.fromString content} 
    
passwordChangeMail ::  KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString  -> KontraLink    -> IO Mail
passwordChangeMail templates hostpart emailaddress personname setpasslink = 
    do 
    title <- renderTemplate templates "passwordChangeMailTitle" ()
    content <- wrapHTML templates =<< renderTemplate templates "passwordChangeMailContent" 
                                                                [("personname",BS.toString $ personname),
                                                                 ("email",BS.toString $ emailaddress),
                                                                 ("passwordlink",show setpasslink),
                                                                 ("ctxhostpart",hostpart)]
    return $ emptyMail {title=BS.fromString title, content = BS.fromString content} 


inviteSubaccountMail ::  KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> KontraLink-> IO Mail
inviteSubaccountMail  templates hostpart supervisorname companyname emailaddress personname setpasslink = 
    do 
    title <- renderTemplate templates "inviteSubaccountMailTitle" ()
    content <- wrapHTML templates =<< renderTemplate templates "inviteSubaccountMailContent" 
                                                                [("personname",BS.toString $ personname),
                                                                 ("email",BS.toString $ emailaddress),
                                                                 ("passwordlink",show setpasslink),
                                                                 ("supervisorname",BS.toString $ supervisorname),  
                                                                 ("companyname",BS.toString $ companyname),
                                                                 ("ctxhostpart",hostpart)]
    return $ emptyMail {title=BS.fromString title, content = BS.fromString content}  

viralInviteMail :: KontrakcjaTemplates -> Context -> BS.ByteString -> KontraLink -> IO Mail
viralInviteMail templates ctx invitedemail setpasslink = do
    let invitername = BS.toString $ maybe BS.empty prettyName (ctxmaybeuser ctx)
    title <- renderTemplate templates "mailViralInviteTitle" 
                                      [("invitername", invitername)]
    content <- wrapHTML templates =<< renderTemplate templates "mailViralInviteContent"
                                                               [("email", BS.toString $ invitedemail),
                                                                ("invitername", invitername),
                                                                ("ctxhostpart", ctxhostpart ctx),
                                                                ("passwordlink", show setpasslink)]
    return $ emptyMail {title=BS.fromString title, content=BS.fromString content}

mailNewAccountCreatedByAdmin:: KontrakcjaTemplates -> Context-> BS.ByteString -> BS.ByteString -> KontraLink ->  IO Mail
mailNewAccountCreatedByAdmin templates ctx personname email setpasslink =    do 
      title <- renderTemplate templates "mailNewAccountCreatedByAdminTitle" ()
      content <- wrapHTML templates =<< renderTemplate templates "mailNewAccountCreatedByAdminContent"
                                                                 [("personname",BS.toString $ personname),
                                                                 ("email",BS.toString $ email),
                                                                 ("passwordlink",show setpasslink),
                                                                 ("creatorname", BS.toString$ maybe BS.empty prettyName (ctxmaybeuser  ctx)),  
                                                                 ("ctxhostpart",ctxhostpart ctx)]
      return $ emptyMail {title=BS.fromString title, content = BS.fromString content}   
    

flashMessageUserDetailsSaved:: KontrakcjaTemplates -> IO String
flashMessageUserDetailsSaved templates = renderTemplate templates "flashMessageUserDetailsSaved" () 

flashMessageMustAcceptTOS:: KontrakcjaTemplates -> IO String
flashMessageMustAcceptTOS templates = renderTemplate templates "flashMessageMustAcceptTOS" ()

flashMessagePasswordNotStrong:: KontrakcjaTemplates -> IO String
flashMessagePasswordNotStrong templates = renderTemplate templates "flashMessagePasswordNotStrong" ()

flashMessageBadOldPassword:: KontrakcjaTemplates -> IO String
flashMessageBadOldPassword templates= renderTemplate templates "flashMessageBadOldPassword" ()

flashMessagePasswordsDontMatch:: KontrakcjaTemplates -> IO String
flashMessagePasswordsDontMatch templates = renderTemplate templates"flashMessagePasswordsDontMatch" ()

flashMessageUserPasswordChanged:: KontrakcjaTemplates -> IO String
flashMessageUserPasswordChanged templates = renderTemplate templates "flashMessageUserPasswordChanged" ()

flashMessagePasswordChangeLinkNotValid:: KontrakcjaTemplates -> IO String
flashMessagePasswordChangeLinkNotValid templates = renderTemplate templates "flashMessagePasswordChangeLinkNotValid" ()

flashMessageUserWithSameEmailExists:: KontrakcjaTemplates -> IO String
flashMessageUserWithSameEmailExists templates = renderTemplate templates "flashMessageUserWithSameEmailExists" ()

flashMessageViralInviteSent:: KontrakcjaTemplates -> IO String
flashMessageViralInviteSent templates = renderTemplate templates "flashMessageViralInviteSent" ()

flashMessageActivationLinkNotValid:: KontrakcjaTemplates -> IO String
flashMessageActivationLinkNotValid templates = renderTemplate templates "flashMessageActivationLinkNotValid" ()

flashMessageUserActivated:: KontrakcjaTemplates -> IO String
flashMessageUserActivated templates = renderTemplate templates "flashMessageUserActivated" ()

flashMessageUserAlreadyActivated:: KontrakcjaTemplates -> IO String
flashMessageUserAlreadyActivated templates = renderTemplate templates "flashMessageUserAlreadyActivated" ()
                                                        
flashMessageNoSuchUserExists:: KontrakcjaTemplates -> IO String                                                        
flashMessageNoSuchUserExists templates = renderTemplate templates "flashMessageNoSuchUserExists" ()

flashMessageChangePasswordEmailSend:: KontrakcjaTemplates -> IO String                                                        
flashMessageChangePasswordEmailSend templates = renderTemplate templates "flashMessageChangePasswordEmailSend" ()

flashMessageNewActivationLinkSend:: KontrakcjaTemplates -> IO String                                                        
flashMessageNewActivationLinkSend templates = renderTemplate templates "flashMessageNewActivationLinkSend" ()

flashMessageUserSignupDone:: KontrakcjaTemplates -> IO String                                                        
flashMessageUserSignupDone templates = renderTemplate templates "flashMessageUserSignupDone" ()

{- Same as personname (username or email) from DocView but works on User -}
prettyName::User -> BS.ByteString
prettyName u = if (BS.null $ userfullname u)
               then unEmail $ useremail $ userinfo u 
               else userfullname u
          
{- View Utills  -}

{-| Users simple view (for templates) -}
data UserSmallView = UserSmallView {
                         usvId::String,  
                         usvFullname::String,
                         usvEmail::String,
                         usvDocsCount::String
                     } deriving (Data, Typeable)

{-| Conversion from 'User' to 'Option', for select box UserSmallView  -}      
userSmallView::User -> UserSmallView 
userSmallView u = UserSmallView {     usvId = (show $ userid u)
                                    , usvFullname = (BS.toString $ userfullname  u)
                                    , usvEmail = (BS.toString $ unEmail $ useremail $ userinfo u)
                                    , usvDocsCount = "" }

userSmallViewWithDocsCount::(User,Int) -> UserSmallView 
userSmallViewWithDocsCount (u,c) = UserSmallView { usvId = (show $ userid u)
                                                 , usvFullname = (BS.toString $ userfullname  u)
                                                 , usvEmail = (BS.toString $ unEmail $ useremail $ userinfo u)
                                                 , usvDocsCount = show c }

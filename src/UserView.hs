{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall #-}
module UserView(
   --pages
    viewSubaccounts,
    showUser,
    pageAcceptTOS,
    --mails  
    passwordChangeMail,
    newUserMail,
    inviteSubaccountMail,
    mailNewAccountCreatedByAdmin,
    --flashmessages
    flashMessageUserDetailsSaved,
    flashMessageMustAcceptTOS,
    flashMessagePasswordNotStrong,
    flashMessageBadOldPassword,
    flashMessagePasswordsDontMatch,
    --utils  
    prettyName) where

import HSP hiding (Request)
import Happstack.Server.SimpleHTTP
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import UserState
import AppView
import User
import KontraLink
import Mails.SendMail(Mail,emptyMail,title,content)
import qualified HSX.XMLGenerator
import Templates.Templates 
import Templates.TemplatesUtils

showUser :: User -> HSP.HSP HSP.XML
showUser user = 
    <div class="accounttable">
     <h1><% userfullname user %></h1>
      <div>
       <form action=LinkAccount method="post">
        <table>
         <tr><td>Namn:</td>
             <td><input type="text" name="fullname" value=(userfullname user)/></td>
         </tr>
         <tr><td>E-post:</td>
             <td><% unEmail $ useremail $ userinfo user %></td>
         </tr>
         <tr><td>Företagsnamn:</td>
             <td><input type="text" name="companyname" value=(usercompanyname $ userinfo user)/></td>
         </tr>
         <tr><td>Organisationsnummer:</td>
             <td><input type="text" name="companynumber" value=(usercompanynumber $ userinfo user)/></td>
         </tr>
         <tr><td>Faktureringsadress:</td>
             <td><input type="text" name="invoiceaddress" value=(useraddress $ userinfo user)/></td>
         </tr>
       </table>
       <input class="button" type="submit" value="Spara ändringar"/>
      </form>
      <div>
           <form action=LinkAccountPassword method="post">
            <table>
             <tr><td>Nuvarande lösenord:</td>
              <td><input type="password" name="oldpassword" autocomplete="off" /></td>
             </tr>
             <tr><td>Nytt lösenord:</td>
              <td><input type="password" name="password" autocomplete="off" /></td>
             </tr>
             <tr><td>Upprepa nytt lösenord:</td>
              <td><input type="password" name="password2" autocomplete="off" /></td>
             </tr>
            </table>
            <input class="button" type="submit" value="Ändra lösenord"/>
           </form>
          </div>
          <div>
           <a href=LinkSubaccount>Underkonton</a>
          </div>

     </div>
    </div>

pageAcceptTOS :: Context -> BS.ByteString -> Kontra Response
pageAcceptTOS ctx tostext = 
    let toptab = TopEmpty
    in
    renderFromBody ctx toptab kontrakcja $
     <form method="post" action="/accepttos" id="toscontainer" class="overlay">
      <a class="close"> </a>                  
      <h2>Vänligen acceptera användarvillkoren</h2>
      <% cdata $ BS.toString tostext %>
     <br/>
     <input type="checkbox" name="tos" id="tos"/>Jag har läst och accepterar SkrivaPå Allmänna Villkor<br/>
     <div class="buttonbox">
      <input class="submiter button" type="submit" value="Skicka"/>
     </div> 
    </form>
  
oneRow :: (EmbedAsAttr m (Attr [Char] [Char]), EmbedAsAttr m (Attr [Char] UserID)) =>  User -> XMLGenT m (HSX.XMLGenerator.XML m)
oneRow (user)  = 
    <tr class="ui-state-default">
     <td class="tdleft">
      <input type="checkbox" name="doccheck" value=(userid user) class="check" />
     </td>
     <td><img width="17" height="17" src=""/></td>
     <td><% userfullname user%></td>
     <td><% useremail $ userinfo user %></td>
     <td> - </td>
     <td class="tdright"></td>
    </tr>


viewSubaccounts :: Context -> [User] -> Kontra Response
viewSubaccounts ctx subusers = 
    renderFromBody ctx TopAccount kontrakcja $ 
    <form method="post" action= LinkSubaccount>
     <h1>Underkonton</h1>
     <table class="doctable" cellspacing="0">
      <col/>
      <col/>
      <col/>
      <col/>
      <col/>
      <thead>
       <tr>
        <td><a href="#" id="all">Alla</a></td>
        <td></td> {- status icon -}
        <td>Namn</td>
        <td>E-post</td>
        <td>Övrigt</td>
        <td></td>
       </tr>
      </thead>
      <tfoot>
       <tr>
        <td colspan="6" style="text-align: right; overflow: hidden;">
          <div class="floatleft">
           <input type="submit" class="button" name="remove" value="Radera"/>
          </div>
          {-
          <div class="floatright">
           <img src="/theme/images/status_draft.png"/> Utkast
           <img src="/theme/images/status_rejected.png"/> Avbrutet
           <img src="/theme/images/status_timeout.png"/> Förfallet
           <img src="/theme/images/status_pending.png"/> Väntar
           <img src="/theme/images/status_viewed.png"/> Granskat
           <img src="/theme/images/status_signed.png"/> Undertecknat
          </div>
          -}
          <div class="clearboth"/>
         </td>
       </tr>
      </tfoot>
     
      <tbody id="selectable">
       <% map oneRow subusers %>
      </tbody>
     </table><br/>
      <table>
	<tr> 
          <td>Namn:</td> 
          <td><input type="text" name="fullname"/></td> 
        </tr>
	<tr>
          <td>E-post:</td> 
          <td><input type="email" name="email"/></td> 
        </tr>
	<tr> 
          <td><input class="button" id="create" type="submit" name="create" value="Skapa ny"/></td>
          <td></td>
	</tr>
      </table>
    </form>

newUserMail :: KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO Mail
newUserMail templates hostpart emailaddress personname newpassword =
    do 
    title <- renderTemplate templates "newUserMailTitle" []
    content <- wrapHTML templates =<< renderTemplate templates "newUserMailContent" [("personname",BS.toString $ personname),
                                                                 ("email",BS.toString $ emailaddress),
                                                                 ("password",BS.toString $ newpassword),
                                                                 ("ctxhostpart",hostpart)]
    return $ emptyMail {title=BS.fromString title, content = BS.fromString content} 
    
passwordChangeMail ::  KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString  -> BS.ByteString    -> IO Mail
passwordChangeMail templates hostpart emailaddress personname newpassword = 
    do 
    title <- renderTemplate templates "passwordChangeMailTitle" []
    content <- wrapHTML templates =<< renderTemplate templates "passwordChangeMailContent" 
                                                                [("personname",BS.toString $ personname),
                                                                 ("email",BS.toString $ emailaddress),
                                                                 ("password",BS.toString $ newpassword),
                                                                 ("ctxhostpart",hostpart)]
    return $ emptyMail {title=BS.fromString title, content = BS.fromString content} 


inviteSubaccountMail ::  KontrakcjaTemplates -> String -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO Mail
inviteSubaccountMail  templates hostpart supervisorname companyname emailaddress personname newpassword = 
    do 
    title <- renderTemplate templates "inviteSubaccountMailTitle" []
    content <- wrapHTML templates =<< renderTemplate templates "inviteSubaccountMailContent" 
                                                                [("personname",BS.toString $ personname),
                                                                 ("email",BS.toString $ emailaddress),
                                                                 ("password",BS.toString $ newpassword),
                                                                 ("supervisorname",BS.toString $ supervisorname),  
                                                                 ("companyname",BS.toString $ companyname),
                                                                 ("ctxhostpart",hostpart)]
    return $ emptyMail {title=BS.fromString title, content = BS.fromString content}   

mailNewAccountCreatedByAdmin:: KontrakcjaTemplates -> Context-> BS.ByteString -> BS.ByteString -> BS.ByteString ->  IO Mail
mailNewAccountCreatedByAdmin templates ctx personname email password =    do 
      title <- renderTemplate templates "mailNewAccountCreatedByAdminTitle" []
      content <- wrapHTML templates =<< renderTemplate templates "mailNewAccountCreatedByAdminContent"
                                                                 [("personname",BS.toString $ personname),
                                                                 ("email",BS.toString $ email),
                                                                 ("password",BS.toString $ password),
                                                                 ("creatorname", BS.toString$ maybe BS.empty prettyName (ctxmaybeuser  ctx)),  
                                                                 ("ctxhostpart",ctxhostpart ctx)]
      return $ emptyMail {title=BS.fromString title, content = BS.fromString content}   
    

flashMessageUserDetailsSaved:: KontrakcjaTemplates -> IO String
flashMessageUserDetailsSaved templates = renderTemplate templates "flashMessageUserDetailsSaved" [] 

flashMessageMustAcceptTOS:: KontrakcjaTemplates ->IO String
flashMessageMustAcceptTOS templates = renderTemplate templates "flashMessageMustAcceptTOS" []

flashMessagePasswordNotStrong:: KontrakcjaTemplates ->IO String
flashMessagePasswordNotStrong templates = renderTemplate templates "flashMessagePasswordNotStrong" []

flashMessageBadOldPassword:: KontrakcjaTemplates ->IO String
flashMessageBadOldPassword templates= renderTemplate templates "flashMessageBadOldPassword" []

flashMessagePasswordsDontMatch:: KontrakcjaTemplates ->IO String
flashMessagePasswordsDontMatch templates = renderTemplate templates"flashMessagePasswordsDontMatch" []

{- Same as personname (username or email) from DocView but works on User -}
prettyName::User -> BS.ByteString
prettyName u = if (BS.null $ userfullname u)
               then unEmail $ useremail $ userinfo u 
               else userfullname u
          

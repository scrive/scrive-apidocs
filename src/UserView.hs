{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module UserView where

import HSP hiding (Request)
import System.Locale (defaultTimeLocale)
import Control.Monad.Trans (MonadIO,liftIO,lift)
import Happstack.Server (Response)
import Happstack.Server.HStringTemplate (webST)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.SimpleHTTP
import qualified HSX.XMLGenerator as HSX (XML)
import Control.Monad
import Data.Object.Json as Json
import Data.Object as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSCL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.UTF8 as BSC
import Network.HTTP (urlEncode)
import Data.Time
import UserState
import Misc
import AppView
import User
import KontraLink
import DocView
import MinutesTime

instance (EmbedAsChild m BS.ByteString) => (EmbedAsChild m Email) where
    asChild = asChild . unEmail

instance Monad m => IsAttrValue m Email where
    toAttrValue = toAttrValue . unEmail

instance Monad m => IsAttrValue m UserID where
    toAttrValue = toAttrValue . show


showUser ctx@(Context {ctxmaybeuser = Just user}) = 
    webHSP $ pageFromBody ctx TopAccount kontrakcja $ 
    <div class="doctable">
     <h1>Välkommen <% userfullname user %></h1>

      <div class="inlinebox">
       <form action=LinkAccount method="post">
        <% tosMessage user %>
        <table>
         <tr><td>Namn:</td>
             <td><input type="text" name="fullname" value=(userfullname user)/></td>
         </tr>
         <tr><td>E-mail:</td>
             <td><% unEmail $ useremail user %></td>
         </tr>
         <tr><td>Företagsnamn:</td>
             <td><input type="text" name="companyname" value=(usercompanyname user)/></td>
         </tr>
         <tr><td>Organisationsnummer:</td>
             <td><input type="text" name="companynumber" value=(usercompanynumber user)/></td>
         </tr>
         <tr><td>Faktureringsadress:</td>
             <td><input type="text" name="invoiceaddress" value=(userinvoiceaddress user)/></td>
         </tr>
         <tr><td>Old password:</td>
             <td><input type="password" name="oldpassword"/></td>
         </tr>
         <tr><td>New password:</td>
             <td><input type="password" name="password"/></td>
         </tr>
         <tr><td>New password again:</td>
             <td><input type="password" name="password2"/></td>
         </tr>
       </table>
       <br/>
       <input class="button" type="submit" value="Change details"/>
      </form>
      <a href=LinkSubaccount>Subaccounts</a>
     </div>
    </div>
  

oneRow user@User{ userfullname, useremail, userid }  = 
    let link = "nothing to see here"
        statusimg = ""
    in
    <tr>
     <td class="tdleft">
      <input type="checkbox" name="usercheck" value=userid/>
     </td>
     <td><img width="17" height="17" src=statusimg/></td>
     <td><% userfullname %></td>
     <td><% useremail %></td>
     <td> - </td>
     <td class="tdright"></td>
    </tr>


viewSubaccounts :: Context -> [User] -> Kontra Response
viewSubaccounts ctx@(Context {ctxmaybeuser = Just user}) subusers = 
    webHSP $ pageFromBody ctx TopAccount kontrakcja $ 
    <form method="post" action= LinkSubaccount>
     <h1>Subaccounts</h1>
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
        <td>E-mail</td>
        <td>Other possible data goes here</td>
        <td></td>
       </tr>
      </thead>
      <tfoot>
       <tr>
        <td colspan="6" style="text-align: right; overflow: hidden;">
          <span> Footer </span>
          {-
          <div class="floatleft">
           <input type="submit" class="button" name="archive" value="Radera"/>
          </div>
          <div class="floatright">
           <img src="/theme/images/status_draft.png"/> Utkast
           <img src="/theme/images/status_rejected.png"/> Avbrutet
           <img src="/theme/images/status_timeout.png"/> Förfallet
           <img src="/theme/images/status_pending.png"/> Väntar
           <img src="/theme/images/status_viewed.png"/> Granskat
           <img src="/theme/images/status_signed.png"/> Undertecknat
          </div>
          <div class="clearboth"/>
          -}
         </td>
       </tr>
      </tfoot>
     
      <tbody>
       <% map oneRow subusers %>
      </tbody>
     </table><br/>
      <table>
	<tr> 
          <td>Namn:</td> 
          <td><input type="textfield" name="fullname"/></td> 
        </tr>
	<tr>
          <td>E-mail:</td> 
          <td><input type="textfield" name="email"/></td> 
        </tr>
	<tr> 
          <td><input class="button" type="submit" name="create" value="Skapa ny"/></td>
          <td></td>
	</tr>
      </table>
    </form>

newUserMail :: BS.ByteString -> BS.ByteString -> IO BS.ByteString
newUserMail emailaddress personname =
    htmlHeadBodyWrapIO ""
     <span>Hello <% personname %>. Welcome to SkrivaPå. </span>

passwordChangeMail :: BS.ByteString
                   -> BS.ByteString
                   -> BS.ByteString
                   -> IO BS.ByteString
passwordChangeMail emailaddress personname newpassword = 
   htmlHeadBodyWrapIO ""
    <span>
      <p>Hej <strong><% personname %></strong>,</p>

      <p>Jag heter Lukas Duczko och är VD på SkrivaPå. Tack för att du har skapat ett konto hos oss. 
         Vi hoppas att du kommer att bli nöjd med våra tjänster. Tveka inte att höra av dig med 
         feedback eller bara en enkel hälsning. Din åsikt är värdefull.</p>

      <p>Användarnamn: <span style="color: orange; text-weight: bold"><% emailaddress %></span><br/>
         Lösenord: <span style="color: orange; text-weight: bold"><% newpassword %></span><br/>
      </p>
      <p>
      <a href="http://skrivapa.se/login">http://skrivapa.se/login</a>
      </p>

      <p>Med vänliga hälsningar<br/>/Lukas Duczko och team <a href="http://skrivapa.se/">SkrivaPå</a>.
      </p>
     </span>



inviteSubaccountMail :: BS.ByteString
                     -> BS.ByteString
                     -> BS.ByteString
                     -> BS.ByteString
                     -> BS.ByteString
                     -> IO BS.ByteString
inviteSubaccountMail supervisorname companyname emailaddress personname newpassword = 
    htmlHeadBodyWrapIO ""
     <span>
      <p>Hej <strong><% personname %></strong>,</p>

      <p><strong><% supervisorname %></strong> har bjudit in dig att öppna ett konto på SkrivaPå genom vilket du kan
         skriva avtal för <strong><% companyname %></strong>. Observera att detta konto inte är
         avsett för privat bruk.</p>

      <p>Användarnamn: <span style="color: orange; text-weight: bold"><% emailaddress %></span><br/>
         Lösenord: <span style="color: orange; text-weight: bold"><% newpassword %></span><br/>
      </p>

      <p>
      <a href="http://skrivapa.se/login">http://skrivapa.se/login</a>
      </p>

      <% poweredBySkrivaPaPara %> 
     </span>

userDetailsSavedFlashMessage :: (MonadIO m) => m FlashMessage
userDetailsSavedFlashMessage = liftM (FlashMessage . renderXMLAsBSHTML) $ webHSP1
    <div>Dina kontoändringar har sparats.</div>

acceptTermsOfServiceForm :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
                            => XMLGenT m (HSX.XML m)
acceptTermsOfServiceForm = <input type="checkbox" name="tos" id="tos">I accept and agree to the Terms of Service</input>

acceptedTermsOfServiceMessage :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
                                 => MinutesTime -> XMLGenT m (HSX.XML m)
acceptedTermsOfServiceMessage time = <span>You accepted the terms of service agreement on <% showDateOnly time %>. </span>

tosMessage user@(User { userhasacceptedtermsofservice = Just time }) = acceptedTermsOfServiceMessage time
tosMessage _ = acceptTermsOfServiceForm
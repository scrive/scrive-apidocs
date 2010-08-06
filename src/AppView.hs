{-# LANGUAGE FlexibleContexts, FlexibleInstances, IncoherentInstances,
             MultiParamTypeClasses, NamedFieldPuns, CPP #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module AppView where

import HSP hiding (Request)
import System.Locale (defaultTimeLocale)
import Control.Monad.Trans (MonadIO,liftIO,lift)
import Happstack.Server (Response)
import Happstack.Server.HStringTemplate (webST)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.SimpleHTTP
import qualified HSX.XMLGenerator as HSX (XML)
import qualified HSX.XMLGenerator
import Control.Monad
import Data.Object.Json as Json
import Data.Object as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSCL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.UTF8 as BSC
import User
import Network.HTTP (urlEncode)
import Data.Time
import qualified Data.Map as Map
import Misc
import HSP.XML
import KontraLink
import System.Directory

instance (XMLGenerator m) => (EmbedAsChild m HeaderPair) where
  asChild (HeaderPair name value) = 
     <% <p> 
         <% BS.toString name ++ ": " ++ show value  %> 
        </p> 
      %>	


data TopMenu = TopNew | TopDocument | TopAccount | TopNone | TopEmpty
             deriving (Eq,Ord)

instance (XMLGenerator m) => (EmbedAsChild m User) where
  asChild user = <% BS.toString (userfullname user) ++ " <" ++ 
                 BS.toString (unEmail $ useremail user) ++ ">"  %>	

#if MIN_VERSION_happstack_server(0,5,1)
rqInputs rq = rqInputsQuery rq ++ rqInputsBody rq
#endif

instance (XMLGenerator m) => (EmbedAsChild m Request) where
  asChild rq = <% <code>
                  <% show (rqMethod rq) %> <% rqUri rq ++ rqQuery rq %><br/>
                  <% map asChild1 (rqInputs rq) %>
                 </code> %>
    where asChild1 (name,input) = 
              <% <span><% name %>: <% input %><br/></span> %>

instance (XMLGenerator m) => (EmbedAsChild m Input) where
  asChild (Input _value (Just filename) _contentType) = 
       <% "File " ++ show filename %>
  asChild (Input value _maybefilename _contentType) = 
       <% show (concatMap BSC.toString (BSCL.toChunks value)) %>

instance (XMLGenerator m) => (EmbedAsChild m Json.JsonScalar) where
  asChild (JsonString x) = <% BS.toString x %>	
  asChild (JsonNumber x) = <% show x %>	
  asChild (JsonBoolean x) = <% show x %>
  asChild JsonNull = <% "null" %>

s (k, v) = <li><% BS.toString k %>: <% v %></li>

y v = <li><% v %></li>



instance (EmbedAsChild m HSP.XML.XML) => (EmbedAsChild m FlashMessage) where
  asChild (FlashMessage msg) = 
      <% cdata $ BSC.toString msg %>


instance (XMLGenerator m) => 
    (EmbedAsChild m (Json.Object BS.ByteString Json.JsonScalar)) where
  asChild (Json.Mapping xs) =
    <%
      <ul>
        <% map s xs %>
      </ul>
    %>
  asChild (Json.Sequence xs) =
    <%
     <ol>
      <% map y xs %>
     </ol>
    %>
  asChild (Json.Scalar val) =
    <%
     <span><% val %></span>
    %>

kontrakcja :: [Char]
kontrakcja = "SkrivaPå"

-- small letter ascii only version on app name
kontrakcjaAscii :: [Char]
kontrakcjaAscii = "skriva"


handleRPXLoginView :: (XMLGenerator m) 
                   => Json.JsonObject
                   -> XMLGenT m (HSX.XML m)
handleRPXLoginView json =
    <p>
      Welcome! This is what we know about you:
      <% json %>
    </p>

loginBox ctx =
   <div>
    <div id="login">
     <form action=LinkLogin method="post">
      <table>
	<tr>
          <td>E-mail:</td> 
          <td><input type="textfield" name="email" size="22"/></td> 
        </tr>
	<tr> 
          <td>Lösenord:</td> 
          <td><input type="password" name="password" size="22"/></td> 
        </tr>
	<tr> 
          <td><input class="button" type="submit" name="login" value="Logga in"/></td>
          <td>
            {- <a href="#" onClick="$('#login').hide(); $('#register').show(); return false;">register</a> -}
           <% rpxSignInLink ctx "Logga in med OpenID" "/" %>
          </td>
	</tr>
      </table>
    </form>
    </div>
    {-
    <div id="register" style="display: none;">
     <form action="/tutorial/actions/newuser" method="post">
      <table>
 	<tr>
          <td>Email:</td>
          <td><input type="textfield" name="email"/></td>
        </tr>
	<tr>
          <td>Password:</td>
          <td><input type="password" name="password"/></td>
        </tr>
	<tr>
          <td>Verify Password:</td>
          <td><input type="password" name="password2"/></td>
        </tr>
	<tr>
          <td><input type="submit" name="create" value="Create Account"/></td>
          <td><a href="#" onClick="$('#register').hide(); $('#login').show(); return false;">login</a></td>
        </tr>
      </table>
     </form>
    </div>
    -}
   </div>

welcomeBody :: (XMLGenerator m, EmbedAsAttr m (Attr [Char] KontraLink)) 
            => Context 
            -> XMLGenT m (HSX.XML m)
welcomeBody (Context {ctxmaybeuser = Just _, ctxhostpart}) = 
  <div class="centerdivnarrow">
   <p class="headline">Välkommen till SkrivaPå!</p>

   <form action=LinkIssue method="post" enctype="multipart/form-data">
    <span class="small">Ladda upp dokument</span><br/>
    <input class="multi" maxlength="1" type="file" name="doc" accept="pdf"/>
    <input class="bigbutton" type="submit" value="Skapa"/>
   </form>
   <hr/>
  </div>


welcomeBody ctx@(Context {ctxmaybeuser = Nothing}) = 
  <div class="centerdiv" style="width: 280px"> 
   <div style="text-align: center">
    <img src="/theme/images/logolarge.png"/><br/>
    <hr/>
    <p class="headline" style="font-size: 200%; text-align: center">
    </p>
      <% loginBox ctx %>
   </div>

   <hr/>

   <p class="headline">Välkommen till SkrivaPå!</p>

   <p class="para">För tillfället testar vi vår online signaturlösning med 
      utvalda kunder. Om du vill bli en tidig testkund, vänligen 
      <a href="mailto:lukas@skrivapa.se">skicka ett mail</a>. Om du redan 
      har ett konto logga in för att börja.</p>
  </div>

errorReport :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
            => Context 
            -> Request 
            -> XMLGenT m (HSX.XML m)
errorReport (Context {ctxmaybeuser}) request = 
  <div>
   <p>Ett fel har uppstått. Det beror inte på dig. Det beror på oss. Vi tar 
      hand om problemet så snart vi kan. Tills vi fixat problemet, vänligen 
      försök igen genom att börja om från <a href="/">startsidan</a>.</p>
   <hr/>
   <p>Information useful to developers:</p>
   <% case ctxmaybeuser of
           Just user -> <p>Logged in as: <% user %></p>
           Nothing -> <p>Not logged in</p>
   %>
   <p><% request %></p>
   <p>HTTP Headers:</p>
   <p><% Map.elems (rqHeaders request) %></p> 
   <p>HTTP Cookies:</p>
   <p><% map show $ rqCookies request %></p> 
  </div>  

{-
   <p>
     <iframe src=("http://" ++ kontrakcjaAscii ++ 
                  ".rpxnow.com/openid/embed?token_url=" ++ serverurl ++ "rpxsignin")
             scrolling="no"  
             frameBorder="no"  
             allowtransparency="true"  
             style="width:400px;height:240px">
     </iframe> 
   </p>

Your token URL implementation should do the following:

    * 1) Extract the token parameter from the POST data.
    * 2) Use the token to make the auth_info API call:
      URL: http://rpxnow.com/api/v2/auth_info
      Parameters:

      apiKey
          03bbfc36d54e523b2602af0f95aa173fb96caed9
      token
          The token value you extracted above

    * 3) Parse the response and extract the identifier. Here's a sample JSON response:

      {
        'stat': 'ok',
        'profile': {
          'identifier': 'http://user.myopenid.com/',
          'email': 'user@example.com',
          'preferredUsername': 'Joe User'
         }
      }

    * 4) Use the identifier as the unique key to sign the user in to your website, 
         and then redirect the user to the appropriate location.
-}

-- * Convenience Functions

dateStr :: UTCTime -> String
dateStr ct =
  formatTime
    defaultTimeLocale
    "%a, %B %d, %Y at %H:%M:%S (UTC)" ct

-- * Main Implementation

renderFromBody :: (MonadIO m, EmbedAsChild (HSPT' IO) xml) 
               => Context 
               -> TopMenu 
               -> String 
               -> xml 
               -> m Response
renderFromBody ctx topmenu title = webHSP . pageFromBody ctx topmenu title

topnavi :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) => Bool -> Context -> String 
        -> KontraLink -> XMLGenT m (HSX.XML m)
topnavi True ctx title link = 
    maybeSignInLink2 ctx title link "active"

topnavi False ctx title link = 
    maybeSignInLink2 ctx title link ""

pageFromBody :: (EmbedAsChild (HSPT' IO) xml) 
             => Context 
             -> TopMenu 
             -> String 
             -> xml 
             -> HSP XML
pageFromBody ctx@(Context {ctxmaybeuser,ctxhostpart,ctxflashmessages}) 
             topMenu title body =
    withMetaData html4Strict $
    <html>
     <head>
      <title><% title %></title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <link rel="stylesheet" type="text/css" href="/theme/style.css" media="screen" />
      <link rel="stylesheet" type="text/css" href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.2/themes/ui-lightness/jquery-ui.css" media="screen" />
      {- <script src="/js/jquery-1.4.2.min.js"/> -}
      <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"/>
      <script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.2/jquery-ui.min.js"/>
      {- <script src="/js/jquery-ui-1.8.custom.min.js"/> -}
      <script src="/js/jquery.MultiFile.js"/>
      <script src="/js/global.js"/>
     </head>
     <body>
      <%
       case ctxflashmessages of
         [] -> <% () %>
         _ -> <% <div class="flashmsgbox">
               <% ctxflashmessages  %>
              </div> %>
       %>
      <div id="headerContainer">
        <img class="logosmall"src="/theme/images/logosmall.png"/>
        <span class="contactabout">
         <%
           case ctxmaybeuser of
             Just _ -> <a href="/logout">Logga ut</a>
             Nothing -> <a href="/login">Logga in</a>
         %> | <a href=LinkAbout>Om SkrivaPå</a></span>
        <div id="headerContainer2">
         <div id="nav">
          <% case ctxmaybeuser of 
               Just _ ->
                 <ul>
                   <li><% topnavi (topMenu== TopNew) ctx "Skapa" LinkMain %></li>
                   <li><% topnavi (topMenu== TopDocument) ctx "Avtal" LinkIssue %></li>
                   <li><% topnavi (topMenu== TopAccount) ctx "Konto" LinkAccount %></li>
                 </ul>
               _ -> <span/>
           %>
         </div>
        </div>
        <div class="clearboth"/>
      </div>
      <div id="mainContainer">
          <% body %>
      </div>
      <div id="footerContainer">
       <div id="footerContainer2">
        <span><a href="/termsofuse.html">Allmänna villkor</a></span>
       </div>
      </div>

      <script type="text/javascript">
         var rpxJsHost = (("https:" == document.location.protocol) ? "https://" : "http://static.");
         document.write(unescape("%3Cscript src='" + rpxJsHost +
                "rpxnow.com/js/lib/rpx.js' type='text/javascript'%3E%3C/script%3E"));
      </script>
      <script type="text/javascript">
         RPXNOW.overlay = true;
         RPXNOW.language_preference = 'en';
      </script>
      <script type="text/javascript">
        var _gaq = _gaq || [];
        _gaq.push(['_setAccount', 'UA-6387711-9']);
        _gaq.push(['_trackPageview']);

        (function() {
          var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
          ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
          var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
          })();

      </script>     
     </body>
    </html>


seeOtherXML :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
seeOtherXML url = <a href=url alt="303 see other"><% url %></a>


showUserOption :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] BSC.ByteString)) 
                  => User -> XMLGenT m (HSX.XML m)
showUserOption user =
    <option value=(show $ userid user) title=(unEmail $ useremail user)>
          <% userfullname user %> <% unEmail $ useremail user %>
    </option>
 
statsPageView :: Int -> Int -> [User] -> BS.ByteString -> HSP XML
statsPageView nusers ndocuments users df =
    withMetaData html4Strict $
    <html>
     <head>
      <title>Stats page</title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
     </head>
     <body>
      <h1>Stats page</h1>
      <table>
       <tr><td>Users</td><td><% show nusers %></td></tr>
       <tr><td>Documents</td><td><% show ndocuments %></td></tr>
      </table>
      <br/>
      <form method="post" action="/become">
       Become user: 
       <table>
        <tr>
         <td>
          <select name="user">
            <% map showUserOption users %>
          </select>
         </td>
         <td>
          <input class="secbutton" type="submit" value="Become"/>
         </td>
        </tr>
       </table>
      </form>
      <br/>
      <form method="post" action="/createuser">
       Create user:<br/> 
       <table>
        <tr>
         <td>Full name:</td>
         <td><input type="textfield" name="fullname"/></td>
        </tr>
        <tr>
         <td>Email address:</td>
         <td><input type="textfield" name="email"/></td>
        </tr>
       </table><br/>
       <input class="secbutton" type="submit" value="Create user"/>
      </form>
      <br/>
      Disk status:<br/>
      <pre><% df %></pre>
     </body>
    </html>


loginPageView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
               => Context -> XMLGenT m (HSX.XML m)
loginPageView ctx = 
  <div class="centerdivnarrow">

   <p class="headline">Logga in SkrivaPå!</p>

   <% loginBox ctx %>

  </div>


developmentWrapper :: (EmbedAsChild (HSPT' IO) body) 
                   => String 
                   -> body 
                   -> HSP XML
developmentWrapper title body =
    withMetaData html4Strict $
    <html>
     <head>
      <title><% title %></title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
     </head>
     <body>
      <% body %>
     </body>
    </html>

databaseContents :: [String] -> HSP XML
databaseContents contents = developmentWrapper "All database files" 
  <div>
     <h1>All database files</h1>
     <ul>
      <% map showOneFile contents %>
     </ul>
  </div>
  where showOneFile file = <li><a href=file><% file %></a></li>

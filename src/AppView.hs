{-# LANGUAGE FlexibleContexts, FlexibleInstances, 
             MultiParamTypeClasses, NamedFieldPuns #-}
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

instance (XMLGenerator m) => (EmbedAsChild m 
                              HeaderPair) where
  asChild (HeaderPair name value) = <% <p> <% BS.toString name ++ ": " ++ 
                 show value  %> </p> %>	


data TopMenu = TopNew | TopDocument | TopAccount | TopNone
             deriving (Eq,Ord)

instance (XMLGenerator m) => (EmbedAsChild m 
                              User) where
  asChild user = <% BS.toString (fullname user) ++ " <" ++ 
                 BS.toString (email user) ++ ">"  %>	

instance (XMLGenerator m) => (EmbedAsChild m 
                              Request) where
  asChild rq = <% <code>
                  <% show (rqMethod rq) %> <% rqUri rq ++ rqQuery rq %><br/>
                  <% map asChild1 (rqInputs rq) %>
                 </code> %>
    where asChild1 (name,input) = <% <span><% name %>: <% input %><br/></span> %>

instance (XMLGenerator m) => (EmbedAsChild m 
                              Input) where
  asChild (Input _value (Just filename) _contentType) = <% "File " ++ show filename %>
  asChild (Input value _maybefilename _contentType) = <% show (concatMap BSC.toString (BSCL.toChunks value)) %>

instance (XMLGenerator m) => (EmbedAsChild m 
                              (Json.JsonScalar)) where
  asChild (JsonString x) = <% BS.toString x %>	
  asChild (JsonNumber x) = <% show x %>	
  asChild (JsonBoolean x) = <% show x %>
  asChild JsonNull = <% "null" %>

s (k, v) = <li><% BS.toString k %>: <% v %></li>

y v = <li><% v %></li>

instance (XMLGenerator m) => (EmbedAsChild m 
                              (Json.Object BS.ByteString Json.JsonScalar)) where
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
kontrakcja = "skrivaPå"

-- small letter ascii only version on app name
kontrakcjaAscii :: [Char]
kontrakcjaAscii = "skriva"


handleRPXLoginView :: (XMLGenerator m) => Json.JsonObject
                      -> XMLGenT m (HSX.XML m)
handleRPXLoginView json =
    <p>
      Welcome! This is what we know about you:
      <% json %>
    </p>

welcomeBody :: (XMLGenerator m) => Context -> XMLGenT m (HSX.XML m)
welcomeBody (Context (Just _) hostpart) = 
  <div class="centerdiv" style="width: 300px">
   <img src="/theme/images/logolarge.png"/>
   <br/> 
   <form action="/issue" method="post" enctype="multipart/form-data">
    <span class="small">Ladda upp dokument</span><br/>
    <input type="file" name="doc"/>
    <input class="button" type="submit" value="Skapa"/>
   </form>
   <hr/>

   <p class="headline">Välkommen till skrivaPå!</p>

   <p class="para">För tillfället testar vi vår online signaturlösning med utvalda kunder. Om du vill bli en tidig testkund, vänligen <a href="mailto:lukas@skrivapa.se">skicka ett mail</a>. Om du redan har ett konto klicka nedan för att börja.</p>
  </div>

welcomeBody ctx@(Context Nothing hostpart) = 
  <div class="centerdiv" style="width: 280px"> 
   <img src="/theme/images/logolarge.png"/><br/>
   <hr/>
   <p class="headline" style="font-size: 200%; text-align: center">
      <% maybeSignInLink ctx "Sign in here" "/" %>
   </p>

{-
   <span class="small">Choose a document to upload:</span><br/>
   <input type="upload"><input type="submit" value="New">
-}   

   <hr/>

   <p class="headline">Välkommen till skrivaPå!</p>

   <p class="para">För tillfället testar vi vår online signaturlösning med utvalda kunder. Om du vill bli en tidig testkund, vänligen <a href="mailto:lukas@skrivapa.se">skicka ett mail</a>. Om du redan har ett konto klicka nedan för att börja.</p>
  </div>

errorReport :: (XMLGenerator m) => Context -> Request -> XMLGenT m (HSX.XML m)
errorReport (Context maybeuser _) request = 
  <div>
   <p>Ett fel har uppstått. Det beror inte på dig. Det beror på oss. Vi tar hand om problemet så snart vi kan så snälla ha tålamod med oss. Tills vi fixat problemet, vänligen försök igen genom att börja om från <a href="/">startsidan</a>.</p>
   <hr/>
   <p>Information useful to developers:</p>
   <% case maybeuser of
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

renderFromBody :: (MonadIO m, EmbedAsChild (HSPT' IO) xml) => Context -> TopMenu -> String -> xml -> m Response
renderFromBody ctx topmenu title = webHSP . pageFromBody ctx topmenu title

topnavi :: (XMLGenerator m) => Bool -> Context -> String -> String -> XMLGenT m (HSX.XML m)
topnavi True ctx title link = 
    -- FIXME: add active attribute here
    maybeSignInLink2 ctx <span><span class="activeleft"/><span class="activemid"><% title %></span><span class="activeright"/></span> link "active"

topnavi False ctx title link = 
    maybeSignInLink2 ctx <span><span class="inactiveleft"/><span class="inactivemid"><% title %></span><span class="inactiveright"/></span> link ""

pageFromBody :: (EmbedAsChild (HSPT' IO) xml) => Context -> TopMenu -> String -> xml -> HSP XML
pageFromBody ctx@(Context maybeuser hostpart) topMenu title body =
    withMetaData html4Strict $
    <html>
     <head>
      <title><% title %></title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <link rel="stylesheet" type="text/css" href="/theme/style.css" media="screen" />
      <link rel="stylesheet" type="text/css" href="/css/ui-lightness/jquery-ui-1.8.custom.css" media="screen" />
     </head>
     <body>
      <div id="headerContainer">
        <img class="logosmall"src="/theme/images/logosmall.png" height="40"/>
        <span class="contactabout">
         <%
           case maybeuser of
             Just _ -> <a href="/logout">Logout</a>
             Nothing -> maybeSignInLink ctx "Login" "/"
         %> | <a href="about">Om oss</a></span>
        <div id="headerContainer2">
         <div id="nav">
          <ul>
           <li><% topnavi (topMenu== TopNew) ctx "Skapa" "/" %></li>
           <li><% topnavi (topMenu== TopDocument) ctx "Dokument" "/issue" %></li>
           <li><% topnavi (topMenu== TopAccount) ctx "Konto" "/account" %></li>
          </ul>
         </div>
        </div>
      </div>
      <br/> {- the navi things are float: left, this clears the flow -}
      <div id="mainContainer">
          <% body %>
      </div>
      <div id="footerContainer">
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


showUserOption :: (XMLGenerator m) => User -> XMLGenT m (HSX.XML m)
showUserOption user = let un (ExternalUserID x) = BS.toString x in
    <option value=(show $ userid user) title=(un $ safehead "showUserOption" $ externaluserids user)><% fullname user %> <% email user %></option>
 
statsPageView :: Int -> Int -> [User] -> HSP XML
statsPageView nusers ndocuments users =
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
       <select name="user">
        <% map showUserOption users %>
       </select>
       <input class="secbutton" type="submit" value="Become"/>
      </form>
     </body>
    </html>


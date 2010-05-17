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
kontrakcja = "Skriva på"

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
  <div style="align: center">
   <img src="/theme/images/skriva.jpg" height="40"/>
   <br/> 
   <form action="/issue" method="post" enctype="multipart/form-data">
    <input type="file" name="doc"/>
    <input type="submit" name="Upload"/>
   </form>
  </div>

welcomeBody ctx@(Context Nothing hostpart) = 
  <div style="align: center">
   <img src="/theme/images/skriva.jpg" height="40"/>
   <br/>
   <p><% maybeSignInLink ctx "Sign in here" "/" %></p>
  </div>

errorReport :: (XMLGenerator m) => Context -> Request -> XMLGenT m (HSX.XML m)
errorReport (Context maybeuser _) request = 
  <div>
   <p>An error occured. It's not you, it's us. We will take care about the problem
   as soon as possible. Meanwhile please <a href="/">go to the very beginning</a>.</p>
   <hr/>
   <p>Information useful to developers:</p>
   <% case maybeuser of
           Just user -> <p>Logged in as: <% user %></p>
           Nothing -> <p>Not logged in</p>
   %>
   <p><% request %></p>
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

renderFromBody :: (MonadIO m, EmbedAsChild (HSPT' IO) xml) => Context -> String -> xml -> m Response
renderFromBody ctx title = webHSP . pageFromBody ctx title

pageFromBody :: (EmbedAsChild (HSPT' IO) xml) => Context -> String -> xml -> HSP XML
pageFromBody ctx@(Context maybeuser hostpart) title body =
    withMetaData html4Strict $
    <html>
     <head>
      <title><% title %></title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <link rel="stylesheet" type="text/css" href="/theme/style.css" media="screen" />
     </head>
     <body>
       <div id="header">
       <div class="grunge"></div>
       <div class="peel">
          <%
              case maybeuser of
                   Just (User{fullname}) -> <span><a href="/account"><% BSC.toString fullname %></a> | <a href="/logout">Logout</a></span>
                   Nothing ->  <a class="rpxnow" onclick="return false;"
                                  href=("https://kontrakcja.rpxnow.com/openid/v2/signin?token_url=" ++ 
                                        urlEncode (hostpart ++ "/"))>Sign in here</a> 


          %>
       </div>
       <div class="topnavi">
        <ul>
         <li class="current_page_item"><a href="/" title=kontrakcja><% kontrakcja %></a></li>
        </ul>
       </div>
      </div>


      <div class="side1">
       <div class="sbar_section">
         <ul>
      	  <li class="cat-item cat-item-1">
           <% maybeSignInLink ctx "Begin" "/" %>
          </li>
      	  <li class="cat-item cat-item-2">
           <% maybeSignInLink ctx "Issue" "/issue" %>
          </li>
      	  <li class="cat-item cat-item-3">
           <% maybeSignInLink ctx "Sign" "/sign" %>
          </li>
         </ul>
       </div>
      </div>

      <div class="wrap">
       <div class="innercont_main">
        {- 
        <div class="post">
         <div class="posttop">
         </div>
        </div>
        -} 
        <% body %>
       </div>
      </div>
           
      <div class="footer">
        <div class="finalfooter">
          <a href="/aboutus">About Us</a> | 
          <a href="/privacypolicy">Privacy Policy</a> | 
          <a href="/blog" title="Lukas Duczko">Blog</a>
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
     </body>
    </html>

{-
          <div class="date">14<div>Feb</div></div>
          <h1 class="posttitle">Kontrakcja</h1>
          <div class="storycontent">
           <p>
              Welcome! You need to enter you Google login and password to use our
              services. Please do it here:
           </p>

           <form action="/login" method="post" enctype="multipart/form-data;charset=UTF-8" accept-charset="UTF-8">
            <p>
             <label for="email">Google mail</label>
             <input type="text" name="email" id="email" tabindex="1" accesskey="M" />
            </p>
            <p>
             <label for="password">Password</label>
             <input type="password" name="password" id="password" tabindex="2" accesskey="P" />
            </p>
            <p>
             <input type="submit" tabindex="3" accesskey="L" value="Login" />
            </p>
           </form>
          </div>
-}

{-
      <div class="footer">
        <div class="finalfooter">Theme : <a href="http://www.dezinerfolio.com/2007/10/10/just-another-wodpress-theme" title="sIMPRESS v2 theme">sIMPRESS v2</a> by <a href="http://dezinerfolio.com" title="Dezinerfolio">Dezinerfolio</a></div>
      </div>
-}

seeOtherXML :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
seeOtherXML url = <a href=url alt="303 see other"><% url %></a>


statsPageView :: Int -> Int -> HSP XML
statsPageView nusers ndocuments =
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
     </body>
    </html>

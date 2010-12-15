{-# LANGUAGE FlexibleContexts, FlexibleInstances, IncoherentInstances,
             MultiParamTypeClasses, NamedFieldPuns, CPP #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -Wall#-}
module AppView( TopMenu(..)
              , kontrakcja
              , htmlHeadBodyWrapIO
              , poweredBySkrivaPaPara
              , loginBox
               -- , pageWelcome
              , pageErrorReport
              , renderFromBody
              , pageForgotPassword
              , pageForgotPasswordConfirm
              , signupPageView
              , SignupForm(..)
              , databaseContents
              , pageAllUsersTable
              , signupConfirmPageView
              , pageLogin
              , pageStats
              ) where 

import HSP hiding (Request)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.SimpleHTTP
import qualified HSX.XMLGenerator as HSX (XML)
import qualified HSX.XMLGenerator
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSC
import User
import qualified Data.Map as Map
import Misc
import KontraLink

poweredBySkrivaPaPara :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
poweredBySkrivaPaPara hostpart = 
    <p>
     <small>Med vänliga hälsningar<%"\n"%><br/><a href=hostpart>SkrivaPå</a></small>
    </p>

htmlHeadBodyWrap :: (XMLGenerator m,EmbedAsChild m a {- ,EmbedAsChild m b -})
                 => a
                 -> XMLGenT m (HSX.XMLGenerator.XML m) --b
                 -> XMLGenT m (HSX.XMLGenerator.XML m)
htmlHeadBodyWrap title content =     
    <html>
     <head>
      <meta http-equiv="content-type" content="text/html; charset=utf-8"/>
      <title><% title %></title>
     </head>
     <body>
      <% content %>
     </body>
    </html>
htmlHeadBodyWrapIO :: (EmbedAsChild (HSPT' IO) a) => a -> XMLGenT (HSPT' IO) (HSX.XMLGenerator.XML (HSPT' IO))   -> IO BSC.ByteString
htmlHeadBodyWrapIO title content = do
  let xml = htmlHeadBodyWrap title content 
  renderHSPToByteString xml

data TopMenu = TopNew | TopDocument | TopAccount | TopNone | TopEmpty
             deriving (Eq,Ord)

kontrakcja :: [Char]
kontrakcja = "SkrivaPå" 

loginBox :: (EmbedAsAttr m (Attr [Char] [Char]),EmbedAsAttr m (Attr [Char] KontraLink)) => XMLGenT m (HSX.XMLGenerator.XML m)
loginBox =
   <div>
    <div id="login">
     <form action=LinkLogin method="post">
      <table>
	<tr>
          <td>E-mail:</td> 
          <td><input type="email" name="email" autocomplete="off" class="noflash"/></td> 
        </tr>
	<tr> 
          <td>Lösenord:</td> 
          <td><input type="password" name="password" autocomplete="off"/></td> 
        </tr>
    <tr>
          <td>
          </td>
          
          <td style="display: none">
            <input type="checkbox" id="rememberme" name="rememberme"/>
            <label for="rememberme">Kom ihåg mig</label>
          </td>
    </tr>
	<tr> 
          <td><input class="button" id="loginbtn" type="submit" name="login" value="Logga in"/></td>
          <td>
           <a href=LinkForgotPassword> Glömt lösenord</a>
          </td>
	</tr>
      </table>
    </form>
    </div>
   </div>


pageErrorReport :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
            => Context 
            -> Request 
            -> XMLGenT m (HSX.XML m)
pageErrorReport (Context {ctxmaybeuser}) request = 
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


-- * Main Implementation

renderFromBody :: (EmbedAsChild (HSPT' IO) xml) 
               => Context 
               -> TopMenu 
               -> String 
               -> xml 
               -> Kontra Response
renderFromBody ctx topmenu title xml = do
                                        res <- webHSP $ pageFromBody ctx topmenu title xml
                                        clearFlashMsgs
                                        return res

topnavi :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
        => Bool 
        -> String 
        -> KontraLink 
        -> XMLGenT m (HSX.XML m)
topnavi active title link = 
    <a href=link class=(if active then "active" else "")><% title %></a>

partialScripts :: (EmbedAsAttr m (Attr [Char] [Char])) => [XMLGenT m (HSX.XMLGenerator.XML m)]    
partialScripts =
      [ <script src="//ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.js"/>
      -- we loaded the min version but at some point google stopped serving this one
      -- , <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"/>
      , <script src="//ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/jquery-ui.min.js"/>
      , <script src="/js/jquery.tools.min.js"/> 
      {- Local versions of the same, but locally
      , <script src="/js/jquery-1.4.2.min.js"/>
      , <script src="/js/jquery-ui-1.8.custom.min.js"/>
      -}
      , <script src="/js/jquery.MultiFile.js"/>
      , <script src="/tiny_mce/jquery.tinymce.js"></script>
      , <script src="/js/global.js"/>
      ]
partialStyles :: (EmbedAsAttr m (Attr [Char] [Char])) => [XMLGenT m (HSX.XMLGenerator.XML m)]
partialStyles = 
      [ <link rel="stylesheet" type="text/css" href="/theme/style.css" media="screen" />,
        <link rel="stylesheet" type="text/css" href="/theme/calendar.css" media="screen" />,
        <link rel="stylesheet" type="text/css" 
            href="//ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/themes/ui-lightness/jquery-ui.css" 
            -- href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/themes/flick/jquery-ui.css"
            -- href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/themes/redmond/jquery-ui.css"
            -- href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/themes/start/jquery-ui.css"
            media="screen" />
      ]

pageFromBody :: (EmbedAsChild (HSPT' IO) xml) 
             => Context 
             -> TopMenu 
             -> String 
             -> xml 
             -> HSP XML
pageFromBody (Context {ctxmaybeuser,ctxflashmessages,ctxproduction}) 
             topMenu title body =
    withMetaData html4Strict $
    <html>
     <head>
      <title><% title %><% if ctxproduction then "" else " (devel)" %></title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <% partialStyles %>
      <% partialScripts {- we would like to move this to the end of html, to load faster -} %>
     </head>
     <body class=(if ctxproduction then "" else "development")>
     <div id="headerWide"/>
     <div id="mainContainer960">
      <div class="flashmsgbox">
               <% ctxflashmessages %>
      </div>
   
      <div id="headerContainer">
      <a href="/">
        <% if ctxproduction
               then <img id="logosmall" src="/theme/images/logosmall.png" alt="Liten logga"/>
               else <span id="logosmall">Staging area</span>
         %>
       </a> 
  

           <% case ctxmaybeuser of
             Just user-> 
                 <span id="userMenu"><% userfullname user%> | <a href=LinkAccount>Konto</a> | <a href=LinkLogout>Logga ut</a></span>
             Nothing -> 
               <div id="loginContainer"> {- new id -}
	         <form action="/login" method="post"> 
		    <div> 
			<input type="email" infotext="Användarnamn" name="email" autocomplete="off" /> 
			<input type="password" name="password" infotext="password" autocomplete="off" /><br /> 
		        <a href=LinkForgotPassword> Glömt lösenord</a> 
                        <input type="submit" value="Logga in" name="login" class="button" /> 
		    </div> 
	         </form> 
               </div> 
           %>
      
         <div id="nav">
          <% case ctxmaybeuser of 
               Just _ ->
                 <ul>
                   <li><% topnavi (topMenu== TopNew) "Skapa avtal" LinkMain %></li>
                   <li><% topnavi (topMenu== TopDocument) "Arkiv" LinkIssue %></li>
                 </ul>
               _ -> <span/>
           %>
         </div>
     
        <div class="clearboth"/>
      </div>
      <div id="mainContainer">
          <% body %>
      </div>
      </div>
      
      
      <div id="footerContainer">
       <div id="footerContainer2">
        <ul>
          <li class="footerCategoryHeader"> 
           SkrivaPå
          </li>
          <li>
           <a href="/why.html">Fördelar</a>
          </li>
          <li>
           <a href="/features.html">Funktioner</a>
          </li>
          <li>
           <a href="/pricing.html">Prisplan</a>
          </li>
        </ul>

        <ul>
          <li class="footerCategoryHeader"> 
           Trygghet och villkor
          </li>
          <li>
           <a href="/security.html">Säkerhet</a>
          </li>
          <li>
           <a href="/legal.html">Juridik</a>
          </li>
          <li>
           <a href="/privacypolicy.html">Sekretesspolicy</a>
          </li>
          <li>
           <a href="/termsofuse.html">Allmäna Villkor</a>
          </li>
        </ul>

        <ul>  
          <li class="footerCategoryHeader"> 
           Om oss
          </li>
          <li>
           <a href="/contact.html">Kontakt</a>
          </li>
        </ul>
		
		<div id="copy"><% cdata "&copy;" %> 2010 SkrivaPå</div> 
       </div>
      </div>
      <% if ctxproduction
         then [
               <script type="text/javascript">
                 var _gaq = _gaq || [];
                 _gaq.push(['_setAccount', 'UA-6387711-9']);
                 _gaq.push(['_trackPageview']);

                 (function() {
                    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
                    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
                    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
                  })();

                 (function() {
                    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
                    ga.src = "https://eu1.snoobi.com/snoop.php?tili=skrivapa_se";
                    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
                  })();
               </script>
               ]
         else []
       %>
     </body>
    </html>

pageStats :: Int -> Int -> BS.ByteString -> HSP XML
pageStats nusers ndocuments df =
    developmentWrapper "Stats page" []
     <div>
      <h1>Stats page</h1>
      <table>
       <tr><td>Users</td><td><% show nusers %></td></tr>
       <tr><td>Documents</td><td><% show ndocuments %></td></tr>
      </table>
      <br/>
      <form method="post" action="/createuser">
       Create user:<br/> 
       <table>
        <tr>
         <td>Full name:</td>
         <td><input type="text" name="fullname"/></td>
        </tr>
        <tr>
         <td>Email address:</td>
         <td><input type="email" name="email"/></td>
        </tr>
       </table><br/>
       <input type="submit" value="Create user"/>
      </form>
      <br/>
      Disk status:<br/>
      <pre><% df %></pre>
     </div>

signupConfirmPageView :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) =>  XMLGenT m (HSX.XML m)
signupConfirmPageView  =  <div>Ditt konto har skapats! Vi har skickat ett mail med dina användaruppgifter till din inkorg.</div>

data SignupForm = SignupForm {
    signupFullname :: BS.ByteString,
    signupEmail :: BS.ByteString,
    signupPassword :: BS.ByteString,
    signupPassword2 :: BS.ByteString }
    
instance FromData SignupForm where
    fromData = do
        fullname <- lookBS "fullname"
        email <- lookBS "email"
        password <- lookBS "password"
        password2 <- lookBS "password2"
        return $ SignupForm {
            signupFullname = concatChunks fullname,
            signupEmail = concatChunks email,
            signupPassword = concatChunks password,
            signupPassword2 = concatChunks password2
        }
        
signupPageView :: ( XMLGenerator m , EmbedAsAttr m (Attr String KontraLink), EmbedAsAttr m (Attr String BS.ByteString))=> Maybe SignupForm -> XMLGenT m (HSX.XML m)
signupPageView form =
    <div class="centerdivnarrow">
        <form action=LinkSignup method="post">
            <table>
                <tr>
                    <td>Namn:</td>
                    <td><input name="fullname" value=(maybe BS.empty signupFullname form) /></td>
                </tr>
                <tr>
                    <td>E-mail:</td>
                    <td><input type="email" name="email" value=(maybe BS.empty signupEmail form) /></td>
                </tr>
                <tr>
                    <td>Lösenord:</td>
                    <td><input name="password" type="password" /></td>
                </tr>
                <tr>
                    <td>Upprepa nytt lösenord:</td>
                    <td><input name="password2" type="password" /></td>
                </tr>
            </table>
            <input type="submit" value="Skapa konto" />
        </form>
    </div>

pageForgotPassword :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) 
               => XMLGenT m (HSX.XML m)
pageForgotPassword =
  <div class="centerdivnarrow">
    <form action=LinkForgotPassword method="post">
      <table>
        <tr>
          <td>E-mail</td>
          <td><input name="email" type="email"/></td>
        </tr>
      </table>
      <input type="submit" value="Skicka nytt lösenord" />
    </form>
  </div>

pageForgotPasswordConfirm :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) =>  XMLGenT m (HSX.XML m)
pageForgotPasswordConfirm  =
  <div class="centerdivnarrow">
    <p>Ett nytt lösenord har skickats till din e-post. Du kan nu logga in med dina nya uppgifter.</p>
    <% loginBox %>
  </div>

pageLogin :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) => XMLGenT m (HSX.XML m)
pageLogin = 
  <div class="centerdivnarrow">

   <p class="headline">Logga in SkrivaPå!</p> 

   <% loginBox %>

  </div>


developmentWrapper :: (EmbedAsChild (HSPT' IO) body) 
                   => String 
                   -> [FlashMessage]
                   -> body 
                   -> HSP XML
developmentWrapper title ctxflashmessages body =
    withMetaData html4Strict $
    <html>
     <head>
      <title><% title %></title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <% partialStyles %>
      <% partialScripts %>
     </head>
     <body>
        <div class="flashmsgbox">
          <% ctxflashmessages %>
        </div> 
  
      <% body %>
     </body>
    </html>

databaseContents :: [String] -> HSP XML
databaseContents contents = developmentWrapper "All database files" []
  <div>
     <h1>All database files</h1>
     <ul>
      <% map showOneFile contents %>
     </ul>
  </div>
  where showOneFile file = <li><a href=file><% file %></a></li>


userInfo :: (HSX.XMLGenerator.XMLGen m, Show t) =>  (User, t) -> XMLGenT m (HSX.XMLGenerator.XML m)
userInfo (user,docs) = <tr><td><% userfullname user %></td><td><% unEmail $ useremail $ userinfo user %></td><td><% show docs %></td></tr>

pageAllUsersTable :: [(User,Int)] -> HSP XML
pageAllUsersTable users =
    developmentWrapper "Alla SkrivaPå anwender" []
     <div>
      <h1>Alla SkrivaPå anwender</h1>
      <table>
       <thead><td>Username</td><td>Email</td><td>Docs</td></thead>
       <% map userInfo users %>
      </table>
     </div>



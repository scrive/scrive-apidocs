{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module User 
    ( module UserState
    , withUser
    , maybeSignInLink
    , maybeSignInLink2
    , userLogin
    , Context(..)
    , isSuperUser
    , Kontra(..)
    , rpxSignInLink
    )
    where

import UserState
import Session
import Happstack.Server
import Happstack.Server.HStringTemplate (webST)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.Server.SimpleHTTP
import qualified HSX.XMLGenerator as HSX (XML)
import HSP
import Network.HTTP (urlEncode)
import qualified Data.ByteString.UTF8 as BS
import qualified Network.Curl as Curl
import Control.Monad
import Data.Maybe
import Control.Monad.Reader (ask)
import Control.Monad.Trans(liftIO, MonadIO,lift)
import Data.Object
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Object.Json as Json
import Happstack.Data.IxSet ((@=),getOne)
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import System.Log.Logger
import Control.Monad.State
import MinutesTime
import KontraLink

seeOtherXML :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
seeOtherXML url = <a href=url alt="303 see other"><% url %></a>

data Context = Context 
    { ctxmaybeuser     :: Maybe User
    , ctxhostpart      :: String
    , ctxflashmessages :: [FlashMessage]
    , ctxtime          :: MinutesTime
    }

type Kontra a = ServerPartT (StateT Context IO) a

{-
 We need pleasant user experience here. Therefore:

 When logged in link should just do action.

 When not logged in:
 1. Link should trigger overlay (or login screen)
 2. After login it should redirect to the page it was clicked at first

 Questions:
 - what to do about settings page?
 - what to do if login failed?
 - how to handle POST data?

 Thoughts:
 - settings page can serve as user-create too
 - if user just created, show the settings
 - otherwise just redirect back to proper place

-}
withUser :: (MonadIO m) => Maybe User -> ServerPartT m Response -> ServerPartT m Response
withUser (Just user) action = action
withUser Nothing action = msum
                          [ methodOnly GET >> provideRPXNowLink
                             -- FIXME: you seem to be doing method POST while 
                             -- not logged in and requesting login at the same time
                             -- this is not going to work right now, stop it!
                            ]

userLogin :: (MonadIO m) => ServerPartT m (Maybe User)
userLogin = do
  maybeuser <- withMSessionDataSP2 $ \maybeuserid -> do 
    case maybeuserid of
      Just (sid,userid) -> do
                       -- prolong session
                       startSession sid
                       query $ GetUserByUserID userid
      Nothing -> return Nothing
  case maybeuser of
    Just user -> return maybeuser
    Nothing -> userLogin1

userLogin1 :: (MonadIO m) => ServerPartT m (Maybe User)
userLogin1 = do
    maybetoken <- getDataFn (look "token") 
#if MIN_VERSION_happstack_server(0,5,1)
    case maybetoken of
      Left _ -> return Nothing
      Right token -> do
#else
    case maybetoken of
      Nothing -> return Nothing
      Just token -> do
#endif

              let req = "https://rpxnow.com/api/v2/auth_info" ++ 
                        "?apiKey=03bbfc36d54e523b2602af0f95aa173fb96caed9" ++
                        -- a348dd93f1d78ae11c443574d73d974299007c00" ++
                        "&token=" ++ token
              
              {-
                FIXME: Get the certificate of that server and import it
                to private repository.
               -}
        
              (_code,rpxdata) <- liftIO $ 
                                 Curl.curlGetString_ req [ Curl.CurlFollowLocation True
                                                         , Curl.CurlSSLVerifyPeer False
                                                         ] 

              liftIO $ noticeM rootLoggerName $ "RPXNow: " ++ BS.toString rpxdata

              let unJsonString (Json.JsonString x) = x
              let maybeProfileMapping = do
                    json <- Json.decode rpxdata
                    jsonMapping <- fromMapping json 
                    lookupMapping (BS.fromString "profile") jsonMapping
              let maybeVerifiedEmail = do
                    profileMapping <- maybeProfileMapping
                    verifiedEmail <- lookupScalar (BS.fromString "verifiedEmail") profileMapping
                    return (unJsonString verifiedEmail)
              let maybeNameFormatted = do
                    profileMapping <- maybeProfileMapping
                    nameMapping <- lookupMapping (BS.fromString "name") profileMapping
                    nameFormatted <- lookupScalar (BS.fromString "formatted") nameMapping
                    return (unJsonString nameFormatted)
              let verifiedEmail = maybe BS.empty id maybeVerifiedEmail
              let nameFormatted = maybe BS.empty id maybeNameFormatted

              when (verifiedEmail==BS.empty) $
                   error "SkrivaPa requires verifiedEmail in your OpenID login data, we cannot work without one"

              maybeuser <- query $ GetUserByEmail (Email verifiedEmail)
    
              user <- case maybeuser of
                        Just user -> do
                          liftIO $ noticeM rootLoggerName $ "User: " ++ BS.toString nameFormatted ++ " <" ++ 
                                 BS.toString verifiedEmail ++ "> logged in"
                          return user
                        Nothing -> do
                          user <- update $ AddUser nameFormatted verifiedEmail BS.empty Nothing
                          liftIO $ noticeM rootLoggerName $ "User: " ++ BS.toString nameFormatted ++ " <" ++ 
                                 BS.toString verifiedEmail ++ "> logged in (new)"
                          return user
              sessionid <- update $ NewSession (userid user)
              startSession sessionid
              rq <- askRq
              let link = rqUri rq
              response <- webHSP (seeOtherXML link)
              finishWith (redirect 303 link response)
              return (Just user)

provideRPXNowLink :: (MonadIO m) => ServerPartT m Response
provideRPXNowLink = do -- FIXME it was guarded by method GET but it didn't help
    rq <- askRq
    let Just host = getHeader "host" rq
    {-
      FIXME: watch out for protocol here
    -}
    let serverurl = "http://" ++ BS.toString host ++ rqUri rq
    let url = "https://kontrakcja.rpxnow.com/openid/v2/signin?token_url=" ++ urlEncode serverurl
    v <- webHSP $ seeOtherXML url
    seeOther url (v)

rpxSignInLink (Context {ctxhostpart}) title url = do
    -- FIXME: this is very simple url handling....
    let fullurl = ctxhostpart ++ url
    <a class="rpxnow"href=("https://kontrakcja.rpxnow.com/openid/v2/signin?token_url=" ++ urlEncode fullurl)><% title %></a> 

{-
maybeSignInLink
  :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] [Char])) =>
     Context -> XMLGenT m (HSX.XML m) -> String -> XMLGenT m (HSX.XML m)
-}

maybeSignInLink :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) => Context -> String 
                -> KontraLink -> XMLGenT m (HSX.XML m)
{-
maybeSignInLink (Context {ctxmaybeuser = Nothing, ctxhostpart}) title url = do
    -- FIXME: this is very simple url handling....
    let fullurl = ctxhostpart ++ url
    <a class="rpxnow" onclick="return false;"
       href=("https://skrivapa.rpxnow.com/openid/v2/signin?token_url=" ++ urlEncode fullurl)><% title %></a> 
-}
maybeSignInLink (Context {}) title url = do
    <a href=url><% title %></a> 

{-
maybeSignInLink2 (Context {ctxmaybeuser = Nothing, ctxhostpart}) title url class1 = do
    -- FIXME: this is very simple url handling....
    let fullurl = ctxhostpart ++ url
    <a class=("rpxnow " ++ class1) onclick="return false;" class=class1
       href=("https://skrivapa.rpxnow.com/openid/v2/signin?token_url=" ++ urlEncode fullurl)><% title %></a> 
-}
maybeSignInLink2 :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] KontraLink)) => Context -> String 
                 -> KontraLink -> String -> XMLGenT m (HSX.XML m)
maybeSignInLink2 (Context {}) title url class1 = do
    <a href=url class=class1><% title %></a> 


{- 
userLogin2 :: (MonadIO m) => ServerPartT m (Maybe User)
userLogin2 = do
  let identifier = BS.fromString "auser"
  let formatted =  BS.fromString "Emica Zaboo"
  maybeuser <- query $ GetUserByExternalUserID 
               (ExternalUserID identifier)
    
  user <- case maybeuser of
            Just user -> return user
            Nothing -> do
               user <- update $ AddUser (ExternalUserID identifier) (formatted) (BS.fromString "")
               return user
  sessionid <- update $ NewSession (userid user)
  startSession sessionid

  return (Just user)
-}

isSuperUser (Just user) 
    | useremail user == Email (BS.fromString "gracjanpolak@gmail.com") = True
    | useremail user == Email (BS.fromString "gracjan@skrivapa.se") = True
    | useremail user == Email (BS.fromString "lukas@skrivapa.se") = True
    | useremail user == Email (BS.fromString "lukas.duczko@gmail.com") = True
    | useremail user == Email (BS.fromString "ericwnormand@gmail.com") = True
    | otherwise = False
isSuperUser Nothing = False

{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module User 
    ( module UserControl
    , module UserState
    , module UserView
    , withUser
    , maybeSignInLink
    , userLogin
    , Context(..)
    )
    where

import UserControl
import UserState
import UserView
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

seeOtherXML :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
seeOtherXML url = <a href=url alt="303 see other"><% url %></a>

data Context = Context (Maybe User) String



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
withUser :: Maybe User -> ServerPartT IO Response -> ServerPartT IO Response
withUser (Just user) action = action
withUser Nothing action = msum
                          [ methodOnly GET >> provideRPXNowLink
                             -- FIXME: you seem to be doing method POST while 
                             -- not logged in and requesting login at the same time
                             -- this is not going to work right now, stop it!
                            ]

userLogin :: ServerPartT IO (Maybe User)
userLogin = do
  maybeuser <- withMSessionDataSP2 $ \maybeuserid -> do 
    case maybeuserid of
      Just (sid,userid) -> do
                       -- prolong session
                       startSession sid
                       query $ FindUserByUserID userid
      Nothing -> return Nothing
  case maybeuser of
    Just user -> return maybeuser
    Nothing -> userLogin1

userLogin1 :: ServerPartT IO (Maybe User)
userLogin1 = do
    maybetoken <- getDataFn (look "token") 
    case maybetoken of
      Nothing -> return Nothing
      Just token -> do

              let req = "https://rpxnow.com/api/v2/auth_info" ++ 
                        "?apiKey=03bbfc36d54e523b2602af0f95aa173fb96caed9" ++
                        "&token=" ++ token
              
              {-
                FIXME: Get the certificate of that server and import it
                to private repository.
               -}
        
              (_code,rpxdata) <- liftIO $ 
                                 Curl.curlGetString req [Curl.CurlFollowLocation True
                                                        ,Curl.CurlSSLVerifyPeer False] 

                    {-
                      FIXME: Take care of the situation when not all data is available
                     -}
    
              let Just json = Json.decode (BSL.fromString rpxdata) 
                  Just jsonMapping = fromMapping json 
                  Just profileMapping = lookupMapping (BS.fromString "profile") jsonMapping
                  -- Json.JsonString verifiedEmail = maybe id (Json.JsonString (BS.fromString "")) $ lookupScalar (BS.fromString "verifiedEmail") profileMapping
                  Just (Json.JsonString identifier) = lookupScalar (BS.fromString "identifier") profileMapping
                  Just nameMapping = lookupMapping (BS.fromString "name") profileMapping
                  Just (Json.JsonString formatted) = lookupScalar (BS.fromString "formatted") nameMapping
        
              maybeuser <- query $ FindUserByExternalUserID (ExternalUserID identifier)
    
              user <- case maybeuser of
                        Just user -> return user
                        Nothing -> do
                          user <- update $ AddUser (ExternalUserID identifier) (formatted) (BS.fromString "")
                          return user
              sessionid <- update $ NewSession (userid user)
              startSession sessionid
              return (Just user)

provideRPXNowLink :: ServerPartT IO Response
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

maybeSignInLink
  :: (XMLGenerator m,EmbedAsAttr m (Attr [Char] [Char])) =>
     Context -> String -> String -> XMLGenT m (HSX.XML m)
maybeSignInLink (Context Nothing base) title url = do
    -- FIXME: this is very simple url handling....
    let fullurl = base ++ url
    <a class="rpxnow" onclick="return false;"
       href=("https://kontrakcja.rpxnow.com/openid/v2/signin?token_url=" ++ urlEncode fullurl)><% title %></a> 
maybeSignInLink (Context (Just _) base) title url = do
    let fullurl = base ++ url
    <a href=fullurl><% title %></a> 


userLogin2 :: ServerPartT IO (Maybe User)
userLogin2 = do
  let identifier = BS.fromString "auser"
  let formatted =  BS.fromString "Emica Zaboo"
  maybeuser <- query $ FindUserByExternalUserID 
               (ExternalUserID identifier)
    
  user <- case maybeuser of
            Just user -> return user
            Nothing -> do
               user <- update $ AddUser (ExternalUserID identifier) (formatted) (BS.fromString "")
               return user
  sessionid <- update $ NewSession (userid user)
  startSession sessionid
  return (Just user)













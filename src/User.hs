{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module User 
    ( module UserState
    , withUser
    , maybeSignInLink
    , maybeSignInLink2
    , Context(..)
    , isSuperUser
    , Kontra(..)
    , rpxSignInLink
    , createRememberMeCookie
    , sessionLength
    , RememberMe(..)
    , readRememberMeCookie
    , admins
    )
    where

import System.Time
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
import Control.Monad
import Data.Maybe
import Codec.Utils (Octet)
import Control.Monad.Reader (ask)
import Control.Monad.Trans(liftIO, MonadIO,lift)
import Data.Object
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.Object.Json as Json
import qualified Codec.Binary.Base64 as Base64
import Data.HMAC (hmac_sha1)
import Happstack.Data.IxSet ((@=),getOne)
import Happstack.Server hiding (simpleHTTP)
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import System.Log.Logger
import Control.Monad.State
import MinutesTime
import KontraLink
import DocState
import System.IO.Unsafe
import qualified Data.Set as Set
import Control.Concurrent.MVar
import System.Process
import Data.Word

data Context = Context 
    { ctxmaybeuser           :: Maybe User
    , ctxhostpart            :: String
    , ctxflashmessages       :: [FlashMessage]
    , ctxtime                :: MinutesTime
    , ctxnormalizeddocuments :: MVar (Set.Set FileID)
    , ctxipnumber            :: Word32
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


userLoginx :: (MonadIO m) => ServerPartT m (Maybe User)
userLoginx = do
  muserid <- currentUserID
  msid <- currentSessionId 
  muser <- case (muserid,msid) of
            (Just userid,Just msid) -> do
                                        startSession msid
                                        query $ GetUserByUserID userid
            _ -> return Nothing     
  case muser of
    Just user -> return muser
    Nothing -> userLogin1x

userLogin1x :: (MonadIO m) => ServerPartT m (Maybe User)
userLogin1x = do
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
                        "?apiKey=" ++ 
                        -- "03bbfc36d54e523b2602af0f95aa173fb96caed9" ++
                        "a348dd93f1d78ae11c443574d73d974299007c00" ++
                        "&token=" ++ token
              
              let curlproc = CreateProcess { std_out = CreatePipe
                                           , std_err = CreatePipe
                                           , std_in = Inherit
                                           , cwd = Nothing
                                           , cmdspec = RawCommand "curl" [ "-q", "-k", req ]
                                           , close_fds = True
                                           , env = Nothing
                                           }
              (_, Just outhandle, Just errhandle, curlProcHandle) <- liftIO $ createProcess curlproc
              errcontent <- liftIO $ BS.hGetContents errhandle
              rpxdata <- liftIO $ BS.hGetContents outhandle
              
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

              when (verifiedEmail==BS.empty) $ do
                liftIO $ BS.putStrLn rpxdata
                error "SkrivaPa requires verifiedEmail in your OpenID login data, we cannot work without one"
                         

              maybeuser <- query $ GetUserByEmail (Email verifiedEmail)
    
              case maybeuser of
                Just user -> do
                  liftIO $ noticeM rootLoggerName $ "User: " ++ BS.toString nameFormatted ++ " <" ++ 
                         BS.toString verifiedEmail ++ "> logged in"
                  sessionid <- update $ NewSession $ emptySessionDataWithUserID (userid user)
                  startSession sessionid
                  rq <- askRq
                  let link = rqUri rq
                  response <- webHSP (seeOtherXML link)
                  finishWith (redirect 303 link response)
                  return (Just user)

                Nothing -> do
                  let link = "/login"
                  response <- webHSP $ seeOtherXML link
                  finishWith (redirect 303 link response)
                  return Nothing



provideRPXNowLink :: (MonadIO m) => ServerPartT m Response
provideRPXNowLink = do -- FIXME it was guarded by method GET but it didn't help
    rq <- askRq
    let Just host = getHeader "host" rq
    {-
      FIXME: watch out for protocol here
    -}
    let serverurl = "http://" ++ BS.toString host ++ rqUri rq
    let url = "https://skrivapa.rpxnow.com/openid/v2/signin?token_url=" ++ urlEncode serverurl
    v <- webHSP $ seeOtherXML url
    seeOther url (v)

rpxSignInLink (Context {ctxhostpart}) title url = do
    -- FIXME: this is very simple url handling....
    let fullurl = ctxhostpart ++ url
    <a class="rpxnow"href=("https://skrivapa.rpxnow.com/openid/v2/signin?token_url=" ++ urlEncode fullurl)><% title %></a> 

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

admins = map (Email . BS.fromString)
         [ "gracjanpolak@gmail.com"
         , "gracjan@skrivapa.se"
         , "lukas@skrivapa.se"
         , "lukas.duczko@gmail.com"
         , "ericwnormand@gmail.com"
         , "oskar@skrivapa.se"
         ]

isSuperUser (Just user@User{useremail}) = useremail `elem` admins 
isSuperUser _ = False

-- Identity, LongTerm, Nonce, Signature
data RememberMe = RememberMe UserID Bool Integer [Octet] [Octet] deriving Show
instance Binary.Binary RememberMe where
    put (RememberMe identity longTerm expiry nonce signature) =
        Binary.put (identity, longTerm, expiry, nonce, signature)
    get = do
        identity <- Binary.get
        longTerm <- Binary.get
        expiry <- Binary.get
        nonce <- Binary.get
        signature <- Binary.get
        return (RememberMe identity longTerm expiry nonce signature)

instance Binary.Binary UserID where
    put UserID {unUserID=id} = Binary.put id
    get = do
        id <- Binary.get
        return $ UserID {unUserID=id}

-- TODO make this something else - perhaps configurable - changing it will log everyone out
rememberMeSecret :: [Octet]
rememberMeSecret = [123,89,54,78,12,82,13,234] -- an arbitrary, but secret array of bytes

makeNonce :: IO [Octet]
makeNonce = randomOctets 20

-- Create a remember me cookie
createRememberMeCookie :: UserID -> Bool -> IO String
createRememberMeCookie identity longTerm = do
    nonce <- makeNonce
    (TOD now _) <- getClockTime
    let expiry = now + toInteger (sessionLength longTerm)
    return $ Base64.encode $ BSL.unpack $ Binary.encode $ RememberMe identity longTerm expiry nonce (rememberMeSignature identity nonce expiry)
    
sessionLength :: Bool -> Int
sessionLength True = 60 * 60 * 24 * 14 -- 2 weeks
sessionLength False = 60 * 60 -- 1 hour

rememberMeSignature :: UserID -> [Octet] -> Integer -> [Octet]
rememberMeSignature identity nonce expiry =
    hmac_sha1 rememberMeSecret (BSL.unpack $ Binary.encode (identity, nonce, expiry))

readRememberMeCookie :: String -> Maybe RememberMe
readRememberMeCookie cookieString = do
    let isCookieVerified = unsafePerformIO $ verifyRememberMeCookie cookie
    if isCookieVerified
      then Just cookie
      else Nothing
    where
        Just cookieBytes = Base64.decode cookieString
        cookie = (Binary.decode $ BSL.pack cookieBytes) :: RememberMe

verifyRememberMeCookie :: RememberMe -> IO Bool
verifyRememberMeCookie (RememberMe identity _ expiry nonce signature)
    | (rememberMeSignature identity nonce expiry) == signature = do
        (TOD now _) <- liftIO $ getClockTime
        return $ now < expiry
    | otherwise = return False

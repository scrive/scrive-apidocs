{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module User 
    ( module UserState
    , withUser
    , Context(..)
    , isSuperUser
    , Kontra(..)
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

instance MonadState s m => MonadState s (ServerPartT m) where
    get = lift get
    put = lift . put

withUser :: Kontra Response -> Kontra Response
withUser action = do
  ctx <- get
  case ctxmaybeuser ctx of
    Just user -> action
    Nothing -> do
      let link = ctxhostpart ctx ++ "/login"
      response <- webHSP (seeOtherXML link)
      seeOther link response



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

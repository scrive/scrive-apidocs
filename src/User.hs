{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module User 
    ( module UserState
    , Context(..)
    , isSuperUser
    , Kontra(..)
    , admins
    , initialUsers
    , clearFlashMsgs
    , addFlashMsgText 
    , addFlashMsgHtml
    , logUserToContext
    )
    where

import System.Time
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State
import Codec.Utils (Octet)
import Control.Concurrent.MVar
import Data.HMAC (hmac_sha1)
import Data.Word
import DocState
import HSP
import Happstack.Server
import MinutesTime
import Session
import System.IO.Unsafe
import System.Log.Logger
import System.Process
import UserState
import qualified Codec.Binary.Base64 as Base64
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as Set
import qualified HSX.XMLGenerator as HSX (XML)
import Misc (renderXMLAsBSHTML)
import qualified Data.Map as Map

data Context = Context 
    { ctxmaybeuser           :: Maybe User
    , ctxhostpart            :: String
    , ctxflashmessages       :: [FlashMessage]
    , ctxtime                :: MinutesTime
    , ctxnormalizeddocuments :: MVar (Map.Map FileID JpegPages)
    , ctxipnumber            :: Word32
    }

type Kontra a = ServerPartT (StateT Context IO) a

instance MonadState s m => MonadState s (ServerPartT m) where
    get = lift get
    put = lift . put


admins = map (Email . BS.fromString)
         [ "gracjanpolak@gmail.com"
         , "lukas@skrivapa.se"
         , "oskar@skrivapa.se"
         ]

initialUsers = map (Email . BS.fromString)
         [ "gracjanpolak@gmail.com"
         , "lukas@skrivapa.se"
         , "ericwnormand@gmail.com"
         , "oskar@skrivapa.se"
         , "kbaldyga@gmail.com"
         ]

isSuperUser (Just user@User{useremail}) = useremail `elem` admins 
isSuperUser _ = False

addFlashMsgText :: BS.ByteString -> Kontra ()
addFlashMsgText msg = do
                       ctx@Context{ ctxflashmessages = flashmessages} <- get
                       put $ ctx{ ctxflashmessages =  (FlashMessage msg) : flashmessages}
                       
clearFlashMsgs:: Kontra ()                       
clearFlashMsgs = do
                       ctx <- get
                       put $ ctx{ ctxflashmessages = []}
  
addFlashMsgHtml :: HSP.HSP HSP.XML 
                -> Kontra ()
addFlashMsgHtml msg = do
                       ctx@Context{ ctxflashmessages = flashmessages} <- get
                       msg' <- liftM renderXMLAsBSHTML (liftIO $ evalHSP Nothing msg)
                       put $ ctx {ctxflashmessages = (FlashMessage msg') :  flashmessages} 

logUserToContext user =  do
                          ctx<- get
                          put$ ctx { ctxmaybeuser =  user}    

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
    , addFlashMsgHtmlFromTemplate
    , logUserToContext
    , onlySuperUser
    , changePasswordLink
    , activateLink
    , queryOrFail
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
import HSP hiding (Request)
import Happstack.Server
import MinutesTime
import Session
import System.IO.Unsafe
import System.Log.Logger
import System.Process
import Happstack.State (query,QueryEvent)
import UserState
import qualified Codec.Binary.Base64 as Base64
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as Set
import Misc (renderXMLAsBSHTML)
import qualified Data.Map as Map
import HSP.XML
import Data.Object.Json as Json
import Data.Object as Json
import qualified Network.AWS.Authentication as AWS
import Templates.Templates  (KontrakcjaTemplates)
import Mails.MailsConfig
import KontraLink
import qualified TrustWeaver as TW

instance Monad m => IsAttrValue m DocumentID where
    toAttrValue = toAttrValue . show

instance (EmbedAsChild m BS.ByteString) => (EmbedAsChild m Email) where
    asChild = asChild . unEmail

instance Monad m => IsAttrValue m Email where
    toAttrValue = toAttrValue . unEmail

instance Monad m => IsAttrValue m UserID where
    toAttrValue = toAttrValue . show

instance (XMLGenerator m) => (EmbedAsChild m User) where
  asChild user = <% BS.toString (userfullname user) ++ " <" ++ 
                 BS.toString (unEmail $ useremail $ userinfo user) ++ ">"  %>	    

instance (XMLGenerator m) => (EmbedAsChild m HeaderPair) where
  asChild (HeaderPair name value) = 
     <% <p> 
         <% BS.toString name ++ ": " ++ show value  %> 
        </p> 
      %>	
instance (EmbedAsChild m HSP.XML.XML,EmbedAsAttr m (Attr [Char] [Char])) => (EmbedAsChild m FlashMessage) where
  asChild (FlashMessage msg) = 
     asChild (<span class="flashmessage"> <% cdata $ BS.toString msg %> </span>)
     
instance (XMLGenerator m) => (EmbedAsChild m (Json.Object BS.ByteString Json.JsonScalar)) where
  asChild (Json.Mapping xs) = 
    let s (k, v) = <li><% BS.toString k %>: <% v %></li> 
    in
     <% <ul> <% map s xs %>  </ul> %>
  asChild (Json.Sequence xs) =
   let y v = <li><% v %></li>
   in
     <% <ol> <% map y xs %> </ol> %>
  asChild (Json.Scalar val) =  <% <span> <% val %> </span> %>

instance (XMLGenerator m) => (EmbedAsChild m Json.JsonScalar) where
  asChild (JsonString x) = <% BS.toString x %>	
  asChild (JsonNumber x) = <% show x %>	
  asChild (JsonBoolean x) = <% show x %>
  asChild JsonNull = <% "null" %>  
    
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
       <% show (concatMap BS.toString (BSL.toChunks value)) %>
       
    
data Context = Context 
    { ctxmaybeuser           :: Maybe User
    , ctxhostpart            :: String
    , ctxflashmessages       :: [FlashMessage]
    , ctxtime                :: MinutesTime
    , ctxnormalizeddocuments :: MVar (Map.Map FileID JpegPages)
    , ctxipnumber            :: Word32
    , ctxs3action            :: AWS.S3Action
    , ctxproduction          :: Bool
    , ctxtemplates           :: KontrakcjaTemplates 
    , ctxmailsconfig         :: MailsConfig  
    , ctxtwconf              :: TW.TrustWeaverConf
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

isSuperUser (Just user) = (useremail $ userinfo user) `elem` admins 
isSuperUser _ = False

onlySuperUser::Kontra a -> Kontra a
onlySuperUser a =
                 do
                 ctx <- get
                 if (isSuperUser $ ctxmaybeuser  ctx)
                  then a
                  else mzero
                     

addFlashMsgText :: String -> Kontra ()
addFlashMsgText msg = do
                       ctx@Context{ ctxflashmessages = flashmessages} <- get
                       put $ ctx{ ctxflashmessages =  (FlashMessage $ BS.fromString msg) : flashmessages}
                       
clearFlashMsgs:: Kontra ()                       
clearFlashMsgs = do
                       ctx <- get
                       put $ ctx{ ctxflashmessages = []}
                       
addFlashMsgHtmlFromTemplate msg = do
                       ctx@Context{ ctxflashmessages = flashmessages} <- get
                       put $ ctx {ctxflashmessages = (FlashMessage $ BS.fromString msg) :  flashmessages} 

logUserToContext user =  do
  ctx <- get
  put $ ctx { ctxmaybeuser =  user}    

changePasswordLink::UserID -> IO KontraLink
changePasswordLink uid =  do
                           session <- createLongTermSession (uid)
                           return (LinkPasswordChange (getSessionId session) (getSessionMagicHash session))     
activateLink::UserID -> IO KontraLink
activateLink uid =  do
                           session <- createLongTermSession (uid)
                           return (LinkActivateAccount (getSessionId session) (getSessionMagicHash session))                            

queryOrFail :: (QueryEvent ev (Maybe res)) => ev -> Kontra res
queryOrFail q = do
  mres <- query q
  case mres of
    Just res -> return res
    Nothing -> mzero
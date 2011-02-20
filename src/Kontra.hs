{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Kontra
    ( module User.UserState
    , Context(..)
    , isSuperUser
    , Kontra
    , admins
    , initialUsers
    , clearFlashMsgs
    , addELegTransaction
    , addFlashMsgText 
    , logUserToContext
    , onlySuperUser
    , unloggedActionLink
    , queryOrFail
    )
    where

import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar
import Data.Word
import Doc.DocState
import HSP hiding (Request)
import Happstack.Server
import MinutesTime
import Session
import Happstack.State (query,QueryEvent)
import User.UserState
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import HSP.XML
import qualified Network.AWS.Authentication as AWS
import Templates.Templates  (KontrakcjaTemplates)
import Mails.MailsConfig()
import KontraLink
import qualified TrustWeaver as TW
import ELegitimation.ELeg
import Mails.SendMail
import qualified MemCache

-- these HSP type class instances help us express our data in xml

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
    , ctxmailer              :: Mailer 
    , ctxtwconf              :: TW.TrustWeaverConf
    , ctxelegtransactions    :: [ELegTransaction]
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString
    }

type Kontra a = ServerPartT (StateT Context IO) a

instance MonadState s m => MonadState s (ServerPartT m) where
    get = lift get
    put = lift . put

{- |
   A list of admin emails.
-}
admins :: [Email]
admins = map (Email . BS.fromString)
         [ "gracjanpolak@gmail.com"
         , "lukas@skrivapa.se"
         , "oskar@skrivapa.se"
         , "viktor@skrivapa.se"
         ]

{- |
   A list of default user emails.  These should start out as the users
   in a brand new system.
-}
initialUsers :: [Email]
initialUsers = map (Email . BS.fromString)
         [ "gracjanpolak@gmail.com"
         , "lukas@skrivapa.se"
         , "ericwnormand@gmail.com"
         , "oskar@skrivapa.se"
         , "kbaldyga@gmail.com"
         , "viktor@skrivapa.se"
         ]

{- |
   Whether the user is an administrator.
-}
isSuperUser :: Maybe User -> Bool
isSuperUser (Just user) = (useremail $ userinfo user) `elem` admins 
isSuperUser _ = False

{- |
   Will mzero if not logged in as a super user.
-}
onlySuperUser::Kontra a -> Kontra a
onlySuperUser a =
                 do
                 ctx <- get
                 if (isSuperUser $ ctxmaybeuser  ctx)
                  then a
                  else mzero
          
{- |
   Adds an Eleg Transaction to the context.
-}           
addELegTransaction :: ELegTransaction -> Kontra ()
addELegTransaction tr = do
  ctx@Context { ctxelegtransactions = currenttrans } <- get
  put $ ctx { ctxelegtransactions = (tr : currenttrans) }

{- |
   Adds a flash message to the context.
-}  
addFlashMsgText :: String -> Kontra ()
addFlashMsgText msg = do
                       ctx@Context{ ctxflashmessages = flashmessages} <- get
                       put $ ctx{ ctxflashmessages =  (FlashMessage $ BS.fromString msg) : flashmessages}
     
{- |
   Clears all the flash messages from the context.
-}                  
clearFlashMsgs:: Kontra ()                       
clearFlashMsgs = do
                       ctx <- get
                       put $ ctx{ ctxflashmessages = []}

{- |
   Sticks the logged in user onto the context
-}
logUserToContext :: Maybe User -> Kontra ()
logUserToContext user =  do
  ctx <- get
  put $ ctx { ctxmaybeuser =  user}    

{- |
   A link that can be used by un-logged in users, to do things like activate their login.
-}
unloggedActionLink::User -> IO KontraLink
unloggedActionLink user =  do
                           session <- createLongTermSession (userid user)
                           return $ LinkUnloggedUserAction (getSessionId session) 
                                                           (getSessionMagicHash session) 
                                                           (BS.toString $ unEmail $ useremail $ userinfo user)
                                                           (BS.toString $ userfullname user)
                                                       
{- |
   Perform a query (like with query) but if it returns Nothing, mzero; otherwise, return fromJust
 -}
queryOrFail :: (QueryEvent ev (Maybe res)) => ev -> Kontra res
queryOrFail q = do
  mres <- query q
  case mres of
    Just res -> return res
    Nothing -> mzero


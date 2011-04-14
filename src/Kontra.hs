{-# LANGUAGE CPP, IncoherentInstances #-}

module Kontra
    ( module User.UserState
    , Context(..)
    , isSuperUser
    , Kontra
    , KontraModal
    , admins
    , initialUsers
    , clearFlashMsgs
    , addELegTransaction
    , addFlashMsg
    , addModal
    , logUserToContext
    , onlySuperUser
    , newPasswordReminderLink
    , newViralInvitationSentLink
    , newAccountCreatedLink
    , newAccountCreatedBySigningLink
    , scheduleEmailSendout
    , queryOrFail
    , returnJustOrMZero
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
import Misc
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
import Mails.SendMail
import KontraLink
import ActionSchedulerState
import qualified TrustWeaver as TW
import ELegitimation.ELeg
import Mails.SendMail
import qualified MemCache


#if MIN_VERSION_happstack_server(0,5,1)
rqInputs2 rq = do
    let inputs = rqInputsQuery rq 
    body <- readMVar (rqInputsBody rq)
    return $ inputs ++ body
#else
rqInputs2 rq = return (rqInputs rq)
#endif
  
data Context = Context 
    { ctxmaybeuser           :: Maybe User
    , ctxhostpart            :: String
    , ctxflashmessages       :: [FlashMessage]
    , ctxtime                :: MinutesTime
    , ctxnormalizeddocuments :: MVar (Map.Map FileID JpegPages)
    , ctxipnumber            :: Word32
    , ctxdocstore            :: FilePath
    , ctxs3action            :: AWS.S3Action
    , ctxgscmd               :: String
    , ctxproduction          :: Bool
    , ctxtemplates           :: KontrakcjaTemplates 
    , ctxesenforcer          :: MVar ()
    , ctxtwconf              :: TW.TrustWeaverConf
    , ctxelegtransactions    :: [ELegTransaction]
    , ctxfilecache           :: MemCache.MemCache FileID BS.ByteString
    , ctxxtoken              :: MagicHash
    }

type Kontra a = ServerPartT (StateT Context IO) a
type KontraModal = ReaderT KontrakcjaTemplates IO String

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
         , "andrzej@skrivapa.se"
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
onlySuperUser :: Kontra a -> Kontra a
onlySuperUser a = do
    ctx <- get
    if (isSuperUser $ ctxmaybeuser ctx)
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
addFlashMsg :: FlashMessage -> Kontra ()
addFlashMsg flash =
    modify (\ctx@Context{ ctxflashmessages = flashmessages } ->
        ctx { ctxflashmessages = flash : flashmessages })

{- |
   Clears all the flash messages from the context.
-}                  
clearFlashMsgs:: Kontra ()                       
clearFlashMsgs = modify (\ctx -> ctx { ctxflashmessages = [] })


{- |
   Adds a modal from string
-}  
addModal :: KontraModal ->  Kontra ()
addModal flash = do
  ctx <- get  
  fm <- liftIO $ runReaderT flash (ctxtemplates ctx)  
  put $  ctx { ctxflashmessages = (FlashMessage (Modal,fm)):(ctxflashmessages ctx) }


{- |
   Sticks the logged in user onto the context
-}
logUserToContext :: Maybe User -> Kontra ()
logUserToContext user =  do
  ctx <- get
  put $ ctx { ctxmaybeuser = user}    

newPasswordReminderLink :: MonadIO m => User -> m KontraLink
newPasswordReminderLink user = do
    action <- liftIO $ newPasswordReminder user
    return $ LinkPasswordReminder (actionID action)
                                  (prToken $ actionType action)

newViralInvitationSentLink :: MonadIO m => Email -> UserID -> m KontraLink
newViralInvitationSentLink email inviterid = do
    action <- liftIO $ newViralInvitationSent email inviterid
    return $ LinkViralInvitationSent (actionID action)
                                     (visToken $ actionType action)

newAccountCreatedLink :: MonadIO m => User -> m KontraLink
newAccountCreatedLink user = do
    action <- liftIO $ newAccountCreated user
    return $ LinkAccountCreated (actionID action)
                                (acToken $ actionType action)
                                (BS.toString . unEmail . useremail $ userinfo user)

newAccountCreatedBySigningLink :: MonadIO m => User -> (DocumentID, SignatoryLinkID) -> m (KontraLink, KontraLink)
newAccountCreatedBySigningLink user doclinkdata = do
    action <- liftIO $ newAccountCreatedBySigning user doclinkdata
    let aid = actionID action
        token = acbsToken $ actionType action
    return $ (LinkAccountCreatedBySigning aid token, LinkAccountRemoval aid token)

-- | Schedule mail for send out and awake scheduler
scheduleEmailSendout :: MonadIO m => MVar () -> Mail -> m ()
scheduleEmailSendout enforcer mail = do
    liftIO $ do
        newEmailSendoutAction mail
        tryPutMVar enforcer ()
    return ()

{- |
   Perform a query (like with query) but if it returns Nothing, mzero; otherwise, return fromJust
 -}
queryOrFail :: (QueryEvent ev (Maybe res)) => ev -> Kontra res
queryOrFail q = do
  mres <- query q
  returnJustOrMZero mres

-- | if it's not a just, mzero. Otherwise, return the value
returnJustOrMZero :: Maybe a -> Kontra a
returnJustOrMZero = maybe mzero return

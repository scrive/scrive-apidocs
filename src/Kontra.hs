module Kontra
    ( module User.UserState
    , module User.Password
    , Context(..)
    , Kontrakcja
    , KontraMonad(..)
    , isSuperUser
    , Kontra(runKontra)
    , KontraModal
    , initialUsers
    , clearFlashMsgs
    , addELegTransaction
    , addFlashMsg
    , addModal
    , addModalT
    , logUserToContext
    , onlySuperUser
    , newPasswordReminderLink
    , newViralInvitationSentLink
    , newAccountCreatedLink
    , newAccountCreatedBySigningLink
    , scheduleEmailSendout
    , queryOrFail
    , queryOrFailIfLeft
    , returnJustOrMZero
    , returnRightOrMZero
    , param
    , currentService
    , currentServiceID
    , HasService(..)
    )
    where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent.MVar
import Data.Word
import Doc.DocState
import Happstack.Server
import MinutesTime
import Misc
import Happstack.State (query,QueryEvent)
import User.UserState
import User.Password hiding (Password, NoPassword)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as Map
import qualified Network.AWS.Authentication as AWS
import Templates.Templates  (KontrakcjaTemplates, TemplatesMonad(..))
import Mails.MailsConfig()
import KontraLink
import ActionSchedulerState
import qualified TrustWeaver as TW
import ELegitimation.ELeg
import Mails.SendMail
import qualified MemCache
import API.Service.ServiceState
import FlashMessage
import Company.CompanyState
import Util.HasSomeUserInfo

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
    , ctxcompany             :: Maybe Company
    , ctxservice             :: Maybe Service
    , ctxlocation            :: String
    , ctxadminaccounts       :: [Email]
    }

-- | This is for grouping things together so we won't need to
-- write all that each time we write function type signature
class (Functor m, HasRqData m, KontraMonad m, Monad m, MonadIO m, ServerMonad m) => Kontrakcja m

class KontraMonad m where
    getContext    :: m Context
    modifyContext :: (Context -> Context) -> m ()

newtype Kontra a = Kontra { runKontra :: ServerPartT (StateT Context IO) a }
    deriving (Applicative, FilterMonad Response, Functor, HasRqData, Monad, MonadIO, MonadPlus, MonadState Context, ServerMonad, WebMonad Response)

instance Kontrakcja Kontra

instance KontraMonad Kontra where
    getContext    = get
    modifyContext = modify

instance TemplatesMonad Kontra where
        getTemplates = do
            ctx <- get
            return (ctxtemplates ctx)

type KontraModal = ReaderT KontrakcjaTemplates IO String

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
         , "mariusz@skrivapa.se"
         , "heidi@skrivapa.se"
         ]

{- |
   Whether the user is an administrator.
-}
isSuperUser :: [Email] -> Maybe User -> Bool
isSuperUser admins (Just user) = (useremail $ userinfo user) `elem` admins
isSuperUser _ _ = False

{- |
   Will mzero if not logged in as a super user.
-}
onlySuperUser :: Kontra a -> Kontra a
onlySuperUser a = do
    ctx <- get
    if isSuperUser (ctxadminaccounts ctx) (ctxmaybeuser ctx)
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
  put $  ctx { ctxflashmessages = (toFlashMsg Modal fm):(ctxflashmessages ctx) }

{- |
   Adds modal as flash template, used for semantics sake
-}
addModalT :: FlashMessage -> Kontra ()
addModalT = addFlashMsg

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
                                (BS.toString $ getEmail user)

newAccountCreatedBySigningLink :: MonadIO m => User -> (DocumentID, SignatoryLinkID) -> m (ActionID, MagicHash)
newAccountCreatedBySigningLink user doclinkdata = do
    action <- liftIO $ newAccountCreatedBySigning user doclinkdata
    let aid = actionID action
        token = acbsToken $ actionType action
    return $ (aid, token)

-- | Schedule mail for send out and awake scheduler
scheduleEmailSendout :: MonadIO m => MVar () -> Mail -> m ()
scheduleEmailSendout enforcer mail = do
    _ <- liftIO $ do
        newEmailSendoutAction mail
        tryPutMVar enforcer ()
    return ()

{- |
   Perform a query (like with query) but if it returns Nothing, mzero; otherwise, return fromJust
 -}
queryOrFail :: (MonadPlus m,Monad m, MonadIO m) => (QueryEvent ev (Maybe res)) => ev -> m res
queryOrFail q = do
  mres <- query q
  returnJustOrMZero mres

queryOrFailIfLeft :: (MonadPlus m,Monad m, MonadIO m) => (QueryEvent ev (Either a res)) => ev -> m res
queryOrFailIfLeft q = do
  mres <- query q
  returnRightOrMZero mres

-- | if it's not a just, mzero. Otherwise, return the value
returnJustOrMZero :: (MonadPlus m,Monad m) => Maybe a -> m a
returnJustOrMZero = maybe mzero return

returnRightOrMZero :: (MonadPlus m, Monad m) => Either a b -> m b
returnRightOrMZero (Left _) = mzero
returnRightOrMZero (Right res) = return res

-- | Checks if request contains a param , else mzero
param :: String -> Kontra Response -> Kontra Response
param p action = (getDataFnM $ look p) >> action

-- | Current service id

currentService :: Context -> (Maybe Service)
currentService  ctx = ctxservice ctx

currentServiceID :: Context -> Maybe ServiceID
currentServiceID  ctx = serviceid <$> currentService ctx


class HasService a where
    getService:: a -> Maybe ServiceID

instance HasService Document where
    getService = documentservice



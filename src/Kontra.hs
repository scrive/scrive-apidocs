module Kontra
    ( Context(..)
    , Kontrakcja
    , KontraMonad(..)
    , isSuperUser
    , Kontra(runKontra)
    , clearFlashMsgs
    , addELegTransaction
    , logUserToContext
    , onlySuperUser
    , onlyBackdoorOpen
    , newPasswordReminderLink
    , newViralInvitationSentLink
    , newAccountCreatedLink
    , newAccountCreatedBySigningLink
    , scheduleEmailSendout
    , queryOrFail
    , param
    , currentService
    , currentServiceID
    , HasService(..)
    )
    where

import API.Service.Model
import ActionSchedulerState
import Context
import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.Reader
import Control.Monad.State
import DB.Classes
import DB.Types
import Doc.DocState
import ELegitimation.ELeg
import Happstack.Server
import Happstack.State (query, QueryEvent)
import KontraLink
import KontraMonad
import Mails.SendMail
import Misc
import Templates.Templates
import User.Model
import Util.HasSomeUserInfo
import qualified AppLogger as Log
import qualified Data.ByteString.UTF8 as BS
import Util.MonadUtils

newtype Kontra a = Kontra { runKontra :: ServerPartT (StateT Context IO) a }
    deriving (Applicative, FilterMonad Response, Functor, HasRqData, Monad, MonadIO, MonadPlus, ServerMonad, WebMonad Response)

instance Kontrakcja Kontra

instance DBMonad Kontra where
  getConnection = ctxdbconn <$> getContext
  handleDBError e = do
    Log.error $ show e
    mzero

instance KontraMonad Kontra where
    getContext    = Kontra get
    modifyContext = Kontra . modify

instance TemplatesMonad Kontra where
    getTemplates = ctxtemplates <$> getContext
    getLocalTemplates locale = do
      Context{ctxglobaltemplates} <- getContext
      return $ localizedVersion locale ctxglobaltemplates
{- |
   Whether the user is an administrator.
-}
isSuperUser :: [Email] -> Maybe User -> Bool
isSuperUser admins (Just user) = (useremail $ userinfo user) `elem` admins
isSuperUser _ _ = False

{- |
   Will mzero if not logged in as a super user.
-}
onlySuperUser :: Kontrakcja m => m a -> m a
onlySuperUser a = do
    ctx <- getContext
    if isSuperUser (ctxadminaccounts ctx) (ctxmaybeuser ctx)
        then a
        else mzero

{- |
    Will mzero if the testing backdoor isn't open.
-}
onlyBackdoorOpen :: Kontrakcja m => m a -> m a
onlyBackdoorOpen a = do
  ctx <- getContext
  if ctxbackdooropen ctx
    then a
    else mzero

{- |
   Adds an Eleg Transaction to the context.
-}
addELegTransaction :: Kontrakcja m => ELegTransaction -> m ()
addELegTransaction tr = do
    modifyContext $ \ctx -> ctx {ctxelegtransactions = tr : ctxelegtransactions ctx }

{- |
   Clears all the flash messages from the context.
-}
clearFlashMsgs:: KontraMonad m => m ()
clearFlashMsgs = modifyContext $ \ctx -> ctx { ctxflashmessages = [] }

{- |
   Sticks the logged in user onto the context
-}
logUserToContext :: Kontrakcja m => Maybe User -> m ()
logUserToContext user =
    modifyContext $ \ctx -> ctx { ctxmaybeuser = user}

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
                                     (BS.toString $ unEmail email)

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
  guardJust mres

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



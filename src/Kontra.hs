module Kontra 
    ( module KontraError
    , module KontraMonad
    , module Context
    , Kontra(..)
    , Kontra'(..)
    , runKontra'
    , clearFlashMsgs
    , addELegTransaction
    , logUserToContext
    , logPadUserToContext
    , isAdmin
    , isSales
    , onlyAdmin
    , onlySalesOrAdmin
    , onlyBackdoorOpen
    , newPasswordReminderLink
    , newViralInvitationSentLink
    , newAccountCreatedLink
    , runDBOrFail
    , queryOrFail
    , getAsString
    , getDataFnM
    , currentService
    , currentServiceID
    , HasService(..)
    , disableLocalSwitch -- Turn off the flag switcher on top bar
    , switchLocale       -- set language
    )
    where

import API.Service.Model
import ActionSchedulerState
import Context
import Control.Applicative
import Control.Logic
import Control.Monad.Error (MonadError, ErrorT, runErrorT)
import Control.Monad.Reader
import Control.Monad.State
import Crypto.RNG (CryptoRNG, getCryptoRNGState)
import DB.Classes
import ELegitimation.ELegTransaction
import Doc.DocStateData
import Happstack.Server
import KontraError (internalError, respond404)
import KontraLink
import KontraMonad
import Mails.MailsConfig
import Templates.Templates
import User.Model
import Util.HasSomeUserInfo
import qualified Log
import Util.MonadUtils
import Misc

-- | Kontra' is 'MonadPlus', but it should only be used on toplevel
-- for interfacing with static routing.
newtype Kontra' a = Kontra' { unKontra' :: ServerPartT (ErrorT KontraError (StateT Context IO)) a }
    deriving (MonadPlus, Applicative, FilterMonad Response, Functor, HasRqData, Monad, MonadIO, MonadError KontraError, ServerMonad, WebMonad Response)

-- | Kontra is a traditional Happstack handler monad except that it's
-- not MonadZero.
--
-- Since we use static routing, there is no need for mzero inside a
-- handler.  Instead we signal errors explicitly through
-- 'KontraError'.
newtype Kontra a = Kontra { unKontra :: Kontra' a }
    deriving (Applicative, FilterMonad Response, Functor, HasRqData, Monad, MonadIO, MonadError KontraError, ServerMonad, WebMonad Response, CryptoRNG, KontraMonad, DBMonad, TemplatesMonad)

instance Kontrakcja Kontra
instance Kontrakcja Kontra'

instance CryptoRNG Kontra' where
  getCryptoRNGState = Kontra' $ gets (rngstate . ctxdbenv)

instance DBMonad Kontra' where
  getDBEnv = ctxdbenv <$> getContext
  handleDBError e = do
    Log.error $ show e
    internalError

instance KontraMonad Kontra' where
    getContext    = Kontra' get
    modifyContext = Kontra' . modify

instance TemplatesMonad Kontra' where
    getTemplates = ctxtemplates <$> getContext
    getLocalTemplates locale = do
      Context{ctxglobaltemplates} <- getContext
      return $ localizedVersion locale ctxglobaltemplates

-- | In case of KontraError, remove all Happstack filters
runKontra' :: Context -> Kontra' a -> ServerPartT IO (Either KontraError a)
runKontra' ctx = mapServerPartT (\s -> trans `fmap` evalStateT (runErrorT s) ctx) . unKontra'
  where trans (Left e)                   = Just (Right (Left e), filterFun id)
        trans (Right (Just (Left r,f)))  = Just (Left r,f)
        trans (Right (Just (Right a,f))) = Just (Right (Right a),f)
        trans (Right Nothing)            = Nothing

{- Logged in user is admin-}
isAdmin :: Context -> Bool
isAdmin ctx = (useremail <$> userinfo <$> ctxmaybeuser ctx) `melem` (ctxadminaccounts ctx) && scriveService ctx

{- Logged in user is sales -}
isSales :: Context -> Bool
isSales ctx = (useremail <$> userinfo <$> ctxmaybeuser ctx) `melem` (ctxsalesaccounts ctx) && scriveService ctx

{- |
   Will 404 if not logged in as an admin.
-}
onlyAdmin :: Kontrakcja m => m a -> m a
onlyAdmin m = ifM (isAdmin <$> getContext) m respond404

{- |
   Will 404 if not logged in as a sales admin.
-}
onlySalesOrAdmin :: Kontrakcja m => m a -> m a
onlySalesOrAdmin m = ifM ((isAdmin ||^ isSales) <$> getContext) m respond404

{- |
    Will 404 if the testing backdoor isn't open.
-}
onlyBackdoorOpen :: Kontrakcja m => m a -> m a
onlyBackdoorOpen a = do
  backdoorOpen <- isBackdoorOpen . ctxmailsconfig <$> getContext
  if backdoorOpen
    then a
    else respond404

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

logPadUserToContext :: Kontrakcja m => Maybe User -> m ()
logPadUserToContext user =
    modifyContext $ \ctx -> ctx { ctxmaybepaduser = user}
    
disableLocalSwitch :: Kontrakcja m => m ()
disableLocalSwitch =
    modifyContext $ \ctx -> ctx { ctxlocaleswitch = False}

switchLocale :: Kontrakcja m => Locale -> m ()
switchLocale locale =
     modifyContext $ \ctx -> ctx {
         ctxlocale     = locale,
         ctxtemplates  = localizedVersion locale (ctxglobaltemplates ctx)
     }

newPasswordReminderLink :: (MonadIO m, CryptoRNG m) => User -> m KontraLink
newPasswordReminderLink user = do
    action <- newPasswordReminder user
    return $ LinkPasswordReminder (actionID action)
                                  (prToken $ actionType action)

newViralInvitationSentLink :: (MonadIO m, CryptoRNG m) => Email -> UserID -> m KontraLink
newViralInvitationSentLink email inviterid = do
    action <- newViralInvitationSent email inviterid
    return $ LinkViralInvitationSent (actionID action)
                                     (visToken $ actionType action)
                                     (unEmail email)

newAccountCreatedLink :: (MonadIO m, CryptoRNG m) => User -> m KontraLink
newAccountCreatedLink user = do
    action <- newAccountCreated user
    return $ LinkAccountCreated (actionID action)
                                (acToken $ actionType action)
                                (getEmail user)

-- | Runs DB action and fails if it returned Nothing
runDBOrFail :: (DBMonad m, MonadError KontraError m) => DB (Maybe r) -> m r
runDBOrFail f = runDB f >>= guardJust

{- |
   Perform a query (like with query) but if it returns Nothing, fail; otherwise, return fromJust
 -}
queryOrFail :: (MonadError KontraError m, DBMonad m, Monad m, MonadIO m, DBQuery ev (Maybe res)) => ev -> m res
queryOrFail q = do
  mres <- runDBQuery q
  guardJust mres

-- data fetchers specific to Kontra

getAsString :: (HasRqData m, MonadIO m, ServerMonad m, MonadError KontraError m) => String -> m String
getAsString = getDataFnM . look

-- | Extract data from GET or POST request. Fail with 'internalError' if param
-- variable not present or when it cannot be read.
getDataFnM :: (HasRqData m, MonadIO m, ServerMonad m, MonadError KontraError m) => RqData a -> m a
getDataFnM fun = either (const internalError) return =<< getDataFn fun

-- | Current service id

currentService :: Context -> (Maybe Service)
currentService  ctx = ctxservice ctx

currentServiceID :: Context -> Maybe ServiceID
currentServiceID  ctx = serviceid <$> currentService ctx

scriveService :: (HasService a) => a -> Bool
scriveService a = Nothing ==  getService a

class HasService a where
    getService:: a -> Maybe ServiceID

instance HasService Document where
    getService = documentservice

instance HasService Context where
    getService ctx = serviceid <$> ctxservice ctx

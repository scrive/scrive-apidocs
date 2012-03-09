{-# LANGUAGE CPP #-}

module Kontra
    ( Context(..)
    , Kontrakcja
    , KontraMonad(..)
    , Kontra
    , runKontra
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
import Control.Monad.Reader
import Control.Monad.State
import Crypto.RNG (CryptoRNG, getCryptoRNGState)
import DB.Classes
import ELegitimation.ELegTransaction
import Doc.DocStateData
import Happstack.Server
import KontraLink
import KontraMonad
import Mails.MailsConfig
import Templates.Templates
import User.Model
import Util.HasSomeUserInfo
import qualified Log
import qualified Data.ByteString.UTF8 as BS
import Util.MonadUtils
import Misc

newtype Kontra a = Kontra { unKontra :: ServerPartT (StateT Context IO) a }
    deriving (Applicative, FilterMonad Response, Functor, HasRqData, Monad, MonadIO, MonadPlus, ServerMonad, WebMonad Response)

instance Kontrakcja Kontra

instance CryptoRNG Kontra where
  getCryptoRNGState = Kontra $ gets (rngstate . ctxdbenv)

instance DBMonad Kontra where
  getDBEnv = ctxdbenv <$> getContext
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

runKontra :: Context -> Kontra a -> ServerPartT IO a
runKontra ctx = mapServerPartT (\s -> evalStateT s ctx) . unKontra

{- Logged in user is admin-}
isAdmin :: Context -> Bool
isAdmin ctx = (useremail <$> userinfo <$> ctxmaybeuser ctx) `melem` (ctxadminaccounts ctx) && scriveService ctx

{- Logged in user is sales -}
isSales :: Context -> Bool
isSales ctx = (useremail <$> userinfo <$> ctxmaybeuser ctx) `melem` (ctxsalesaccounts ctx) && scriveService ctx

{- |
   Will mzero if not logged in as an admin.
-}
onlyAdmin :: Kontrakcja m => m a -> m a
onlyAdmin = guardTrueM $ isAdmin <$> getContext

{- |
   Will mzero if not logged in as a sales admin.
-}
onlySalesOrAdmin :: Kontrakcja m => m a -> m a
onlySalesOrAdmin = guardTrueM $ (isAdmin ||^ isSales) <$> getContext



{- |
    Will mzero if the testing backdoor isn't open.
-}
onlyBackdoorOpen :: Kontrakcja m => m a -> m a
onlyBackdoorOpen a = do
  backdoorOpen <- isBackdoorOpen . ctxmailsconfig <$> getContext
  if backdoorOpen
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
                                     (BS.toString $ unEmail email)

newAccountCreatedLink :: (MonadIO m, CryptoRNG m) => User -> m KontraLink
newAccountCreatedLink user = do
    action <- newAccountCreated user
    return $ LinkAccountCreated (actionID action)
                                (acToken $ actionType action)
                                (BS.toString $ getEmail user)

-- | Runs DB action and mzeroes if it returned Nothing
runDBOrFail :: (DBMonad m, MonadPlus m) => DB (Maybe r) -> m r
runDBOrFail f = runDB f >>= guardJust

{- |
   Perform a query (like with query) but if it returns Nothing, mzero; otherwise, return fromJust
 -}
queryOrFail :: (MonadPlus m, DBMonad m, Monad m, MonadIO m, DBQuery ev (Maybe res)) => ev -> m res
queryOrFail q = do
  mres <- runDBQuery q
  guardJust mres

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

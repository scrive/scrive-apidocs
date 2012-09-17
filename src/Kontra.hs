module Kontra
    ( module KontraError
    , module KontraMonad
    , module Context
    , Kontra(..)
    , KontraPlus(..)
    , runKontraPlus
    , clearFlashMsgs
    , addELegTransaction
    , logUserToContext
    , logPadUserToContext
    , isAdmin
    , isSales
    , onlyAdmin
    , onlySalesOrAdmin
    , onlyBackdoorOpen
    , getAsString
    , getDataFnM
    , switchLocale       -- set language
    )
    where

import Context
import Control.Applicative
import Control.Logic
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Util
import Crypto.RNG
import DB
import ELegitimation.ELegTransaction
import Happstack.Server
import KontraError
import KontraMonad
import Mails.MailsConfig
import OurServerPart
import Templates.Templates
import Templates.TemplatesLoader
import User.Model
import Utils.List
import Utils.Monad

type InnerKontraPlus = StateT Context (CryptoRNGT (DBT (OurServerPartT IO)))

-- | KontraPlus is 'MonadPlus', but it should only be used on toplevel
-- for interfacing with static routing.
newtype KontraPlus a = KontraPlus { unKontraPlus :: InnerKontraPlus a }
  deriving (MonadPlus, Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadBase IO, MonadDB, MonadIO, ServerMonad, WebMonad Response)

runKontraPlus :: Context -> KontraPlus a -> CryptoRNGT (DBT (OurServerPartT IO)) a
runKontraPlus ctx f = evalStateT (unKontraPlus f) ctx

instance Kontrakcja KontraPlus

instance MonadBaseControl IO KontraPlus where
  newtype StM KontraPlus a = StKontraPlus { unStKontraPlus :: StM InnerKontraPlus a }
  liftBaseWith = newtypeLiftBaseWith KontraPlus unKontraPlus StKontraPlus
  restoreM     = newtypeRestoreM KontraPlus unStKontraPlus
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance KontraMonad KontraPlus where
  getContext    = KontraPlus get
  modifyContext = KontraPlus . modify

instance TemplatesMonad KontraPlus where
  getTemplates = ctxtemplates <$> getContext
  getLocalTemplates locale = do
    Context{ctxglobaltemplates} <- getContext
    return $ localizedVersion locale ctxglobaltemplates

-- | Kontra is a traditional Happstack handler monad except that it's
-- not MonadZero and WebMonad.
--
-- Since we use static routing, there is no need for mzero inside a
-- handler. Instead we signal errors explicitly through 'KontraError'.
newtype Kontra a = Kontra { unKontra :: KontraPlus a }
  deriving (Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadBase IO, MonadIO, MonadDB, ServerMonad, KontraMonad, TemplatesMonad)

instance Kontrakcja Kontra

instance MonadBaseControl IO Kontra where
  newtype StM Kontra a = StKontra { unStKontra :: StM KontraPlus a }
  liftBaseWith = newtypeLiftBaseWith Kontra unKontra StKontra
  restoreM     = newtypeRestoreM Kontra unStKontra
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

{- Logged in user is admin-}
isAdmin :: Context -> Bool
isAdmin ctx = (useremail <$> userinfo <$> ctxmaybeuser ctx) `melem` (ctxadminaccounts ctx)

{- Logged in user is sales -}
isSales :: Context -> Bool
isSales ctx = (useremail <$> userinfo <$> ctxmaybeuser ctx) `melem` (ctxsalesaccounts ctx)

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
addELegTransaction _tr = return () -- FIXME
    --modifyContext $ \ctx -> ctx {ctxelegtransactions = tr : ctxelegtransactions ctx }

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
    
switchLocale :: Kontrakcja m => Locale -> m ()
switchLocale locale =
     modifyContext $ \ctx -> ctx {
         ctxlocale     = locale,
         ctxtemplates  = localizedVersion locale (ctxglobaltemplates ctx)
     }

-- data fetchers specific to Kontra

getAsString :: (HasRqData m, MonadBase IO m, MonadIO m, ServerMonad m) => String -> m String
getAsString = getDataFnM . look

-- | Extract data from GET or POST request. Fail with 'internalError' if param
-- variable not present or when it cannot be read.
getDataFnM :: (HasRqData m, MonadBase IO m, MonadIO m, ServerMonad m) => RqData a -> m a
getDataFnM fun = either (const internalError) return =<< getDataFn fun

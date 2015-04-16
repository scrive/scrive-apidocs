module Kontra
    ( module KontraError
    , module KontraMonad
    , module Context
    , Kontra(..)
    , runKontra
    , clearFlashMsgs
    , logUserToContext
    , logPadUserToContext
    , isAdmin
    , isSales
    , onlyAdmin
    , onlySalesOrAdmin
    , onlyBackdoorOpen
    , getDataFnM
    , switchLang       -- set language
    )
    where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Database.PostgreSQL.PQTypes.Class.Instances.Overlapping ()
import Happstack.Server
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.TemplatesLoader as TL

import Context
import Control.Logic
import Control.Monad.Trans.Control.Util
import Control.Monad.Trans.Instances ()
import Crypto.RNG
import DB
import GuardTime (GuardTimeConfMonad(..))
import Happstack.Server.Instances ()
import Happstack.Server.ReqHandler
import KontraError
import KontraMonad
import KontraPrelude
import MailContext (MailContextMonad(..))
import Mails.MailsConfig
import MinutesTime.Class
import Templates
import User.Model
import Utils.List
import qualified Amazon as AWS
import qualified Log

type InnerKontra = StateT Context (AWS.AmazonMonadT (CryptoRNGT (DBT (ReqHandlerT (Log.LogT IO)))))

-- | Kontra is a traditional Happstack handler monad except that it's
-- not WebMonad.
--
-- Note also that in Kontra we don't do backtracking, which is why it
-- is not an instance of MonadPlus.  Errors are signaled explicitly
-- through 'KontraError'.
newtype Kontra a = Kontra { unKontra :: InnerKontra a }
  deriving (Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadBase IO, MonadCatch, MonadDB, MonadIO, MonadMask, MonadThrow, ServerMonad, AWS.AmazonMonad)

instance Log.MonadLog Kontra where
  mixlogjs now title js = liftBase $ Log.mixlogjsIO now title js

runKontra :: Context -> Kontra a -> AWS.AmazonMonadT (CryptoRNGT (DBT (ReqHandlerT (Log.LogT IO)))) a
runKontra ctx f = evalStateT (unKontra f) ctx

instance MonadBaseControl IO Kontra where
  newtype StM Kontra a = StKontra { unStKontra :: StM InnerKontra a }
  liftBaseWith = newtypeLiftBaseWith Kontra unKontra StKontra
  restoreM     = newtypeRestoreM Kontra unStKontra
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTime Kontra where
  currentTime = ctxtime <$> getContext

instance KontraMonad Kontra where
  getContext    = Kontra get
  modifyContext = Kontra . modify

instance TemplatesMonad Kontra where
  getTemplates = ctxtemplates <$> getContext
  getTextTemplatesByLanguage langStr = do
     Context{ctxglobaltemplates} <- getContext
     return $ TL.localizedVersion langStr ctxglobaltemplates

instance GuardTimeConfMonad Kontra where
  getGuardTimeConf = ctxgtconf <$> getContext

instance MailContextMonad Kontra where
  getMailContext = contextToMailContext <$> getContext

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
onlyAdmin m = do
  admin <- isAdmin <$> getContext
  if admin
    then m
    else respond404

{- |
   Will 404 if not logged in as a sales admin.
-}
onlySalesOrAdmin :: Kontrakcja m => m a -> m a
onlySalesOrAdmin m = do
  admin <- (isAdmin ||^ isSales) <$> getContext
  if admin
    then m
    else respond404

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

switchLang :: Kontrakcja m => Lang -> m ()
switchLang lang =
     modifyContext $ \ctx -> ctx {
         ctxlang       = lang,
         ctxtemplates  = localizedVersion lang (ctxglobaltemplates ctx)
     }

-- | Extract data from GET or POST request. Fail with 'internalError' if param
-- variable not present or when it cannot be read.
getDataFnM :: (HasRqData m, MonadBase IO m, MonadIO m, ServerMonad m) => RqData a -> m a
getDataFnM fun = either (const internalError) return =<< getDataFn fun

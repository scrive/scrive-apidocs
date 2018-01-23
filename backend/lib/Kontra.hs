module Kontra
    ( module KontraError
    , module KontraMonad
    , module Context
    , Kontra(..)
    , runKontra
    , logUserToContext
    , logPadUserToContext
    , unsafeSessionTakeover
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
import Control.Monad.Trans.Control
import Crypto.RNG
import Happstack.Server
import Log
import Text.StringTemplates.Templates
import qualified Control.Monad.State.Strict as S
import qualified Text.StringTemplates.TemplatesLoader as TL

import Context
import Control.Monad.Trans.Instances ()
import DB
import GuardTime (GuardTimeConfMonad(..))
import Happstack.Server.Instances ()
import Happstack.Server.ReqHandler
import KontraError
import KontraMonad
import MailContext (MailContextMonad(..))
import Session.Cookies
import Session.Data
import Session.Model
import Templates
import User.Model
import Utils.HTTP
import qualified Amazon as AWS

type InnerKontra = S.StateT Context (DBT (AWS.AmazonMonadT (CryptoRNGT (LogT (ReqHandlerT IO)))))

-- | Kontra is a traditional Happstack handler monad except that it's
-- not WebMonad.
--
-- Note also that in Kontra we don't do backtracking, which is why it
-- is not an instance of MonadPlus.  Errors are signaled explicitly
-- through 'KontraError'.
newtype Kontra a = Kontra { unKontra :: InnerKontra a }
  deriving (Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadBase IO, MonadCatch, MonadDB, MonadIO, MonadMask, MonadThrow, ServerMonad, AWS.AmazonMonad, MonadLog)

runKontra :: Context -> Kontra a -> DBT (AWS.AmazonMonadT (CryptoRNGT (LogT (ReqHandlerT IO)))) a
runKontra ctx f = S.evalStateT (unKontra f) ctx

instance MonadBaseControl IO Kontra where
  type StM Kontra a = StM InnerKontra a
  liftBaseWith f = Kontra $ liftBaseWith $ \run -> f $ run . unKontra
  restoreM       = Kontra . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTime Kontra where
  currentTime = get ctxtime <$> getContext

instance KontraMonad Kontra where
  getContext    = Kontra S.get
  modifyContext = Kontra . S.modify

instance TemplatesMonad Kontra where
  getTemplates = get ctxtemplates <$> getContext
  getTextTemplatesByLanguage langStr = do
     globaltemplates <- get ctxglobaltemplates <$> getContext
     return $ TL.localizedVersion langStr globaltemplates

instance GuardTimeConfMonad Kontra where
  getGuardTimeConf = get ctxgtconf <$> getContext

instance MailContextMonad Kontra where
  getMailContext = contextToMailContext <$> getContext

{- Logged in user is admin-}
isAdmin :: Context -> Bool
isAdmin ctx = maybe False (`elem` get ctxadminaccounts ctx)
  (useremail <$> userinfo <$> get ctxmaybeuser ctx)

{- Logged in user is sales -}
isSales :: Context -> Bool
isSales ctx = maybe False (`elem` get ctxsalesaccounts ctx)
  (useremail <$> userinfo <$> get ctxmaybeuser ctx)

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
  admin <- (isAdmin || isSales) <$> getContext
  if admin
    then m
    else respond404

{- |
    Will 404 if the testing backdoor isn't open.
-}
onlyBackdoorOpen :: Kontrakcja m => m a -> m a
onlyBackdoorOpen a = do
  backdoorOpen <- get ctxismailbackdooropen <$> getContext
  if backdoorOpen
    then a
    else respond404

{- |
   Sticks the logged in user onto the context
-}
logUserToContext :: Kontrakcja m => Maybe User -> m ()
logUserToContext user =
    modifyContext $ set ctxmaybeuser user

logPadUserToContext :: Kontrakcja m => Maybe User -> m ()
logPadUserToContext user =
    modifyContext $ set ctxmaybepaduser user

unsafeSessionTakeover :: Kontrakcja m => SessionCookieInfo -> m (Maybe Session)
unsafeSessionTakeover SessionCookieInfo{..} = do
  domain <- currentDomain
  msession <- getSession cookieSessionID cookieSessionToken domain
  case msession of
   Nothing -> return Nothing
   Just s -> do
    mUser <- maybe (return Nothing) (dbQuery . GetUserByID) $ sesUserID s
    mPadUser <- maybe (return Nothing) (dbQuery . GetUserByID) $ sesPadUserID s
    modifyContext $ (\ctx -> set ctxsessionid (sesID s) $
                             set ctxmaybeuser mUser $
                             set ctxmaybepaduser mPadUser $ ctx)
    return $ Just s

switchLang :: Kontrakcja m => Lang -> m ()
switchLang lang =
     modifyContext $
     \ctx -> set ctxlang lang $
             set ctxtemplates
             (localizedVersion lang (get ctxglobaltemplates ctx))
             $ ctx

-- | Extract data from GET or POST request. Fail with 'internalError' if param
-- variable not present or when it cannot be read.
getDataFnM :: (HasRqData m, MonadBase IO m, MonadIO m, ServerMonad m) => RqData a -> m a
getDataFnM fun = either (const internalError) return =<< getDataFn fun

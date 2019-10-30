module Kontra
    ( module KontraError
    , module KontraMonad
    , module Context
    , KontraG(..)
    , Kontra
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
import FileStorage
import GuardTime (GuardTimeConfMonad(..))
import Happstack.Server.Instances ()
import Happstack.Server.ReqHandler
import KontraError
import KontraMonad
import MailContext (MailContextMonad(..))
import PdfToolsLambda.Conf
import Session.Cookies
import Session.Model
import Session.Types
import Templates
import User.Model
import Utils.HTTP

type InnerKontra fst = DBT (fst (CryptoRNGT (LogT (ReqHandlerT IO))))

-- | Kontra is a traditional Happstack handler monad except that it's
-- not a WebMonad.
--
-- Note also that in Kontra we don't do backtracking, which is why it
-- is not an instance of MonadPlus.  Errors are signaled explicitly
-- through 'KontraError'.
newtype KontraG fst a =
  Kontra { unKontra :: S.StateT Context (InnerKontra fst) a }

deriving instance Monad        (InnerKontra fst) => Applicative  (KontraG fst)
deriving instance CryptoRNG    (InnerKontra fst) => CryptoRNG    (KontraG fst)
deriving instance Functor      (InnerKontra fst) => Functor      (KontraG fst)
deriving instance Monad        (InnerKontra fst) => Monad        (KontraG fst)
deriving instance MonadBase IO (InnerKontra fst) => MonadBase IO (KontraG fst)
deriving instance MonadCatch   (InnerKontra fst) => MonadCatch   (KontraG fst)
deriving instance MonadDB      (InnerKontra fst) => MonadDB      (KontraG fst)
deriving instance MonadIO      (InnerKontra fst) => MonadIO      (KontraG fst)
deriving instance MonadMask    (InnerKontra fst) => MonadMask    (KontraG fst)
deriving instance MonadThrow   (InnerKontra fst) => MonadThrow   (KontraG fst)
deriving instance ServerMonad  (InnerKontra fst) => ServerMonad  (KontraG fst)
deriving instance (MonadLog (InnerKontra fst), MonadTime (KontraG fst))
  => MonadLog (KontraG fst)
deriving instance (HasRqData (InnerKontra fst), Monad (InnerKontra fst))
  => HasRqData (KontraG fst)
deriving instance FilterMonad Response (InnerKontra fst)
  => FilterMonad Response (KontraG fst)
deriving instance MonadFileStorage (InnerKontra fst)
  => MonadFileStorage (KontraG fst)

type Kontra = KontraG FileStorageT

runKontraG :: Monad (InnerKontra fst) => Context -> KontraG fst a -> InnerKontra fst a
runKontraG ctx f = S.evalStateT (unKontra f) ctx

runKontra :: Context -> Kontra a -> InnerKontra FileStorageT a
runKontra = runKontraG

instance MonadBaseControl IO (InnerKontra fst)
    => MonadBaseControl IO (KontraG fst) where
  type StM (KontraG fst) a = StM (S.StateT Context (InnerKontra fst)) a
  liftBaseWith f = Kontra $ liftBaseWith $ \run -> f $ run . unKontra
  restoreM = Kontra . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance (MonadTrans fst, Monad (InnerKontra fst), KontraMonad (KontraG fst))
    => MonadTime (KontraG fst) where
  currentTime = view #ctxTime <$> getContext

instance (MonadTrans fst, Monad (InnerKontra fst))
    => KontraMonad (KontraG fst) where
  getContext    = Kontra S.get
  modifyContext = Kontra . S.modify

instance (MonadTrans fst, Monad (InnerKontra fst))
    => TemplatesMonad (KontraG fst) where
  getTemplates = view #ctxTemplates <$> getContext
  getTextTemplatesByLanguage langStr = do
    globaltemplates <- view #ctxGlobalTemplates <$> getContext
    return $ TL.localizedVersion langStr globaltemplates

instance (MonadTrans fst, Monad (InnerKontra fst))
    => GuardTimeConfMonad (KontraG fst) where
  getGuardTimeConf = view #ctxGtConf <$> getContext

instance (MonadTrans fst, Monad (InnerKontra fst))
    => PdfToolsLambdaMonad (KontraG fst) where
  getPdfToolsLambdaEnv = view #ctxPdfToolsLambdaEnv <$> getContext

instance (MonadTrans fst, Monad (InnerKontra fst))
    => MailContextMonad (KontraG fst) where
  getMailContext = contextToMailContext <$> getContext

-- | Logged in user is admin with 2FA (2FA only enforced for production = true)
isAdmin :: Context -> Bool
isAdmin ctx = case ctx ^. #ctxMaybeUser of
  Nothing -> False
  Just user ->
    (useremail (userinfo user) `elem` ctx ^. #ctxAdminAccounts)
      && (usertotpactive user || not (ctx ^. #ctxProduction))


-- | Logged in user is sales with 2FA (2FA only enforced for production = true)
isSales :: Context -> Bool
isSales ctx = case ctx ^. #ctxMaybeUser of
  Nothing -> False
  Just user ->
    (useremail (userinfo user) `elem` ctx ^. #ctxSalesAccounts)
      && (usertotpactive user || not (ctx ^. #ctxProduction))

-- | Will 404 if not logged in as an admin.
onlyAdmin :: Kontrakcja m => m a -> m a
onlyAdmin m = do
  admin <- isAdmin <$> getContext
  if admin then m else respond404

-- | Will 404 if not logged in as a sales admin.
onlySalesOrAdmin :: Kontrakcja m => m a -> m a
onlySalesOrAdmin m = do
  admin <- (isAdmin || isSales) <$> getContext
  if admin then m else respond404

-- | Will 404 if the testing backdoor isn't open.
onlyBackdoorOpen :: Kontrakcja m => m a -> m a
onlyBackdoorOpen a = do
  backdoorOpen <- view #ctxIsMailBackdoorOpen <$> getContext
  if backdoorOpen then a else respond404

-- | Sticks the logged in user onto the context.
logUserToContext :: Kontrakcja m => Maybe User -> m ()
logUserToContext user = modifyContext $ set #ctxMaybeUser user

logPadUserToContext :: Kontrakcja m => Maybe User -> m ()
logPadUserToContext user = modifyContext $ set #ctxMaybePadUser user

unsafeSessionTakeover :: Kontrakcja m => SessionCookieInfo -> m (Maybe Session)
unsafeSessionTakeover SessionCookieInfo {..} = do
  domain   <- currentDomain
  msession <- getSession cookieSessionID cookieSessionToken domain
  case msession of
    Nothing -> return Nothing
    Just s  -> do
      mUser    <- maybe (return Nothing) (dbQuery . GetUserByID) $ sesUserID s
      mPadUser <- maybe (return Nothing) (dbQuery . GetUserByID) $ sesPadUserID s
      modifyContext $ \ctx ->
        ctx
          & (#ctxSessionID .~ sesID s)
          & (#ctxMaybeUser .~ mUser)
          & (#ctxMaybePadUser .~ mPadUser)
      return $ Just s

switchLang :: Kontrakcja m => Lang -> m ()
switchLang lang = modifyContext $ \ctx ->
  ctx
    & (#ctxLang .~ lang)
    & (#ctxTemplates .~ localizedVersion lang (ctx ^. #ctxGlobalTemplates))

-- | Extract data from GET or POST request. Fail with 'internalError'
-- if param variable not present or when it cannot be read.
getDataFnM :: (HasRqData m, MonadBase IO m, MonadIO m, ServerMonad m) => RqData a -> m a
getDataFnM fun = either (const internalError) return =<< getDataFn fun

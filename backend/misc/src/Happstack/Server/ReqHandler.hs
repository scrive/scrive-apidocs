module Happstack.Server.ReqHandler (
    withDecodedBody
  , withRqData
  , RqDataError(..)
  , ReqHandlerSt(..)
  , ReqHandlerT(..)
  , runReqHandlerT
  , runReqHandlerT'
  , mapReqHandlerT
  , PlusSandboxT(..)
  , runPlusSandboxT
  , mapPlusSandboxT
  , WebSandboxT(..)
  , runWebSandboxT
  , mapWebSandboxT
  , handleRequest
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Time
import Control.Monad.Trans.Control
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.Time
import Data.Typeable
import Happstack.Server
import Happstack.Server.Internal.MessageWrap
import Happstack.Server.Internal.Monads
import Network.Socket
import qualified Happstack.Server.Internal.Listen as L

import Happstack.Server.Instances ()

----------------------------------------

-- | Sane variant of 'decodeBody' that doesn't require 'WebMonad'.
withDecodedBody
  :: (FilterMonad Response m, ServerMonad m, MonadIO m)
  => BodyPolicy
  -> m Response
  -> m Response
withDecodedBody bp action = do
  rq      <- askRq
  (_, me) <- bodyInput bp rq
  case me of
    Just e  -> requestEntityTooLarge (toResponse e)
    Nothing -> action

-- | Sane variant of 'withDataFn' that doesn't require 'MonadPlus'.
withRqData :: (HasRqData m, MonadThrow m, ServerMonad m) => RqData a -> (a -> m r) -> m r
withRqData fn action = either (rqDataError . Errors) action =<< getDataFn fn

----------------------------------------

newtype RqDataError = RqDataError (Errors String)
  deriving (Eq, Ord, Show, Typeable)
instance Exception RqDataError

----------------------------------------

data ReqHandlerSt = ReqHandlerSt {
  hsRequest :: !Request
, hsFilter  :: !(Response -> Response)
, hsTime    :: !UTCTime
}

type InnerReqHandlerT = StateT ReqHandlerSt

-- | Replacement for 'ServerPerT' with 'MonadMask'
-- instance and no 'MonadPlus' and 'MonadWeb' instances.
newtype ReqHandlerT m a = ReqHandlerT { unReqHandlerT :: InnerReqHandlerT m a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadBase b, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadTrans)

runReqHandlerT :: Socket -> Conf -> ReqHandlerT IO Response -> IO ()
runReqHandlerT sock conf handler =
  L.listen' sock conf $ \req -> handleRequest req handler

handleRequest :: (MonadIO m) => Request -> ReqHandlerT m Response -> m Response
handleRequest r m = do
  -- TODO perhaps use MonadTime
  now       <- liftIO getCurrentTime
  (res, st) <- runReqHandlerT' now r m
  liftIO . runValidator pure $ hsFilter st res

runReqHandlerT'
  :: (MonadIO m) => UTCTime -> Request -> ReqHandlerT m a -> m (a, ReqHandlerSt)
runReqHandlerT' now req (ReqHandlerT action) = do
  runStateT action $ ReqHandlerSt req identity now

mapReqHandlerT
  :: (m (a, ReqHandlerSt) -> n (b, ReqHandlerSt)) -> ReqHandlerT m a -> ReqHandlerT n b
mapReqHandlerT f = ReqHandlerT . mapStateT f . unReqHandlerT

instance MonadTransControl ReqHandlerT where
  type StT ReqHandlerT a = StT InnerReqHandlerT a
  liftWith = defaultLiftWith ReqHandlerT unReqHandlerT
  restoreT = defaultRestoreT ReqHandlerT
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (ReqHandlerT m) where
  type StM (ReqHandlerT m) a = ComposeSt ReqHandlerT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance Monad m => MonadTime (ReqHandlerT m) where
  currentTime = ReqHandlerT $ gets hsTime

instance Monad m => FilterMonad Response (ReqHandlerT m) where
  setFilter f = ReqHandlerT . modify $ \st -> st { hsFilter = f }
  composeFilter f = ReqHandlerT . modify $ \st -> st { hsFilter = f . hsFilter st }
  getFilter m = ReqHandlerT . StateT $ \st -> do
    (res, st') <- runStateT (unReqHandlerT m) st
    -- Make Response filters local to the passed computation.
    return ((res, hsFilter st'), st' { hsFilter = hsFilter st })

instance (MonadIO m, MonadThrow m) => HasRqData (ReqHandlerT m) where
  askRqEnv    = smAskRqEnv
  rqDataError = throwM . RqDataError
  localRqEnv  = smLocalRqEnv

instance Monad m => ServerMonad (ReqHandlerT m) where
  askRq = ReqHandlerT $ gets hsRequest
  localRq f m = ReqHandlerT . StateT $ \st -> do
    let req = hsRequest st
    (res, st') <- runStateT (unReqHandlerT m) $ st { hsRequest = f req }
    return (res, st' { hsRequest = req })

----------------------------------------

-- | Sandbox for happstack functions using 'MonadPlus'.
-- Note that this is NOT a valid instance of 'MonadMask'.
newtype PlusSandboxT m a = PlusSandboxT { unPlusSandboxT :: MaybeT m a }
  deriving ( Applicative, Functor, HasRqData, Monad, MonadBase b
           , MonadCatch, MonadIO, MonadThrow, MonadTrans
           , FilterMonad r, ServerMonad)

runPlusSandboxT :: PlusSandboxT m a -> m (Maybe a)
runPlusSandboxT = runMaybeT . unPlusSandboxT

mapPlusSandboxT :: (m (Maybe a) -> n (Maybe b)) -> PlusSandboxT m a -> PlusSandboxT n b
mapPlusSandboxT f = PlusSandboxT . mapMaybeT f . unPlusSandboxT

instance (Functor m, Monad m) => Alternative (PlusSandboxT m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadPlus (PlusSandboxT m) where
  mzero = PlusSandboxT mzero
  PlusSandboxT m1 `mplus` PlusSandboxT m2 = PlusSandboxT $ m1 `mplus` m2

----------------------------------------

-- | Sandbox for happstack functions using 'WebMonad'.
-- Note that this is NOT a valid instance of 'MonadMask'.
--
-- In addition, 'MonadPlus' is not derived because
-- its instance for ErrorT throws away information.
newtype WebSandboxT m a = WebSandboxT { unWebSandboxT :: ExceptT Response m a }
  deriving ( Applicative, Functor, HasRqData, Monad, MonadBase b
           , MonadCatch, MonadIO, MonadThrow, MonadTrans
           , FilterMonad r, ServerMonad)

runWebSandboxT :: WebSandboxT m a -> m (Either Response a)
runWebSandboxT = runExceptT . unWebSandboxT

mapWebSandboxT
  :: (m (Either Response a) -> n (Either Response b))
  -> WebSandboxT m a
  -> WebSandboxT n b
mapWebSandboxT f = WebSandboxT . mapExceptT f . unWebSandboxT

instance Monad m => WebMonad Response (WebSandboxT m) where
  finishWith = WebSandboxT . throwError

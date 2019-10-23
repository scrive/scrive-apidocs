module Mailer (
    Mailer
  , runMailer
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Crypto.RNG
import Happstack.Server
import Log

import Control.Monad.Trans.Instances ()
import DB
import Happstack.Server.Instances ()
import Happstack.Server.ReqHandler

type InnerMailer = CryptoRNGT (DBT (LogT (ReqHandlerT IO)))

newtype Mailer a = Mailer { unMailer :: InnerMailer a }
  deriving (Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadBase IO, MonadCatch, MonadDB, MonadIO, MonadMask, MonadThrow, MonadTime, ServerMonad, MonadLog)

instance MonadBaseControl IO Mailer where
  type StM Mailer a = StM InnerMailer a
  liftBaseWith f = Mailer $ liftBaseWith $ \run -> f $ run . unMailer
  restoreM = Mailer . restoreM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runMailer :: CryptoRNGState -> Mailer a -> DBT (LogT (ReqHandlerT IO)) a
runMailer rng = runCryptoRNGT rng . unMailer

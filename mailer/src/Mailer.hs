module Mailer (
    Mailer
  , runMailer
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Database.PostgreSQL.PQTypes.Class.Instances.Overlapping ()
import Happstack.Server

import Control.Monad.Trans.Control.Util
import Control.Monad.Trans.Instances ()
import Crypto.RNG
import DB
import Happstack.Server.Instances ()
import Happstack.Server.ReqHandler
import KontraPrelude
import Log

type InnerMailer = CryptoRNGT (DBT (ReqHandlerT (LogT IO)))

newtype Mailer a = Mailer { unMailer :: InnerMailer a }
  deriving (Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadBase IO, MonadCatch, MonadDB, MonadIO, MonadMask, MonadThrow, MonadTime, ServerMonad, MonadLog)

instance MonadBaseControl IO Mailer where
  newtype StM Mailer a = StMailer { unStMailer :: StM InnerMailer a }
  liftBaseWith = newtypeLiftBaseWith Mailer unMailer StMailer
  restoreM     = newtypeRestoreM Mailer unStMailer
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runMailer :: CryptoRNGState -> Mailer a -> DBT (ReqHandlerT (LogT IO)) a
runMailer rng = runCryptoRNGT rng . unMailer

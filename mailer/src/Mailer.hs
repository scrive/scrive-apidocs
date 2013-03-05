module Mailer (
    Mailer
  , runMailer
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Happstack.Server

import Control.Monad.Trans.Control.Util
import Crypto.RNG
import DB.Core
import qualified Log
import OurServerPart ()

type InnerMailer = CryptoRNGT (DBT (ServerPartT IO))

newtype Mailer a = Mailer { unMailer :: InnerMailer a }
  deriving (Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadBase IO, MonadDB, MonadIO, MonadPlus, ServerMonad, Log.MonadLog)

instance MonadBaseControl IO Mailer where
  newtype StM Mailer a = StMailer { unStMailer :: StM InnerMailer a }
  liftBaseWith = newtypeLiftBaseWith Mailer unMailer StMailer
  restoreM     = newtypeRestoreM Mailer unStMailer
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

runMailer :: CryptoRNGState -> Mailer a -> DBT (ServerPartT IO) a
runMailer rng = runCryptoRNGT rng . unMailer

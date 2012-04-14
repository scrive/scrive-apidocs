module Mailer (
    Mailer
  , runMailer
  ) where

import Control.Applicative
import Control.Monad.Reader
import Happstack.Server

import Crypto.RNG
import DB.Core

newtype Mailer a = Mailer { unMailer :: CryptoRNGT (DBT (ServerPartT IO)) a }
  deriving (Applicative, CryptoRNG, FilterMonad Response, Functor, HasRqData, Monad, MonadDB, MonadIO, MonadPlus, ServerMonad, WebMonad Response)

runMailer :: CryptoRNGState -> Mailer a -> DBT (ServerPartT IO) a
runMailer rng = runCryptoRNGT rng . unMailer

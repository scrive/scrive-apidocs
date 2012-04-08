module Mailer (
    Mailer
  , runMailer
  ) where

import Control.Applicative
import Control.Monad.Reader
import Happstack.Server

import Crypto.RNG (CryptoRNG, getCryptoRNGState)
import DB.Classes

newtype Mailer a = Mailer { unMailer :: ServerPartT (ReaderT DBEnv IO) a }
  deriving (Applicative, FilterMonad Response, Functor, HasRqData, Monad, MonadIO, MonadPlus, ServerMonad, WebMonad Response)

instance CryptoRNG Mailer where
  getCryptoRNGState = Mailer $ asks envRNG

instance MonadDB Mailer where
  getDBEnv = Mailer ask

runMailer :: DBEnv -> Mailer a -> ServerPartT IO a
runMailer env = mapServerPartT (\r -> runReaderT r env) . unMailer

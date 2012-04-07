module Mailer (
    Mailer
  , runMailer
  ) where

import Control.Applicative
import Control.Monad.Reader
import Happstack.Server

import Crypto.RNG (CryptoRNG, getCryptoRNGState)
import DB.Classes
import qualified Log (mailingServer)

newtype Mailer a = Mailer { unMailer :: ServerPartT (ReaderT DBEnv IO) a }
  deriving (Applicative, FilterMonad Response, Functor, HasRqData, Monad, MonadIO, MonadPlus, ServerMonad, WebMonad Response, MonadReader DBEnv)

instance CryptoRNG Mailer where
  getCryptoRNGState = asks envRNG

instance DBMonad Mailer where
  getDBEnv = Mailer ask
  handleDBError e = do
    Log.mailingServer $ "SQL error: " ++ show e
    finishWith =<< internalServerError (toResponse "Internal server error")

runMailer :: DBEnv -> Mailer a -> ServerPartT IO a
runMailer env = mapServerPartT (\r -> runReaderT r env) . unMailer

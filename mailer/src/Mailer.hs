module Mailer (
    Mailer
  , runMailer
  ) where

import Control.Applicative
import Control.Monad.Reader
import DB.Nexus
import Happstack.Server

import Crypto.RNG (CryptoRNG, getCryptoRNGState)
import DB.Classes
import qualified Log (mailingServer)

newtype Mailer a = Mailer { unMailer :: ServerPartT (ReaderT Nexus IO) a }
  deriving (Applicative, FilterMonad Response, Functor, HasRqData, Monad, MonadIO, MonadPlus, ServerMonad, WebMonad Response, MonadReader Nexus)

instance CryptoRNG Mailer where
  getCryptoRNGState = asks nexusRNG

instance DBMonad Mailer where
  getConnection = Mailer ask
  handleDBError e = do
    Log.mailingServer $ "SQL error: " ++ show e
    finishWith =<< internalServerError (toResponse "Internal server error")

runMailer :: Nexus -> Mailer a -> ServerPartT IO a
runMailer conn = mapServerPartT (\r -> runReaderT r conn) . unMailer

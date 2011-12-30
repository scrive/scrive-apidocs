module Mailer (
    Mailer
  , runMailer
  ) where

import Control.Applicative
import Control.Monad.Reader
import Database.HDBC.PostgreSQL
import Happstack.Server

import DB.Classes
import qualified AppLogger as Log (mailingServer)

newtype Mailer a = Mailer { unMailer :: ServerPartT (ReaderT Connection IO) a }
  deriving (Applicative, FilterMonad Response, Functor, HasRqData, Monad, MonadIO, MonadPlus, ServerMonad, WebMonad Response)

instance DBMonad Mailer where
  getConnection = Mailer ask
  handleDBError e = do
    Log.mailingServer $ "SQL error: " ++ show e
    finishWith =<< internalServerError (toResponse "Internal server error")

runMailer :: Connection -> Mailer a -> ServerPartT IO a
runMailer conn = mapServerPartT (\r -> runReaderT r conn) . unMailer

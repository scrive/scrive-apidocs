module ForkAction (forkAction) where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Time
import Database.PostgreSQL.PQTypes
import qualified Control.Concurrent.Thread as T

import Context
import KontraMonad
import KontraPrelude
import qualified Log

forkAction :: forall m. (MonadDB m, MonadBaseControl IO m, Log.MonadLog m, KontraMonad m)
           => String -> m () -> m ()
forkAction title action = do
  (_, mjoin) <- liftBaseDiscard T.forkIO . withNewConnection $ mask $ \release -> do
    Log.mixlog_ $ "forkAction: " ++ title ++ " started"
    start <- liftBase getCurrentTime
    result <- try $ release action
    end <- liftBase getCurrentTime
    let duration = diffUTCTime end start
    case result of
      Left (e::SomeException) -> do
        rollback
        Log.mixlog_ $ "forkAction:" <+> title <+> "finished in" <+> show duration <+> "with exception" <+> show e
        -- rethrow, so it can be propagated to the parent thread
        throwIO e
      Right _ -> do
        commit
        Log.mixlog_ $ "forkAction:" <+> title <+> "finished in" <+> show duration
  modifyContext $ \ctx -> ctx { ctxthreadjoins = mjoin : ctxthreadjoins ctx }

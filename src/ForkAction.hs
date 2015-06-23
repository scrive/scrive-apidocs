module ForkAction (forkAction) where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Time
import Database.PostgreSQL.PQTypes
import Log
import qualified Control.Concurrent.Thread as T

import Context
import Crypto.RNG
import KontraMonad
import KontraPrelude
import Log.Utils

forkAction :: forall m. (CryptoRNG m, MonadDB m, MonadBaseControl IO m, MonadLog m, KontraMonad m)
           => String -> m () -> m ()
forkAction title action = do
  (_, mjoin) <- liftBaseDiscard T.forkIO . withNewConnection $ mask $ \release -> localRandomID "action_id" . localData ["action_name" .= title] $ do
    logInfo_ "forkAction started"
    start <- liftBase getCurrentTime
    result <- try $ release action
    end <- liftBase getCurrentTime
    let duration = diffUTCTime end start
    case result of
      Left (e::SomeException) -> do
        rollback
        logInfo "forkAction finished" $ object [
            "time" .= toDouble duration
          , "error" .= show e
          ]
        -- rethrow, so it can be propagated to the parent thread
        throwIO e
      Right _ -> do
        commit
        logInfo "forkAction finished" $ object [
            "time" .= toDouble duration
          ]
  modifyContext $ \ctx -> ctx { ctxthreadjoins = mjoin : ctxthreadjoins ctx }
  where
    toDouble :: NominalDiffTime -> Double
    toDouble = realToFrac

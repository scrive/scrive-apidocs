module Doc.Flow
    ( processEventThrow
    , EngineEvent(..)
    , UserAction(..)
    )
  where

import Control.Monad.Catch
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG
import Database.PostgreSQL.PQTypes (MonadDB)
import Log (MonadLog)
import Text.StringTemplates.Templates (TemplatesMonad)

import API.V2.Errors
import API.V2.MonadUtils (apiError)
import Doc.DocumentMonad (DocumentMonad)
import EventStream.Class (MonadEventStream)
import File.Storage (MonadFileStorage)
import Flow.Engine (EngineError(..), EngineEvent(..), processEvent)
import Flow.Machinize (UserAction(..))
import GuardTime (GuardTimeConfMonad)
import MailContext (MailContextMonad)

processEventThrow
  :: ( CryptoRNG m
     , DocumentMonad m
     , GuardTimeConfMonad m
     , MailContextMonad m
     , MonadBaseControl IO m
     , MonadDB m
     , MonadEventStream m
     , MonadFileStorage m
     , MonadIO m
     , MonadLog m
     , MonadMask m
     , MonadThrow m
     , TemplatesMonad m
     )
  => EngineEvent
  -> m ()
processEventThrow event = runExceptT (processEvent event) >>= \case
  Right _                    -> pure ()
  Left  NoAssociatedDocument -> serverError' "No associated document."
  Left  NoAssociatedUser     -> serverError' "No associated user."
  Left UnexpectedEventReceived -> conflict' "Unxpected event received."
  Left AggregatorSteppingFailed -> serverError' "Aggregator stepping failed."
  Left DuplicateEventReceived -> conflict' "Duplicate event received."
  Left  NoInstance           -> serverError' "No instance."
  Left  (InvalidProcess _)   -> serverError' "Invalid process."
  where
    serverError' = apiError . serverError
    conflict'    = apiError . conflictError

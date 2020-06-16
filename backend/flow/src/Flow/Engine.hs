{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Flow.Engine (processMachinizeEvent, finalizeFlow, processEvent) where

import Control.Monad.Catch
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Crypto.RNG
import Data.Aeson.Types
import Database.PostgreSQL.PQTypes (MonadDB)
import GHC.Generics
import Log (MonadLog, localData, logAttention_, logTrace_)
import Text.StringTemplates.Templates (TemplatesMonad)

import Doc.DocumentID (DocumentID)
import Doc.DocumentMonad
import Doc.SignatoryLinkID (SignatoryLinkID, fromSignatoryLinkID)
import EventStream.Class
import Flow.Aggregator
  ( AggregatorError(DuplicateEvent, TransducerError, UnknownEventInfo)
  , AggregatorStep(FinalState, NeedMoreEvents, StateChange), runAggregatorStep
  )
import File.Storage (MonadFileStorage)
import Flow.ActionConsumers (toConsumableAction, consumeFlowAction) -- in backend/lib/Flow
import Flow.Id (InstanceId)
import Flow.Machinize (Deed(..), EventInfo(..), LowAction)
import Flow.Model
  ( selectAggregatorData, selectDocumentNameFromKV, selectUserNameFromKV
  , updateAggregatorState
  )
import GuardTime (GuardTimeConfMonad)
import MailContext
import qualified Flow.Transducer as Transducer (Error)

data EngineEvent = EngineEvent
    { instanceId :: InstanceId
    , deed :: Deed
    , signatoryId :: SignatoryLinkID
    , documentId :: DocumentID
    }
  deriving (Show, Eq, Generic)

toPairs :: EngineEvent -> [Pair]
toPairs EngineEvent {..} =
  [ "instance_id" .= instanceId
  , "deed" .= deed
  , "signatory_id" .= show (fromSignatoryLinkID signatoryId)
  , "document_id" .= documentId
  ]

data EngineError
    = NoAssociatedDocument
    | NoAssociatedUser
    | UnexpectedEventReceived
    | TransducerSteppingFailed Transducer.Error
    | DuplicateEventReceived
    | NotImplemented Text
  deriving (Show, Exception)

processEvent
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
processEvent event@EngineEvent {..} = do
  localData (toPairs event) $ do
    documentName <- fromMaybeM noDocumentNameFound
      $ selectDocumentNameFromKV instanceId documentId
    userName <- fromMaybeM noUserNameFound $ selectUserNameFromKV instanceId signatoryId
    processMachinizeEvent
      instanceId
      EventInfo { eventInfoDeed     = deed
                , eventInfoUser     = userName
                , eventInfoDocument = documentName
                }
  where
    noDocumentNameFound = do
      logAttention_ "Given document not associated with flow instance."
      throwM NoAssociatedDocument
    noUserNameFound = do
      logAttention_ "Signatory not associated with flow intance."
      throwM NoAssociatedUser

pushActions
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
  => InstanceId
  -> [LowAction]
  -> m ()
pushActions instanceId actions = do
  consumableActions <- mapM (toConsumableAction instanceId) actions
  mapM_ consumeFlowAction consumableActions

finalizeFlow :: (MonadLog m, MonadDB m, MonadThrow m) => m ()
finalizeFlow = throwM $ NotImplemented "Finalizing flow"

processMachinizeEvent
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
  => InstanceId
  -> EventInfo
  -> m ()
processMachinizeEvent instanceId eventInfo = do
  (machine, aggregator) <- selectAggregatorData instanceId
  let (aggregatorResult, newState) = runAggregatorStep eventInfo aggregator machine
  either failGracefully processStep aggregatorResult
  updateAggregatorState instanceId newState
  where
    -- TODO: Think of some error monad. MonadFail is ugly.
    -- TODO: This fail thing may leak some error to user.
    -- TODO: Improve error messages.
    failGracefully :: (MonadLog m, MonadDB m, MonadThrow m) => AggregatorError -> m ()
    failGracefully UnknownEventInfo = do
      logAttention_ "Unexpected event received."
      throwM UnexpectedEventReceived
    failGracefully DuplicateEvent = do
      logAttention_ "Duplicate event received."
      throwM DuplicateEventReceived
    failGracefully (TransducerError err) = do
      logAttention_ $ "Transducer is in inconsistent state: " <> showt err
      throwM $ TransducerSteppingFailed err

    processStep NeedMoreEvents        = logTrace_ "Aggregator needs more events to step."
    processStep (StateChange actions) = pushActions instanceId actions
    processStep (FinalState  actions) = do
      pushActions instanceId actions
      finalizeFlow


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Flow.Engine (processMachinizeEvent, finalizeFlow, processEvent) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Extra (fromMaybeM)
import Data.Aeson.Types
import Database.PostgreSQL.PQTypes (MonadDB)
import GHC.Generics
import Log (MonadLog, localData, logAttention_, logTrace_)

import Doc.DocumentID (DocumentID)
import Doc.SignatoryLinkID (SignatoryLinkID, fromSignatoryLinkID)
import Flow.Aggregator
  ( AggregatorError(DuplicateEvent, TransducerError, UnknownEventInfo)
  , AggregatorStep(FinalState, NeedMoreEvents, StateChange), runAggregatorStep
  )
import Flow.Id (InstanceId)
import Flow.Machinize (Deed(..), EventInfo(..), LowAction)
import Flow.Model
  ( selectAggregatorData, selectDocumentNameFromKV, selectUserNameFromKV
  , updateAggregatorState
  )
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


-- TODO: Take engine event dereference all ides and call processMachinizeEvent.
processEvent :: (MonadLog m, MonadDB m, MonadThrow m) => EngineEvent -> m ()
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

pushActions :: (MonadLog m, MonadDB m, MonadThrow m) => [LowAction] -> m ()
pushActions _ = throwM $ NotImplemented "Push action"

finalizeFlow :: (MonadLog m, MonadDB m, MonadThrow m) => m ()
finalizeFlow = throwM $ NotImplemented "Finalizing flow"

processMachinizeEvent
  :: (MonadLog m, MonadDB m, MonadThrow m) => InstanceId -> EventInfo -> m ()
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

    processStep :: (MonadLog m, MonadDB m, MonadThrow m) => AggregatorStep -> m ()
    processStep NeedMoreEvents        = logTrace_ "Aggregator needs more events to step."
    processStep (StateChange actions) = pushActions actions
    processStep (FinalState  actions) = do
      pushActions actions
      finalizeFlow

